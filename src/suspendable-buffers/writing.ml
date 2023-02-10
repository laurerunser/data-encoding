(* TODO: expect tests *)

type destination =
  { buffer : bytes
  ; offset : int
  ; length : int
  ; written : int
  ; maximum_length : int
  }

let mk_destination ?(maximum_length = max_int) buffer offset length =
  if offset < 0
  then failwith "Suspendable_buffers.Writing.mk_destination: negative offset";
  if length < 0
  then failwith "Suspendable_buffers.Writing.mk_destination: negative length";
  if offset + length > Bytes.length buffer
  then failwith "Suspendable_buffers.Writing.mk_destination: offset+length overflow";
  { buffer; offset; length; written = 0; maximum_length }
;;

let slice_of_destination { buffer; offset; length = _; written; maximum_length = _ } =
  buffer, offset, written
;;

let set_maximum_length destination maximum_length =
  if maximum_length < 0
  then
    raise
      (Invalid_argument "Suspendable_buffers.Writing.set_maximum_length: negative length");
  if maximum_length > destination.maximum_length
  then
    raise
      (Invalid_argument
         "Suspendable_buffers.Writing.set_maximum_length: cannot increase maximum length");
  { destination with maximum_length }
;;

let bump_written destination writing =
  { destination with written = destination.written + writing }
;;

type written =
  | Written of { destination : destination }
  | Failed of
      { destination : destination
      ; error : string
      }
  | Suspended of
      { destination : destination
      ; cont : bytes -> int -> int -> written
      }

let destination_too_small_to_continue_message =
  "new destination buffer is too small to continue"
;;

let writef destination writing write =
  assert (writing >= 0);
  if destination.written + writing > destination.maximum_length
  then Failed { destination; error = "maximum-length exceeded" }
  else if destination.written + writing > destination.length
  then
    Suspended
      { destination
      ; cont =
          (fun buffer offset length ->
            let destination =
              mk_destination
                ~maximum_length:(destination.maximum_length - destination.written)
                buffer
                offset
                length
            in
            if destination.offset + writing > destination.length
            then
              (* TODO: instead of failing here, allow to continue after more
                 buffering, possibly go a slow path where the value is written
                 to an internal buffer which is blitted bit by bit on the small
                 buffers that are passed by the user *)
              Failed { destination; error = destination_too_small_to_continue_message }
            else (
              write destination.buffer (destination.offset + destination.written);
              let destination = bump_written destination writing in
              Written { destination }))
      }
  else (
    write destination.buffer (destination.offset + destination.written);
    let destination = bump_written destination writing in
    Written { destination })
;;

let write_small_string destination s =
  let writing = String.length s in
  writef destination writing (fun b o -> Bytes.blit_string s 0 b o writing)
;;

let write_char destination c = writef destination 1 (fun b o -> Bytes.set b o c)

type chunkwriter = bytes -> int -> int -> chunkwritten

and chunkwritten =
  | K of int * chunkwriter
  | Finish of int

(* similar to [writef] but writes the data in chunks, this allows to write data
   which is larger than the current buffer can accomodate, this is intended for
   writing string and other such potentially big blobs. *)
let rec writechunked destination write =
  let writing =
    min
      (destination.maximum_length - destination.written)
      (destination.length - destination.written)
  in
  assert (writing >= 0);
  match write destination.buffer (destination.offset + destination.written) writing with
  | Finish written ->
    let destination = bump_written destination written in
    Written { destination }
  | K (written, write) ->
    if written > writing
    then
      raise
        (Failure "Suspendable_buffers.Writing.writechunked: chunkwriter exceeded limit");
    let destination = bump_written destination written in
    Suspended
      { destination
      ; cont =
          (fun buffer offset length ->
            let destination =
              mk_destination
                ~maximum_length:(destination.maximum_length - destination.written)
                buffer
                offset
                length
            in
            writechunked destination write)
      }
;;

let write_large_string destination s =
  let size = String.length s in
  let rec chunkwriter source_offset buffer offset maxwritesize =
    let needswriting = size - source_offset in
    if needswriting = 0
    then Finish 0
    else if needswriting <= maxwritesize
    then (
      Bytes.blit_string s source_offset buffer offset needswriting;
      Finish needswriting)
    else (
      Bytes.blit_string s source_offset buffer offset maxwritesize;
      K (maxwritesize, chunkwriter (source_offset + maxwritesize)))
  in
  writechunked destination (chunkwriter 0)
;;

let write_large_bytes destination s =
  let size = Bytes.length s in
  let rec chunkwriter source_offset buffer offset maxwritesize =
    let needswriting = size - source_offset in
    if needswriting = 0
    then Finish 0
    else if needswriting <= maxwritesize
    then (
      Bytes.blit s source_offset buffer offset needswriting;
      Finish needswriting)
    else (
      Bytes.blit s source_offset buffer offset maxwritesize;
      K (maxwritesize, chunkwriter (source_offset + maxwritesize)))
  in
  writechunked destination (chunkwriter 0)
;;

let rec ( let* ) x f =
  match x with
  | Written { destination } -> f destination
  | Failed _ -> x
  | Suspended { destination; cont } ->
    let cont buffer offset length =
      let* x = cont buffer offset length in
      f x
    in
    Suspended { destination; cont }
;;

let rec to_string_loop buffer acc k =
  match k buffer 0 (Bytes.length buffer) with
  | Written { destination } ->
    Ok (Bytes.sub_string destination.buffer 0 destination.written :: acc)
  | Failed { destination; error } when error = destination_too_small_to_continue_message
    ->
    assert (destination.written = 0);
    let buffer_size = 2 * (1 + Bytes.length buffer) in
    let buffer = Bytes.make buffer_size '\x00' in
    to_string_loop buffer acc k
  | Failed { destination = _; error } -> Error error
  | Suspended { destination; cont } ->
    if destination.written = 0
    then to_string_loop buffer acc cont
    else (
      let acc = Bytes.sub_string destination.buffer 0 destination.written :: acc in
      to_string_loop buffer acc cont)
;;

let to_string ?(buffer_size = 1024) writer =
  let buffer = Bytes.make buffer_size '\x00' in
  match
    to_string_loop buffer [] (fun buffer offset length ->
        let destination = mk_destination ~maximum_length:max_int buffer offset length in
        writer destination)
  with
  | Ok rev_chunks ->
    let chunks = List.rev rev_chunks in
    Ok (String.concat "" chunks)
  | Error _ as err -> err
;;

let rec blit_instructions_loop buffer k () =
  match k buffer 0 (Bytes.length buffer) with
  | Written { destination } -> Seq.Cons (slice_of_destination destination, Seq.empty)
  | Failed { destination = _; error } -> raise (Failure error)
  | Suspended { destination; cont } ->
    Seq.Cons (slice_of_destination destination, blit_instructions_loop buffer cont)
;;

let blit_instructions ~buffer writer =
  blit_instructions_loop buffer (fun buffer offset length ->
      let destination = mk_destination buffer offset length in
      writer destination)
;;
