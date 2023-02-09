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

let write1 destination writing write =
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

type chunkwriter = bytes -> int -> int -> chunkwritten

and chunkwritten =
  | K of int * chunkwriter
  | Finish of int

(* similar to [write1] but writes the data in chunks, this allows to write data
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
