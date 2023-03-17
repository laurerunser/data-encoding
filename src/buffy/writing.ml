(* TODO: expect tests *)

type state =
  { destination : Dst.t
  ; written : int
  ; maximum_length : int
  }

let mk_state ?(maximum_length = max_int) destination =
  { destination; written = 0; maximum_length }
;;

let set_maximum_length state maximum_length =
  if maximum_length < 0
  then
    raise
      (Invalid_argument "Suspendable_buffers.Writing.set_maximum_length: negative length");
  if maximum_length > state.maximum_length
  then
    raise
      (Invalid_argument
         "Suspendable_buffers.Writing.set_maximum_length: cannot increase maximum length");
  { state with maximum_length }
;;

let bump_written state writing = { state with written = state.written + writing }

type written =
  | Written of { state : state }
  | Failed of
      { state : state
      ; error : string
      }
  | Suspended of
      { state : state
      ; cont : Dst.t -> written
      }

let destination_too_small_to_continue_message =
  "new destination buffer is too small to continue"
;;

let writef state writing write =
  assert (writing >= 0);
  if state.written + writing > state.maximum_length
  then Failed { state; error = "maximum-length exceeded" }
  else if state.written + writing <= Dst.length state.destination
  then (
    write state.destination state.written;
    let state = bump_written state writing in
    Written { state })
  else (
    (* TODO? add an option which does some copying to a temporary buffer and
       uses the original destination to its maximum capacity. (Similar to
       Reading case.) *)
    let maximum_length = state.maximum_length - state.written in
    Suspended
      { state
      ; cont =
          (fun destination ->
            let state = mk_state ~maximum_length destination in
            if Dst.length destination < writing
            then Failed { state; error = destination_too_small_to_continue_message }
            else (
              write state.destination state.written;
              let state = bump_written state writing in
              Written { state }))
      })
;;

let%expect_test _ =
  (* TODO: this and other expect block rely on dst using bytes which may change *)
  let w n f =
    let len = 10 in
    let buffer = Bytes.make len '_' in
    let destination = Dst.of_bytes buffer ~offset:1 ~length:8 in
    let state = mk_state destination in
    match writef state n f with
    | Written { state } ->
      assert (state.destination == destination);
      Format.printf "Written: %s" (Bytes.unsafe_to_string buffer)
    | Failed { state; error } ->
      assert (state.destination == destination);
      Format.printf "Failed: %s (%s)" error (Bytes.unsafe_to_string buffer)
    | Suspended _ -> assert false
    (* no test for suspended in this block *)
  in
  w 0 (fun _ _ -> ());
  [%expect {| Written: __________ |}];
  w 1 (fun b o -> Dst.set_char b o 'X');
  [%expect {| Written: _X________ |}];
  w 2 (fun b o ->
    Dst.set_char b o '/';
    Dst.set_char b (o + 1) '\\');
  [%expect {| Written: _/\_______ |}];
  w 3 (fun b o -> Dst.set_string b o "oOo");
  [%expect {| Written: _oOo______ |}];
  ()
;;

let write_char state c = writef state 1 (fun b o -> Dst.set_char b o c)

let%expect_test _ =
  let w c =
    let len = 10 in
    let buffer = Bytes.make len '_' in
    let destination = Dst.of_bytes buffer ~offset:1 ~length:8 in
    let state = mk_state destination in
    match write_char state c with
    | Written { state } ->
      assert (state.destination == destination);
      Format.printf "Written: %s" (Bytes.unsafe_to_string buffer)
    | Failed { state; error } ->
      assert (state.destination == destination);
      Format.printf "Failed: %s (%s)" error (Bytes.unsafe_to_string buffer)
    | Suspended _ -> assert false
    (* no test for suspended in this block *)
  in
  w ' ';
  [%expect {| Written: _ ________ |}];
  w 'X';
  [%expect {| Written: _X________ |}];
  ()
;;

let write_utf8_uchar state c =
  let c = Uchar.to_int c in
  if c < 0b1000_0000
  then write_char state (Char.chr c)
  else if c < 0b1_00000_000000
  then (
    let c0 = 0b110_00000 lor ((c land 0b11111_000000) lsr 6) in
    let c1 = 0b10_000000 lor (c land 0b00000_111111) in
    writef state 2 (fun b o ->
      Dst.set_uint8 b o c0;
      Dst.set_uint8 b (o + 1) c1))
  else if c < 0b1_0000_000000_000000
  then (
    let c0 = 0b1110_0000 lor ((c land 0b1111_000000_000000) lsr 12) in
    let c1 = 0b10_000000 lor ((c land 0b0000_111111_000000) lsr 6) in
    let c2 = 0b10_000000 lor (c land 0b0000_000000_111111) in
    writef state 2 (fun b o ->
      Dst.set_uint8 b o c0;
      Dst.set_uint8 b (o + 1) c1;
      Dst.set_uint8 b (o + 2) c2))
  else if c < 0b1_000_000000_000000_000000
  then (
    let c0 = 0b11110_000 lor ((c land 0b111_000000_000000_000000) lsr 18) in
    let c1 = 0b10_000000 lor ((c land 0b000_111111_000000_000000) lsr 12) in
    let c2 = 0b10_000000 lor ((c land 0b000_000000_111111_000000) lsr 6) in
    let c3 = 0b10_000000 lor (c land 0b000_000000_000000_111111) in
    writef state 2 (fun b o ->
      Dst.set_uint8 b o c0;
      Dst.set_uint8 b (o + 1) c1;
      Dst.set_uint8 b (o + 2) c2;
      Dst.set_uint8 b (o + 3) c3))
  else (
    let error = "Invalid uchar" in
    Failed { state; error })
;;

let%expect_test _ =
  let w c =
    let len = 6 in
    let buffer = Bytes.make len '_' in
    let destination = Dst.of_bytes buffer ~offset:1 ~length:4 in
    let state = mk_state destination in
    match write_utf8_uchar state c with
    | Written { state } ->
      assert (state.destination == destination);
      Format.printf "Written: %s" (Bytes.unsafe_to_string buffer)
    | Failed { state; error } ->
      assert (state.destination == destination);
      Format.printf "Failed: %s (%s)" error (Bytes.unsafe_to_string buffer)
    | Suspended _ -> assert false
    (* no test for suspended in this block *)
  in
  w (Uchar.of_int 0x24);
  [%expect {| Written: _$____ |}];
  w (Uchar.of_int 0x00A3);
  [%expect {| Written: _£___ |}];
  w (Uchar.of_int 0x20AC);
  [%expect {| Written: _€__ |}];
  w (Uchar.of_int 0x10348);
  [%expect {| Written: _𐍈_ |}];
  ()
;;

type chunkwriter = Dst.t -> int -> int -> chunkwritten

and chunkwritten =
  | CWritten of { written : int }
  | CFailed of
      { written : int
      ; error : string
      }
  | CSuspended of
      { written : int
      ; cont : chunkwriter
      }

(* similar to [writef] but writes the data in chunks, this allows to write data
   which is larger than the current buffer can accomodate, this is intended for
   writing string and other such potentially big blobs. *)
let rec writechunked state write =
  let writing = min state.maximum_length (Dst.length state.destination) - state.written in
  assert (writing >= 0);
  match write state.destination state.written writing with
  | CWritten { written } ->
    let state = bump_written state written in
    Written { state }
  | CFailed { written; error } ->
    let state = bump_written state written in
    Failed { state; error }
  | CSuspended { written; cont } ->
    if written > writing
    then
      raise
        (Failure "Suspendable_buffers.Writing.writechunked: chunkwriter exceeded limit");
    let state = bump_written state written in
    let maximum_length = state.maximum_length - state.written in
    assert (maximum_length >= 0);
    if maximum_length = 0
    then
      Failed { state; error = "maximum-length reached but chunk-writer is not finished" }
    else
      Suspended
        { state
        ; cont =
            (fun destination ->
              let state = mk_state ~maximum_length destination in
              writechunked state cont)
        }
;;

let write_string state src =
  let size = String.length src in
  if size <= Dst.length state.destination - state.written
  then writef state size (fun d o -> Dst.set_string d o src)
  else (
    let rec chunkwriter source_offset destination offset maxwritesize =
      let needswriting = size - source_offset in
      assert (needswriting >= 0);
      if needswriting = 0
      then CWritten { written = 0 }
      else if needswriting <= maxwritesize
      then (
        Dst.set_string_slice destination offset src source_offset needswriting;
        CWritten { written = needswriting })
      else (
        Dst.set_string_slice destination offset src source_offset maxwritesize;
        CSuspended
          { written = maxwritesize; cont = chunkwriter (source_offset + maxwritesize) })
    in
    writechunked state (chunkwriter 0))
;;

let%expect_test _ =
  let w s =
    let len = 10 in
    let buffer = Bytes.make len '_' in
    let destination = Dst.of_bytes buffer ~offset:1 ~length:8 in
    let state = mk_state destination in
    match write_string state s with
    | Written { state } ->
      assert (state.destination == destination);
      Format.printf "Written: %s" (Bytes.unsafe_to_string buffer)
    | Failed { state; error } ->
      assert (state.destination == destination);
      Format.printf "Failed: %s (%s)" error (Bytes.unsafe_to_string buffer)
    | Suspended { state; cont } ->
      assert (state.destination == destination);
      Format.printf "Suspended: %s\n%!" (Bytes.to_string buffer);
      (match cont destination with
       | Written { state } ->
         assert (state.destination == destination);
         Format.printf "Written: %s" (Bytes.unsafe_to_string buffer)
       | Failed { state; error } ->
         assert (state.destination == destination);
         Format.printf "Failed: %s (%s)" error (Bytes.unsafe_to_string buffer)
       | Suspended _ -> assert false)
    (* no test for double suspension in this block*)
  in
  w "";
  [%expect {| Written: __________ |}];
  w "X";
  [%expect {| Written: _X________ |}];
  w "12345678X";
  [%expect {|
    Suspended: _12345678_
    Written: _X2345678_ |}];
  w "12345678qwertyui";
  [%expect {|
    Suspended: _12345678_
    Written: _qwertyui_ |}];
  ()
;;

let write_bytes state src =
  let size = Bytes.length src in
  if size <= Dst.length state.destination - state.written
  then writef state size (fun d o -> Dst.set_bytes d o src)
  else (
    let rec chunkwriter source_offset destination offset maxwritesize =
      let needswriting = size - source_offset in
      assert (needswriting >= 0);
      if needswriting = 0
      then CWritten { written = 0 }
      else if needswriting <= maxwritesize
      then (
        Dst.set_bytes_slice destination offset src source_offset needswriting;
        CWritten { written = needswriting })
      else (
        Dst.set_bytes_slice destination offset src source_offset maxwritesize;
        CSuspended
          { written = maxwritesize; cont = chunkwriter (source_offset + maxwritesize) })
    in
    writechunked state (chunkwriter 0))
;;

let%expect_test _ =
  let w s =
    let len = 10 in
    let buffer = Bytes.make len '_' in
    let destination = Dst.of_bytes buffer ~offset:1 ~length:8 in
    let state = mk_state destination in
    match write_bytes state (Bytes.unsafe_of_string s) with
    | Written { state } ->
      assert (state.destination == destination);
      Format.printf "Written: %s" (Bytes.unsafe_to_string buffer)
    | Failed { state; error } ->
      assert (state.destination == destination);
      Format.printf "Failed: %s (%s)" error (Bytes.unsafe_to_string buffer)
    | Suspended { state; cont } ->
      assert (state.destination == destination);
      Format.printf "Suspended: %s\n%!" (Bytes.to_string buffer);
      (match cont destination with
       | Written { state } ->
         assert (state.destination == destination);
         Format.printf "Written: %s" (Bytes.unsafe_to_string buffer)
       | Failed { state; error } ->
         assert (state.destination == destination);
         Format.printf "Failed: %s (%s)" error (Bytes.unsafe_to_string buffer)
       | Suspended _ -> assert false)
    (* no test for double suspension in this block*)
  in
  w "";
  [%expect {| Written: __________ |}];
  w "X";
  [%expect {| Written: _X________ |}];
  w "12345678X";
  [%expect {|
    Suspended: _12345678_
    Written: _X2345678_ |}];
  w "12345678qwertyui";
  [%expect {|
    Suspended: _12345678_
    Written: _qwertyui_ |}];
  ()
;;

let rec ( let* ) x f =
  match x with
  | Written { state } -> f state
  | Failed _ -> x
  | Suspended { state; cont } ->
    let cont destination =
      let* x = cont destination in
      f x
    in
    Suspended { state; cont }
;;

let rec to_string_loop buffer acc k =
  let destination = Dst.of_bytes buffer in
  match k destination with
  | Written { state } ->
    let b = Bytes.make state.written '\x00' in
    Dst.blit_onto_bytes state.destination 0 b 0 state.written;
    let s = Bytes.unsafe_to_string b in
    Ok (s :: acc)
  | Failed { state; error } when error = destination_too_small_to_continue_message ->
    (* TODO: instead of restarting with the old-like *)
    assert (state.written = 0);
    let buffer_size = 2 * (1 + Dst.length destination) in
    let buffer = Bytes.make buffer_size '\x00' in
    to_string_loop buffer acc k
  | Failed { state = _; error } -> Error error
  | Suspended { state; cont } ->
    if state.written = 0
    then to_string_loop buffer acc cont
    else (
      let b = Bytes.make state.written '\x00' in
      Dst.blit_onto_bytes state.destination 0 b 0 state.written;
      let s = Bytes.unsafe_to_string b in
      let acc = s :: acc in
      to_string_loop buffer acc cont)
;;

let to_string ?(buffer_size = 1024) writer =
  let buffer = Bytes.make buffer_size '\x00' in
  match
    to_string_loop buffer [] (fun destination ->
      let state = mk_state ~maximum_length:max_int destination in
      writer state)
  with
  | Ok rev_chunks ->
    let chunks = List.rev rev_chunks in
    Ok (String.concat "" chunks)
  | Error _ as err -> err
;;

let blit_instruction_of_state { destination; written; maximum_length = _ } =
  let b, o, l = Dst.blit_instructions destination in
  assert (l <= written);
  b, o, written
;;

let rec blit_instructions_loop destination k () =
  match k destination with
  | Written { state } -> Seq.Cons (blit_instruction_of_state state, Seq.empty)
  | Failed { state = _; error } -> raise (Failure error)
  | Suspended { state; cont } ->
    Seq.Cons (blit_instruction_of_state state, blit_instructions_loop destination cont)
;;

let blit_instructions ~buffer writer =
  let destination = Dst.of_bytes buffer in
  blit_instructions_loop destination (fun destination ->
    let state = mk_state destination in
    writer state)
;;
