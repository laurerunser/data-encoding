[@@@landmark "auto"]

(* TODO: expect tests *)
(* TODO: make size_limits mutable? *)

type state =
  { destination : Dst.t
  ; maximum_size : int
  ; size_limits : int list
  }

let written { destination; _ } = Dst.added destination

let mk_state ?(maximum_size = max_int) destination =
  { destination; maximum_size; size_limits = [] }
;;

let internal_mk_state maximum_size size_limits destination =
  { destination; maximum_size; size_limits }
;;

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

let push_limit state length =
  assert (length >= 0);
  let requested_limit = Dst.added state.destination + length in
  if requested_limit > state.maximum_size
  then Error "size-limit exceeds maximum-size"
  else (
    match state.size_limits with
    | inner_most_limit :: _ when requested_limit > inner_most_limit ->
      (* limits need to be correctly nested: you cannot have an inner
         size-limit pointing further than an outer size-limit *)
      Error "size-limit exceeds previously set size-limit"
    | _ -> Ok { state with size_limits = requested_limit :: state.size_limits })
;;

let remove_limit state =
  match state.size_limits with
  | [] -> Error "expected a size-limit but found none"
  | _ :: size_limits -> Ok { state with size_limits }
;;

let destination_too_small_to_continue_message =
  "new destination buffer is too small to continue"
;;

let writef state width write =
  assert (width >= 0);
  let reach = Dst.added state.destination + width in
  if reach > state.maximum_size
  then Failed { state; error = "maximum-size exceeded" }
  else if match state.size_limits with
          | [] -> false
          | limit :: _ -> reach > limit
  then Failed { state; error = "size-limit exceeded" }
  else if width <= Dst.available state.destination
  then (
    write state.destination;
    Written { state })
  else (
    (* TODO? add an option which does some copying to a temporary buffer and
       uses the original destination to its maximum capacity. (Similar to
       Reading case.) *)
    let maximum_size = state.maximum_size - Dst.added state.destination in
    let size_limits =
      List.map (fun limit -> limit - Dst.added state.destination) state.size_limits
    in
    Suspended
      { state
      ; cont =
          (fun destination ->
            let state = internal_mk_state maximum_size size_limits destination in
            if Dst.available destination < width
            then Failed { state; error = destination_too_small_to_continue_message }
            else (
              write state.destination;
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
  w 0 (fun _ -> ());
  [%expect {| Written: __________ |}];
  w 1 (fun b -> Dst.add_char b 'X');
  [%expect {| Written: _X________ |}];
  w 2 (fun b ->
    Dst.add_char b '/';
    Dst.add_char b '\\');
  [%expect {| Written: _/\_______ |}];
  w 3 (fun b -> Dst.add_string b "oOo");
  [%expect {| Written: _oOo______ |}];
  ()
;;

let write_char state c = writef state 1 (fun b -> Dst.add_char b c)

let%expect_test _ =
  let w cs =
    let len = 10 in
    let buffer = Bytes.make len '_' in
    let destination = Dst.of_bytes buffer ~offset:1 ~length:8 in
    match
      List.fold_left
        (fun state c ->
          match write_char state c with
          | Written { state } ->
            assert (state.destination == destination);
            Format.printf "Written: %s\n%!" (Bytes.unsafe_to_string buffer);
            state
          | Failed { state; error } ->
            assert (state.destination == destination);
            Format.printf "Failed: %s (%s)\n%!" error (Bytes.unsafe_to_string buffer);
            raise Exit
          | Suspended _ ->
            Format.printf "Suspended\n%!";
            raise Exit)
        (mk_state destination)
        cs
    with
    | exception Exit -> ()
    | _state -> ()
  in
  w [ '+' ];
  [%expect {| Written: _+________ |}];
  w (List.of_seq (String.to_seq "aBcDeFgHiJkLmN"));
  [%expect
    {|
    Written: _a________
    Written: _aB_______
    Written: _aBc______
    Written: _aBcD_____
    Written: _aBcDe____
    Written: _aBcDeF___
    Written: _aBcDeFg__
    Written: _aBcDeFgH_
    Suspended |}];
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
    writef state 2 (fun b ->
      Dst.add_uint8 b c0;
      Dst.add_uint8 b c1))
  else if c < 0b1_0000_000000_000000
  then (
    let c0 = 0b1110_0000 lor ((c land 0b1111_000000_000000) lsr 12) in
    let c1 = 0b10_000000 lor ((c land 0b0000_111111_000000) lsr 6) in
    let c2 = 0b10_000000 lor (c land 0b0000_000000_111111) in
    writef state 2 (fun b ->
      Dst.add_uint8 b c0;
      Dst.add_uint8 b c1;
      Dst.add_uint8 b c2))
  else if c < 0b1_000_000000_000000_000000
  then (
    let c0 = 0b11110_000 lor ((c land 0b111_000000_000000_000000) lsr 18) in
    let c1 = 0b10_000000 lor ((c land 0b000_111111_000000_000000) lsr 12) in
    let c2 = 0b10_000000 lor ((c land 0b000_000000_111111_000000) lsr 6) in
    let c3 = 0b10_000000 lor (c land 0b000_000000_000000_111111) in
    writef state 2 (fun b ->
      Dst.add_uint8 b c0;
      Dst.add_uint8 b c1;
      Dst.add_uint8 b c2;
      Dst.add_uint8 b c3))
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

type chunkwriter = Dst.t -> int -> chunkwritten

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
  let width =
    min
      (state.maximum_size - Dst.added state.destination)
      (Dst.available state.destination)
  in
  let width =
    match state.size_limits with
    | [] -> width
    | limit :: _ -> min (limit - Dst.added state.destination) width
  in
  assert (width >= 0);
  match write state.destination width with
  | CWritten { written = _ } -> Written { state }
  | CFailed { written = _; error } -> Failed { state; error }
  | CSuspended { written; cont } ->
    if written > width
    then
      raise
        (Failure "Suspendable_buffers.Writing.writechunked: chunkwriter exceeded limit");
    let maximum_size = state.maximum_size - Dst.added state.destination in
    let size_limits =
      List.map (fun limit -> limit - Dst.added state.destination) state.size_limits
    in
    assert (maximum_size >= 0);
    if maximum_size = 0
    then Failed { state; error = "maximum-size reached but chunk-writer is not finished" }
    else if match size_limits with
            | [] -> false
            | limit :: _ ->
              assert (limit >= 0);
              limit = 0
    then Failed { state; error = "size-limit reached but chunk-writer is not finished" }
    else
      Suspended
        { state
        ; cont =
            (fun destination ->
              let state = internal_mk_state maximum_size size_limits destination in
              writechunked state cont)
        }
;;

let write_string state src =
  let size = String.length src in
  if size <= Dst.available state.destination
  then writef state size (fun d -> Dst.add_string d src)
  else (
    let rec chunkwriter source_offset destination maxwritesize =
      let needswriting = size - source_offset in
      assert (needswriting >= 0);
      if needswriting = 0
      then CWritten { written = 0 }
      else if needswriting <= maxwritesize
      then (
        Dst.add_string_slice destination src source_offset needswriting;
        CWritten { written = needswriting })
      else (
        Dst.add_string_slice destination src source_offset maxwritesize;
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
    | Suspended { state = _; cont } ->
      Format.printf "Suspended: %s\n%!" (Bytes.to_string buffer);
      (match cont (Dst.of_bytes buffer ~offset:1 ~length:8) with
       | Written { state = _ } ->
         Format.printf "Written: %s" (Bytes.unsafe_to_string buffer)
       | Failed { state = _; error } ->
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
  if size <= Dst.available state.destination
  then writef state size (fun d -> Dst.add_bytes d src)
  else (
    let rec chunkwriter source_offset destination maxwritesize =
      let needswriting = size - source_offset in
      assert (needswriting >= 0);
      if needswriting = 0
      then CWritten { written = 0 }
      else if needswriting <= maxwritesize
      then (
        Dst.add_bytes_slice destination src source_offset needswriting;
        CWritten { written = needswriting })
      else (
        Dst.add_bytes_slice destination src source_offset maxwritesize;
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
    | Suspended { state = _; cont } ->
      Format.printf "Suspended: %s\n%!" (Bytes.to_string buffer);
      (match cont (Dst.of_bytes buffer ~offset:1 ~length:8) with
       | Written { state = _ } ->
         Format.printf "Written: %s" (Bytes.unsafe_to_string buffer)
       | Failed { state = _; error } ->
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

let write_uint8 state v = writef state 1 (fun dst -> Dst.add_uint8 dst v)
let write_int8 state v = writef state 1 (fun dst -> Dst.add_int8 dst v)
let write_uint16_be state v = writef state 2 (fun dst -> Dst.add_uint16_be dst v)
let write_uint16_le state v = writef state 2 (fun dst -> Dst.add_uint16_le dst v)
let write_int16_be state v = writef state 2 (fun dst -> Dst.add_int16_be dst v)
let write_int16_le state v = writef state 2 (fun dst -> Dst.add_int16_le dst v)
let write_int32_be state v = writef state 4 (fun dst -> Dst.add_int32_be dst v)
let write_int32_le state v = writef state 4 (fun dst -> Dst.add_int32_le dst v)
let write_int64_be state v = writef state 8 (fun dst -> Dst.add_int64_be dst v)
let write_int64_le state v = writef state 8 (fun dst -> Dst.add_int64_le dst v)

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
    let b = Bytes.make (Dst.added state.destination) '\x00' in
    Dst.blit_onto_bytes state.destination 0 b 0 (Dst.added state.destination);
    let s = Bytes.unsafe_to_string b in
    Ok (s :: acc)
  | Failed { state; error } when error = destination_too_small_to_continue_message ->
    (* TODO: instead of restarting with the old-like *)
    assert (Dst.added state.destination = 0);
    let buffer_size = 2 * (1 + Bytes.length buffer) in
    let buffer = Bytes.make buffer_size '\x00' in
    to_string_loop buffer acc k
  | Failed { state = _; error } -> Error error
  | Suspended { state; cont } ->
    if Dst.added state.destination = 0
    then to_string_loop buffer acc cont
    else (
      let b = Bytes.make (Dst.added state.destination) '\x00' in
      Dst.blit_onto_bytes state.destination 0 b 0 (Dst.added state.destination);
      let s = Bytes.unsafe_to_string b in
      let acc = s :: acc in
      to_string_loop buffer acc cont)
;;

let to_string ?(buffer_size = 1024) writer =
  let buffer = Bytes.make buffer_size '\x00' in
  match
    to_string_loop buffer [] (fun destination ->
      let state = mk_state ~maximum_size:max_int destination in
      writer state)
  with
  | Ok rev_chunks ->
    let chunks = List.rev rev_chunks in
    Ok (String.concat "" chunks)
  | Error _ as err -> err
;;

let blit_instruction_of_state { destination; maximum_size = _; size_limits = _ } =
  let b, o, l = Dst.bytes_of_dst destination in
  assert (l <= Dst.added destination);
  b, o, Dst.added destination
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
