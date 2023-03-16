(* TODO: expect tests *)
(* TODO? remove offset, only leave readed: the valid window shrinks *)
(* TODO? add a [read_in_previous_sources] to accumulate the successive readeds, don't shift stop_hints and maximum_length; or maybe we replace [string] with some other form and we don't have this problem anymore *)

type state =
  { source : Src.t
  ; readed : int (* [read] is ambiguous so we make it unambiguously past as [readed] *)
  ; stop_hints : int list
      (* this list is grown when there is a size-header in the encoded binary data *)
  ; maximum_length : int
  }

let rec check_stop_hints base stops maximum_length =
  match stops with
  | [] -> true
  | stop :: stops ->
    base <= stop && stop <= maximum_length && check_stop_hints stop stops maximum_length
;;

let mk_state ?(maximum_length = max_int) ?(stop_hints = []) source =
  if not (check_stop_hints 0 stop_hints maximum_length)
  then failwith "Buffy.R.mk_state: stops out of bounds or out of order";
  { source; readed = 0; stop_hints; maximum_length }
;;

let bump_readed state reading =
  let readed = state.readed + reading in
  assert (readed <= Src.length state.source);
  assert (readed <= state.maximum_length);
  { state with readed }
;;

let rec check_last_stop stops maximum_length =
  match stops with
  | [] -> true
  | [ stop ] -> stop < maximum_length
  | stop :: stops ->
    assert (stop >= 0);
    assert (stop < maximum_length);
    assert (stop <= List.hd stops);
    check_last_stop stops maximum_length
;;

let set_maximum_length state maximum_length =
  if maximum_length < 0
  then raise (Invalid_argument "Buffy.R.set_maximum_length: negative length");
  if maximum_length > state.maximum_length
  then
    raise (Invalid_argument "Buffy.R.set_maximum_length: cannot increase maximum length");
  if not (check_last_stop state.stop_hints maximum_length)
  then
    raise
      (Invalid_argument
         "Buffy.R.set_maximum_length: cannot set maximum length lower than expected stop");
  { state with maximum_length }
;;

(* TODO: there needs to be more tests for stop hints in conjunction with
   suspend-resume *)

let push_stop state length =
  assert (length >= 0);
  if state.readed + length > state.maximum_length
  then Error "expected-stop exceeds maximum-length"
  else (
    let requested_stop = state.readed + length in
    match state.stop_hints with
    | [] -> Ok { state with stop_hints = [ requested_stop ] }
    | previously_requested_stop :: _ ->
      if requested_stop > previously_requested_stop
      then Error "expected-stop exceeds previously requested stop"
      else Ok { state with stop_hints = requested_stop :: state.stop_hints })
;;

let peek_stop state =
  match state.stop_hints with
  | [] -> None
  | stop :: _ -> Some stop
;;

(* TODO? return a [unit readed] instead? With a failed if the stop is different
   from the readed? *)
let pop_stop state =
  match state.stop_hints with
  | [] -> Error "expected an expected-stop but found none"
  | stop :: stop_hints -> Ok (stop, { state with stop_hints })
;;

let bring_first_stop_forward source delta =
  assert (delta >= 0);
  match source.stop_hints with
  | [] -> raise (Invalid_argument "no stop to bring forward")
  | stop :: stop_hints ->
    let new_stop = stop - delta in
    if new_stop < 0
    then raise (Invalid_argument "stop is too forward")
    else { source with stop_hints = new_stop :: stop_hints }
;;

type 'a readed =
  | Readed of
      { state : state
      ; value : 'a
      }
  | Failed of
      { state : state
      ; error : string
      }
  | Suspended of
      { state : state
          (* TODO? add a field to indicate number of chars read from previous buffer *)
          (* TODO? add a field for the buffer that's used as the bridge reading *)
      ; cont : Src.t -> 'a readed
      }

let rec ( let* ) x f =
  match x with
  | Readed { state; value } -> f (value, state)
  | Failed { state; error } -> Failed { state; error }
  | Suspended { state; cont } ->
    let cont source =
      let* x = cont source in
      f x
    in
    Suspended { state; cont }
;;

let rec unsafe_read_copy scratch scratch_offset state =
  let reading = Bytes.length scratch - scratch_offset in
  let readable = Src.length state.source - state.readed in
  if reading <= readable
  then (
    Src.blit_onto_bytes state.source state.readed scratch scratch_offset reading;
    let state = bump_readed state reading in
    Readed { state; value = () })
  else (
    let reading = readable in
    Src.blit_onto_bytes state.source state.readed scratch scratch_offset reading;
    let state = bump_readed state reading in
    let scratch_offset = scratch_offset + reading in
    let maximum_length = state.maximum_length - state.readed in
    let stop_hints = List.map (fun stop -> stop - state.readed) state.stop_hints in
    Suspended
      { state
      ; cont =
          (fun source ->
            let state = mk_state ~maximum_length ~stop_hints source in
            unsafe_read_copy scratch scratch_offset state)
      })
;;

let readf state reading read =
  assert (reading >= 0);
  if state.readed + reading > state.maximum_length
  then Failed { state; error = "maximum-length exceeded" }
  else if match state.stop_hints with
          | [] -> false
          | stop :: _ -> state.readed + reading > stop
  then Failed { state; error = "expected-stop point exceeded" }
  else if state.readed + reading <= Src.length state.source
  then (
    let value = read state.source state.readed in
    let state = bump_readed state reading in
    Readed { state; value })
  else if state.readed = Src.length state.source
  then (
    (* the source was fully consumed, we do a simple continuation *)
    (* To avoid the continuation capturing the [source] we compute some values immediately *)
    let maximum_length = state.maximum_length - state.readed in
    let stop_hints = List.map (fun stop -> stop - state.readed) state.stop_hints in
    Suspended
      { state
      ; cont =
          (fun source ->
            if reading > Src.length source
            then (
              let state = mk_state ~maximum_length ~stop_hints source in
              let scratch = Bytes.make reading '\x00' in
              let scratch_offset = 0 in
              let* (), state = unsafe_read_copy scratch scratch_offset state in
              let value = read (Src.of_bytes scratch) 0 in
              Readed { value; state })
            else (
              let state = mk_state ~maximum_length ~stop_hints source in
              let value = read state.source state.readed in
              let state = bump_readed state reading in
              Readed { state; value }))
      })
  else (
    (* the state has left-over bytes *)
    (* TODO? provide a wrapper with a [copy_threshold] to let the user control
         when the partial read of the remaining of the previous source is
         copied and when is it kept a reference of. *)
    (* TODO? provide a copy_limit and return [Failed] if going over *)
    let split_reading_buffer = Bytes.make reading '\x00' in
    let split_reading_left_length = Src.length state.source - state.readed in
    assert (split_reading_left_length > 0);
    Src.blit_onto_bytes
      state.source
      state.readed
      split_reading_buffer
      0
      split_reading_left_length;
    let state = bump_readed state split_reading_left_length in
    let maximum_length = state.maximum_length - state.readed in
    let stop_hints = List.map (fun stop -> stop - state.readed) state.stop_hints in
    Suspended
      { state
      ; cont =
          (fun source ->
            let state = mk_state ~maximum_length ~stop_hints source in
            let* (), state =
              unsafe_read_copy split_reading_buffer split_reading_left_length state
            in
            let value = read (Src.of_bytes split_reading_buffer) 0 in
            Readed { value; state })
      })
;;

let%expect_test _ =
  let pp_state fmt state =
    Format.fprintf
      fmt
      "source: %S, readed: %d, stops: [%a], maxlen: %d"
      (Src.to_string state.source)
      state.readed
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ',')
         Format.pp_print_int)
      state.stop_hints
      state.maximum_length
  in
  let w state ls =
    Format.printf "State: %a\n" pp_state state;
    match
      List.fold_left
        (fun state l ->
          match readf state l (fun s -> Src.get_string s l) with
          | Suspended _ ->
            Format.printf "Suspended!\n";
            raise Exit
          | Failed { error; state } ->
            Format.printf "Error: %S\nState: %a\n" error pp_state state;
            state
          | Readed { value; state } ->
            Format.printf "Ok: %S\nState: %a\n" value pp_state state;
            state)
        state
        ls
    with
    | exception Exit -> ()
    | _ -> ()
  in
  let full_source = Src.of_string "foobarbaz" in
  let state = mk_state full_source in
  w state [ 3; 0; 6; 1 ];
  [%expect
    {|
    State: source: "foobarbaz", readed: 0, stops: [], maxlen: 4611686018427387903
    Ok: ""
    State: source: "foobarbaz", readed: 3, stops: [], maxlen: 4611686018427387903
    Ok: "foo"
    State: source: "foobarbaz", readed: 3, stops: [], maxlen: 4611686018427387903
    Ok: "baz"
    State: source: "foobarbaz", readed: 9, stops: [], maxlen: 4611686018427387903
    Suspended! |}];
  let state = mk_state ~maximum_length:6 full_source in
  w state [ 3; 0; 6; 3; 1 ];
  [%expect
    {|
    State: source: "foobarbaz", readed: 0, stops: [], maxlen: 6
    Ok: ""
    State: source: "foobarbaz", readed: 3, stops: [], maxlen: 6
    Ok: "foo"
    State: source: "foobarbaz", readed: 3, stops: [], maxlen: 6
    Error: "maximum-length exceeded"
    State: source: "foobarbaz", readed: 3, stops: [], maxlen: 6
    Ok: "bar"
    State: source: "foobarbaz", readed: 6, stops: [], maxlen: 6
    Error: "maximum-length exceeded"
    State: source: "foobarbaz", readed: 6, stops: [], maxlen: 6 |}];
  ()
;;

let read_copy scratch state =
  let reading = Bytes.length scratch in
  if state.readed + reading > state.maximum_length
  then Failed { state; error = "maximum-length exceeded" }
  else if match state.stop_hints with
          | [] -> false
          | stop :: _ -> state.readed + reading > stop
  then Failed { state; error = "expected-stop point exceeded" }
  else unsafe_read_copy scratch 0 state
;;

let read_char state =
  if state.readed + 1 > state.maximum_length
  then Failed { state; error = "maximum-length exceeded" }
  else if match state.stop_hints with
          | [] -> false
          | stop :: _ -> state.readed + 1 > stop
  then Failed { state; error = "expected-stop point exceeded" }
  else if state.readed + 1 > Src.length state.source
  then (
    assert (state.readed = Src.length state.source);
    Suspended
      { state
      ; cont =
          (fun source ->
            (* we are reading 1 char, so we can't have left over from before *)
            let state =
              mk_state
                ~maximum_length:(state.maximum_length - 1)
                ~stop_hints:(List.map (fun stop -> stop - 1) state.stop_hints)
                source
            in
            assert (Src.length source > 0);
            let value = Src.get source state.readed in
            let state = bump_readed state 1 in
            Readed { state; value })
      })
  else (
    let value = Src.get state.source state.readed in
    let state = bump_readed state 1 in
    Readed { state; value })
;;

let%expect_test _ =
  let pp_state fmt state =
    Format.fprintf
      fmt
      "source: %S, readed: %d, stops: [%a], maxlen: %d"
      (Src.to_string state.source)
      state.readed
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ',')
         Format.pp_print_int)
      state.stop_hints
      state.maximum_length
  in
  let w state =
    Format.printf "State: %a\n" pp_state state;
    let rec go state =
      match read_char state with
      | Suspended _ ->
        Format.printf "Suspended!\n";
        ()
      | Failed { error; state } ->
        Format.printf "Error: %S\nState: %a\n" error pp_state state;
        ()
      | Readed { value; state } ->
        Format.printf "Ok: %c\nState: %a\n" value pp_state state;
        go state
    in
    go state
  in
  let state = mk_state (Src.of_string "foobarbaz" ~offset:3 ~length:3) in
  w state;
  [%expect
    {|
    State: source: "bar", readed: 0, stops: [], maxlen: 4611686018427387903
    Ok: b
    State: source: "bar", readed: 1, stops: [], maxlen: 4611686018427387903
    Ok: a
    State: source: "bar", readed: 2, stops: [], maxlen: 4611686018427387903
    Ok: r
    State: source: "bar", readed: 3, stops: [], maxlen: 4611686018427387903
    Suspended! |}];
  let state = mk_state ~maximum_length:3 (Src.of_string "foobarbaz") in
  w state;
  [%expect
    {|
    State: source: "foobarbaz", readed: 0, stops: [], maxlen: 3
    Ok: f
    State: source: "foobarbaz", readed: 1, stops: [], maxlen: 3
    Ok: o
    State: source: "foobarbaz", readed: 2, stops: [], maxlen: 3
    Ok: o
    State: source: "foobarbaz", readed: 3, stops: [], maxlen: 3
    Error: "maximum-length exceeded"
    State: source: "foobarbaz", readed: 3, stops: [], maxlen: 3 |}];
  ()
;;

type 'a chunkreader = Src.t -> 'a chunkreaded

(* similar to [readed] but the state is just the readed count *)
and 'a chunkreaded =
  | CSuspended of
      { readed : int
      ; cont : 'a chunkreader
      }
  | CReaded of
      { readed : int
      ; value : 'a
      }
  | CFailed of
      { readed : int
      ; error : string
      }

let rec readchunked : type a. state -> a chunkreader -> a readed =
 fun state read ->
  let hard_limit = state.maximum_length - state.readed in
  let hard_limit =
    match state.stop_hints with
    | [] -> hard_limit
    | stop :: _ -> min hard_limit (stop - state.readed)
  in
  let suspendable_limit = Src.length state.source - state.readed in
  let reading = min hard_limit suspendable_limit in
  assert (reading <= hard_limit);
  assert (reading >= 0);
  match read (Src.of_src state.source ~offset:state.readed ~length:reading) with
  | CReaded { value; readed } ->
    assert (readed <= reading);
    let state = bump_readed state readed in
    Readed { state; value }
  | CSuspended { readed; cont } ->
    assert (readed <= reading);
    if readed = hard_limit
    then (
      let error = "chunkreader requests more bytes but hard limit was reached" in
      Failed { error; state })
    else (
      let state = bump_readed state readed in
      Suspended
        { state
        ; cont =
            (fun source ->
              let state =
                mk_state
                  ~maximum_length:(state.maximum_length - state.readed)
                  ~stop_hints:
                    (List.map (fun stop -> stop - state.readed) state.stop_hints)
                  source
              in
              readchunked state cont)
        })
  | CFailed { readed; error } ->
    let state = bump_readed state readed in
    let error = "Error in chunk-reader: " ^ error in
    Failed { state; error }
;;

(* TODO: have the user pass the buffer for [read_bytes] to avoid major
   allocation. *)

let read_bytes state len =
  let scratch = Bytes.make len '\000' in
  let* (), state = read_copy scratch state in
  Readed { value = scratch; state }
;;

let read_string state len =
  let scratch = Bytes.make len '\000' in
  let* (), state = read_copy scratch state in
  Readed { value = Bytes.unsafe_to_string scratch; state }
;;

let%expect_test _ =
  let pp_state fmt state =
    Format.fprintf
      fmt
      "source: %S, readed: %d, stops: [%a], maxlen: %d"
      (Src.to_string state.source)
      state.readed
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ',')
         Format.pp_print_int)
      state.stop_hints
      state.maximum_length
  in
  let w state ls =
    Format.printf "State: %a\n" pp_state state;
    match
      List.fold_left
        (fun state l ->
          match read_string state l with
          | Suspended _ ->
            Format.printf "Suspended!\n";
            raise Exit
          | Failed { error; state } ->
            Format.printf "Error: %S\nState: %a\n" error pp_state state;
            state
          | Readed { value; state } ->
            Format.printf "Ok: %S\nState: %a\n" value pp_state state;
            state)
        state
        ls
    with
    | exception Exit -> ()
    | _ -> ()
  in
  let state = mk_state (Src.of_string "foobarbaz") in
  w state [ 3; 0; 6; 1 ];
  [%expect
    {|
    State: source: "foobarbaz", readed: 0, stops: [], maxlen: 4611686018427387903
    Ok: "foo"
    State: source: "foobarbaz", readed: 3, stops: [], maxlen: 4611686018427387903
    Ok: ""
    State: source: "foobarbaz", readed: 3, stops: [], maxlen: 4611686018427387903
    Ok: "barbaz"
    State: source: "foobarbaz", readed: 9, stops: [], maxlen: 4611686018427387903
    Suspended! |}];
  let state = mk_state ~maximum_length:6 (Src.of_string "foobarbaz") in
  w state [ 3; 0; 6; 3; 1 ];
  [%expect
    {|
    State: source: "foobarbaz", readed: 0, stops: [], maxlen: 6
    Ok: "foo"
    State: source: "foobarbaz", readed: 3, stops: [], maxlen: 6
    Ok: ""
    State: source: "foobarbaz", readed: 3, stops: [], maxlen: 6
    Error: "maximum-length exceeded"
    State: source: "foobarbaz", readed: 3, stops: [], maxlen: 6
    Ok: "bar"
    State: source: "foobarbaz", readed: 6, stops: [], maxlen: 6
    Error: "maximum-length exceeded"
    State: source: "foobarbaz", readed: 6, stops: [], maxlen: 6 |}];
  ();
  let w ss ls =
    let rec go ss cont =
      match cont () with
      | Suspended { state; cont } ->
        Format.printf "Suspended!\nState: %a\n" pp_state state;
        let s = List.hd ss in
        let ss = List.tl ss in
        go ss (fun () -> cont (Src.of_string s))
      | Failed { error; state } as failed ->
        Format.printf "Error: %S\nState: %a\n" error pp_state state;
        failed, state, ss
      | Readed { value; state } as readed ->
        Format.printf "Ok: %S\nState: %a\n" value pp_state state;
        readed, state, ss
    in
    let rec reads state ss ls =
      match ls with
      | [] -> ()
      | l :: ls ->
        (match go ss (fun () -> read_string state l) with
         | Suspended _, _, _ -> assert false
         | (Failed _ | Readed _), state, ss -> reads state ss ls)
    in
    let s = List.hd ss in
    let ss = List.tl ss in
    let state = mk_state (Src.of_string s) in
    Format.printf "Start!\nState: %a\n" pp_state state;
    reads state ss ls
  in
  w [ "foo"; "bar"; "baz" ] [ 3; 0; 6 ];
  [%expect
    {|
    Start!
    State: source: "foo", readed: 0, stops: [], maxlen: 4611686018427387903
    Ok: "foo"
    State: source: "foo", readed: 3, stops: [], maxlen: 4611686018427387903
    Ok: ""
    State: source: "foo", readed: 3, stops: [], maxlen: 4611686018427387903
    Suspended!
    State: source: "foo", readed: 3, stops: [], maxlen: 4611686018427387903
    Suspended!
    State: source: "bar", readed: 3, stops: [], maxlen: 4611686018427387900
    Ok: "barbaz"
    State: source: "baz", readed: 3, stops: [], maxlen: 4611686018427387897 |}];
  w
    (let rec xs = "xyz" :: xs in
     xs)
    [ 4; 4; 4 ];
  [%expect
    {|
    Start!
    State: source: "xyz", readed: 0, stops: [], maxlen: 4611686018427387903
    Suspended!
    State: source: "xyz", readed: 3, stops: [], maxlen: 4611686018427387903
    Ok: "xyzx"
    State: source: "xyz", readed: 1, stops: [], maxlen: 4611686018427387900
    Suspended!
    State: source: "xyz", readed: 3, stops: [], maxlen: 4611686018427387900
    Ok: "yzxy"
    State: source: "xyz", readed: 2, stops: [], maxlen: 4611686018427387897
    Suspended!
    State: source: "xyz", readed: 3, stops: [], maxlen: 4611686018427387897
    Ok: "zxyz"
    State: source: "xyz", readed: 3, stops: [], maxlen: 4611686018427387894 |}];
  ()
;;

(* TODO: avoid so many calls to [read_char] and [let*] to speed up reading *)
let read_utf8_uchar state =
  let* c, state = read_char state in
  let c = Char.code c in
  if c land 0b1000_0000 = 0b0000_0000
  then (
    (* length 1 *)
    let value = Uchar.of_int c in
    Readed { state; value })
  else if c land 0b1110_0000 = 0b1100_0000
  then
    (* length 2 *)
    let* c1, state = read_char state in
    let c1 = Char.code c1 in
    if c1 land 0b1100_0000 = 0b1000_0000
    then (
      let c = (c land 0b0001_1111) lsl 6 in
      let c1 = c1 land 0b0011_1111 in
      let point = c lor c1 in
      let value = Uchar.of_int point in
      Readed { state; value })
    else (
      let error = "Invalid UTF8 byte" in
      Failed { state; error })
  else if c land 0b1111_0000 = 0b1110_0000
  then
    (* length 3 *)
    let* c1, state = read_char state in
    let c1 = Char.code c1 in
    let* c2, state = read_char state in
    let c2 = Char.code c2 in
    if c1 land 0b1100_0000 = 0b1000_0000 && c2 land 0b1100_0000 = 0b1000_0000
    then (
      let c = (c land 0b0000_1111) lsl 12 in
      let c1 = (c1 land 0b0011_1111) lsl 6 in
      let c2 = c2 land 0b0011_1111 in
      let point = c lor c1 lor c2 in
      let value = Uchar.of_int point in
      Readed { state; value })
    else (
      let error = "Invalid UTF8 byte" in
      Failed { state; error })
  else if c land 0b1111_1000 = 0b1111_0000
  then
    (* length 4 *)
    let* c1, state = read_char state in
    let c1 = Char.code c1 in
    let* c2, state = read_char state in
    let c2 = Char.code c2 in
    let* c3, state = read_char state in
    let c3 = Char.code c3 in
    if c1 land 0b1100_0000 = 0b1000_0000
       && c2 land 0b1100_0000 = 0b1000_0000
       && c3 land 0b1100_0000 = 0b1000_0000
    then (
      let c = (c land 0b0000_0111) lsl 18 in
      let c1 = (c1 land 0b0011_1111) lsl 12 in
      let c2 = (c2 land 0b0011_1111) lsl 6 in
      let c3 = c3 land 0b0011_1111 in
      let point = c lor c1 lor c2 lor c3 in
      let value = Uchar.of_int point in
      Readed { state; value })
    else (
      let error = "Invalid UTF8 byte" in
      Failed { state; error })
  else (
    let error = "Invalid UTF8 leading byte" in
    Failed { state; error })
;;

let%expect_test _ =
  let w s =
    let rec loop acc state =
      match read_utf8_uchar state with
      | Readed { value; state } -> loop (value :: acc) state
      | Failed { error; state = _ } -> Error error
      | Suspended { state = _; cont = _ } -> Ok (List.rev acc)
    in
    match loop [] (mk_state (Src.of_string s)) with
    | Ok uchars ->
      Format.printf
        "%a\n"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_char fmt '.')
           (fun fmt u -> Format.fprintf fmt "x%x" (Uchar.to_int u)))
        uchars
    | Error error -> Format.printf "Error %s\n" error
  in
  w "";
  [%expect {| |}];
  w "abc";
  [%expect {| x61.x62.x63 |}];
  w "\xff";
  [%expect {| Error Invalid UTF8 leading byte |}];
  w "$";
  [%expect {| x24 |}];
  w "£";
  [%expect {| xa3 |}];
  w "€";
  [%expect {| x20ac |}];
  w "𐍈";
  [%expect {| x10348 |}];
  ()
;;

let read_uint8 state = readf state 1 Src.get_uint8

let of_string read s =
  let state = mk_state (Src.of_string s) in
  match read state with
  | Readed { state; value } ->
    assert (state.readed <= Src.length state.source);
    if state.readed < Src.length state.source then Error "Too many bytes" else Ok value
  | Failed { state = _; error } -> Error error
  | Suspended { state = _; cont = _ } -> Error "Not enough bytes"
;;

let rec of_string_seq_loop seq cont =
  match seq () with
  | Seq.Nil -> Error "Not enough chunks or bytes"
  | Seq.Cons (s, seq) ->
    (match cont (Src.of_string s) with
     | Readed { state; value } ->
       assert (state.readed <= Src.length state.source);
       if state.readed < Src.length state.source
       then Error "Too many bytes"
       else if Seq.is_empty seq
       then Ok value
       else Error "Too many chunks"
     | Failed { state = _; error } -> Error error
     | Suspended { state = _; cont } -> of_string_seq_loop seq cont)
;;

let of_string_seq read s =
  of_string_seq_loop s (fun source ->
    let state = mk_state source in
    read state)
;;
