[@@@landmark "auto"]

(* TODO: expect tests *)
(* TODO? consolidate maximum-size, size-limits, and stop-hints into a single
   list (or list-like structure?) to simplify overflow-checks and
   nesting-checks? *)

(* The state when reading.

   A reading process can be spread across multiple sources (see documentation of
   the [readed] type).

   The maximum-size is the maximum number of byte that the reading process is
   allowed to consume. The maximum-size is allowed to overshoot the length
   of the source, in which case it is patched when a suspension is resumed.
   Specifically, the value of maximum-size in the state of the resumed
   suspension is reduced by the number of bytes read in the
   previous step. E.g., consider a reading process limited to 500 bytes, it
   reads 100 bytes, it is suspended: the resumed process is limited to 400
   bytes.

   The {e size limits} are used as a stack to register the local size-limits.
   E.g., when reading a sequence of values where the total size is not allowed
   to exceed a certain number of bytes, a size-limit is pushed; when the
   sequence has been entirely read the size-limit is removed. If a size-limit is
   exceeded, the reading process fails. A size-limit can overshoot the length of
   the source, in which case all the size-limits are patched when a suspension is
   resumed.

   The {e stop hints} are used as a stack to register the size-headers. E.g.,
   when reading a data-structure with a field indicating the size of the next
   field, a stop-hint is pushed; when reading the next field, the stop point is
   used to stop at the expected offset. A stop-point can overshoot the length of
   the source, in which case all the stop-hints are patched when a suspension is
   resumed. *)
type state =
  { source : Src.t
  ; maximum_size : int
  ; size_limits : int list
      (* this list is grown when there is a size-limit constructor in the encoding *)
  ; stop_hints : int list
      (* this list is grown when there is a size-header in the encoded binary data *)
  }

let readed { source; _ } = Src.gotten source

(* checks invariant regarding the size-limits and stop-hints: that they are in
   increasing order and lower than maximum-size *)
let rec check_stops_and_limits base indices maximum_size =
  match indices with
  | [] -> true
  | index :: indices ->
    base <= index
    && index <= maximum_size
    && check_stops_and_limits index indices maximum_size
;;

let%expect_test _ =
  let w stops maxlen =
    if check_stops_and_limits 0 stops maxlen
    then Format.printf "Ok\n%!"
    else Format.printf "Not\n%!"
  in
  w [] 0;
  [%expect {| Ok |}];
  w [] max_int;
  [%expect {| Ok |}];
  w [ 0; 1; 2 ] max_int;
  [%expect {| Ok |}];
  w [ 0; 0; 2 ] 2;
  [%expect {| Ok |}];
  w [ 1; 0; 2 ] 2;
  (* out-of-order stops *)
  [%expect {| Not |}];
  w [ 0; 1; 2 ] 1;
  (* stops beyond maximum size *)
  [%expect {| Not |}];
  ()
;;

let mk_state ?(maximum_size = max_int) source =
  if maximum_size < 0 then failwith "Buffy.R.mk_state: maximum_size cannot be negative";
  { source; stop_hints = []; size_limits = []; maximum_size }
;;

(* The {e internal} version of [mk_state] takes [stop_hints] as additional
   parameters. These are not available in the public API because they are never
   set from the outside, only from the reading process itself. *)
let internal_mk_state maximum_size size_limits stop_hints source =
  { source; stop_hints; size_limits; maximum_size }
;;

(* TODO: there needs to be more tests for size-limits, stop-hints and
   maximum-size, in conjunction with suspend-resume *)

let push_stop state length =
  assert (length >= 0);
  let requested_stop = length + Src.gotten state.source in
  if requested_stop > state.maximum_size
  then Error "expected-stop exceeds maximum-size"
  else (
    match state.stop_hints with
    | previously_requested_stop :: _ when requested_stop > previously_requested_stop ->
      (* stops need to be correctly nested: you cannot have an inner size-header
         pointing further than an outer size-header *)
      Error "expected-stop exceeds previously requested stop"
    | _ ->
      (match state.size_limits with
       | inner_most_limit :: _ when requested_stop > inner_most_limit ->
         (* stops need to be correctly nested with limits: you cannot have an inner
         size-header pointing further than an outer size-limit *)
         Error "expected-stop exceeds previously set size-limit"
       | _ -> Ok { state with stop_hints = requested_stop :: state.stop_hints }))
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

let push_limit state length =
  assert (length >= 0);
  let requested_limit = length + Src.gotten state.source in
  if requested_limit > state.maximum_size
  then Error "size-limit exceeds maximum-size"
  else (
    match state.size_limits with
    | inner_most_limit :: _ when requested_limit > inner_most_limit ->
      (* limits need to be correctly nested: you cannot have an inner
         size-limit pointing further than an outer size-limit *)
      Error "size-limit exceeds previously set size-limit"
    | _ ->
      (match state.stop_hints with
       | previously_requested_stop :: _ when requested_limit > previously_requested_stop
         ->
         (* limits need to be correctly nested with pointers: you cannot have an
         inner size-limit pointing further than an outer size-header *)
         Error "size-limit exceeds previously requested stop"
       | _ -> Ok { state with size_limits = requested_limit :: state.size_limits }))
;;

let remove_limit state =
  match state.size_limits with
  | [] -> Error "expected a size-limit but found none"
  | _ :: size_limits -> Ok { state with size_limits }
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
  | Readed { state; value } ->
    (* TODO? can we avoid tuple allocation by having the Readed payload as an
         intermediary type? *)
    f (value, state)
  | Failed { state; error } -> Failed { state; error }
  | Suspended { state; cont } ->
    let cont source =
      let* x = cont source in
      f x
    in
    Suspended { state; cont }
;;

let rec unsafe_readcopy scratch scratch_offset state =
  let width = Bytes.length scratch - scratch_offset in
  let readable = Src.available state.source in
  if width <= readable
  then (
    Src.get_blit_onto_bytes state.source scratch scratch_offset width;
    Readed { state; value = () })
  else (
    let width = readable in
    Src.get_blit_onto_bytes state.source scratch scratch_offset width;
    let scratch_offset = scratch_offset + width in
    let maximum_size = state.maximum_size - Src.gotten state.source in
    let stop_hints =
      List.map (fun stop -> stop - Src.gotten state.source) state.stop_hints
    in
    let size_limits =
      List.map (fun limit -> limit - Src.gotten state.source) state.size_limits
    in
    assert (check_stops_and_limits 0 stop_hints maximum_size);
    assert (check_stops_and_limits 0 size_limits maximum_size);
    Suspended
      { state
      ; cont =
          (fun source ->
            let state = internal_mk_state maximum_size size_limits stop_hints source in
            unsafe_readcopy scratch scratch_offset state)
      })
;;

let readf state width read =
  assert (width >= 0);
  let reach = Src.gotten state.source + width in
  if reach > state.maximum_size
  then Failed { state; error = "maximum-size exceeded" }
  else if match state.size_limits with
          | [] -> false
          | limit :: _ -> reach > limit
  then Failed { state; error = "size-limit exceeded" }
  else if match state.stop_hints with
          | [] -> false
          | stop :: _ -> reach > stop
  then Failed { state; error = "expected-stop point exceeded" }
  else if width <= Src.available state.source
  then (
    let value = read state.source in
    Readed { state; value })
  else if Src.available state.source = 0
  then (
    (* the source was fully consumed, we do a simple continuation *)
    (* To avoid the continuation capturing the [source] we compute some values immediately *)
    let maximum_size = state.maximum_size - Src.gotten state.source in
    let stop_hints =
      List.map (fun stop -> stop - Src.gotten state.source) state.stop_hints
    in
    let size_limits =
      List.map (fun limit -> limit - Src.gotten state.source) state.size_limits
    in
    assert (check_stops_and_limits 0 stop_hints maximum_size);
    assert (check_stops_and_limits 0 size_limits maximum_size);
    Suspended
      { state
      ; cont =
          (fun source ->
            if width > Src.available source
            then (
              let state = internal_mk_state maximum_size size_limits stop_hints source in
              (* TODO: document this allocation, also see TODOs below *)
              let scratch = Bytes.make width '\x00' in
              let scratch_offset = 0 in
              let* (), state = unsafe_readcopy scratch scratch_offset state in
              let value = read (Src.of_bytes scratch) in
              Readed { value; state })
            else (
              let state = internal_mk_state maximum_size size_limits stop_hints source in
              let value = read state.source in
              Readed { state; value }))
      })
  else (
    (* state.readed < Src.length state.source < reach < all-limits *)
    (* the state has left-over bytes *)
    (* TODO? provide a wrapper with a [copy_threshold] to let the user control
         when the partial read of the remaining of the previous source is
         copied and when is it kept a reference of. *)
    (* TODO? provide a copy_limit and return [Failed] if going over *)
    let split_reading_buffer = Bytes.make width '\x00' in
    let* (), state = unsafe_readcopy split_reading_buffer 0 state in
    let value = read (Src.of_bytes split_reading_buffer) in
    Readed { value; state })
;;

let%expect_test _ =
  let pp_state fmt state =
    Format.fprintf
      fmt
      "source: %S, readed: %d, stops: [%a], maxlen: %d%!"
      (Src.to_string state.source)
      (Src.gotten state.source)
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ',')
         Format.pp_print_int)
      state.stop_hints
      state.maximum_size
  in
  let w state ls =
    let state = state () in
    Format.printf "State: %a\n%!" pp_state state;
    match
      List.fold_left
        (fun state l ->
          match readf state l (fun s -> Src.get_string s l) with
          | Suspended _ ->
            Format.printf "Suspended!\n%!";
            raise Exit
          | Failed { error; state } ->
            Format.printf "Error: %S\nState: %a\n%!" error pp_state state;
            state
          | Readed { value; state } ->
            Format.printf "Ok: %S\nState: %a\n%!" value pp_state state;
            state)
        state
        ls
    with
    | exception Exit -> ()
    | _ -> ()
  in
  let full_source () = Src.of_string "foobarbaz" in
  let state () = mk_state (full_source ()) in
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
  let state () = mk_state ~maximum_size:6 (full_source ()) in
  w state [ 3; 0; 6; 3; 1 ];
  [%expect
    {|
    State: source: "foobarbaz", readed: 0, stops: [], maxlen: 6
    Ok: "foo"
    State: source: "foobarbaz", readed: 3, stops: [], maxlen: 6
    Ok: ""
    State: source: "foobarbaz", readed: 3, stops: [], maxlen: 6
    Error: "maximum-size exceeded"
    State: source: "foobarbaz", readed: 3, stops: [], maxlen: 6
    Ok: "bar"
    State: source: "foobarbaz", readed: 6, stops: [], maxlen: 6
    Error: "maximum-size exceeded"
    State: source: "foobarbaz", readed: 6, stops: [], maxlen: 6 |}];
  ()
;;

let readcopy scratch state =
  let width = Bytes.length scratch in
  let reach = Src.gotten state.source + width in
  if reach > state.maximum_size
  then Failed { state; error = "maximum-size exceeded" }
  else if match state.size_limits with
          | [] -> false
          | limit :: _ -> reach > limit
  then Failed { state; error = "size-limit exceeded" }
  else if match state.stop_hints with
          | [] -> false
          | stop :: _ -> reach > stop
  then Failed { state; error = "expected-stop point exceeded B" }
  else unsafe_readcopy scratch 0 state
;;

let read_char state =
  let reach = Src.gotten state.source + 1 in
  if reach > state.maximum_size
  then Failed { state; error = "maximum-size exceeded" }
  else if match state.size_limits with
          | [] -> false
          | limit :: _ -> reach > limit
  then Failed { state; error = "size-limit exceeded" }
  else if match state.stop_hints with
          | [] -> false
          | stop :: _ -> reach > stop
  then Failed { state; error = "expected-stop point exceeded C" }
  else if 1 <= Src.available state.source
  then (
    let value = Src.get_char state.source in
    Readed { state; value })
  else (
    (* we are reading 1 char, so we can't have left over from before *)
    assert (Src.available state.source = 0);
    let maximum_size = state.maximum_size - 1 in
    let stop_hints = List.map (fun stop -> stop - 1) state.stop_hints in
    let size_limits = List.map (fun limit -> limit - 1) state.size_limits in
    assert (check_stops_and_limits 0 stop_hints maximum_size);
    assert (check_stops_and_limits 0 size_limits maximum_size);
    Suspended
      { state
      ; cont =
          (fun source ->
            let state = internal_mk_state maximum_size size_limits stop_hints source in
            assert (Src.available source > 0);
            let value = Src.get_char source in
            Readed { state; value })
      })
;;

let%expect_test _ =
  let pp_state fmt state =
    Format.fprintf
      fmt
      "source: %S, readed: %d, stops: [%a], maxlen: %d"
      (Src.to_string state.source)
      (Src.gotten state.source)
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ',')
         Format.pp_print_int)
      state.stop_hints
      state.maximum_size
  in
  let w state =
    Format.printf "State: %a\n%!" pp_state state;
    let rec go state =
      match read_char state with
      | Suspended _ ->
        Format.printf "Suspended!\n%!";
        ()
      | Failed { error; state } ->
        Format.printf "Error: %S\nState: %a\n%!" error pp_state state;
        ()
      | Readed { value; state } ->
        Format.printf "Ok: %c\nState: %a\n%!" value pp_state state;
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
  let state = mk_state ~maximum_size:3 (Src.of_string "foobarbaz") in
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
    Error: "maximum-size exceeded"
    State: source: "foobarbaz", readed: 3, stops: [], maxlen: 3 |}];
  ()
;;

type 'a chunkreader = Src.t -> int -> 'a chunkreaded

(* similar to [readed] but the state is just the readed count *)
and 'a chunkreaded =
  | CReaded of
      { readed : int
      ; value : 'a
      }
  | CFailed of
      { readed : int
      ; error : string
      }
  | CSuspended of
      { readed : int
      ; cont : 'a chunkreader
      }

let rec readchunked : type a. state -> a chunkreader -> a readed =
 fun state read ->
  let hard_limit = state.maximum_size - Src.gotten state.source in
  let hard_limit =
    match state.size_limits with
    | [] -> hard_limit
    | limit :: _ -> min hard_limit (limit - Src.gotten state.source)
  in
  let hard_limit =
    match state.stop_hints with
    | [] -> hard_limit
    | stop :: _ -> min hard_limit (stop - Src.gotten state.source)
  in
  let suspendable_limit = Src.available state.source in
  let max_width = min hard_limit suspendable_limit in
  assert (max_width <= hard_limit);
  assert (max_width >= 0);
  match read state.source max_width with
  | CReaded { value; readed } ->
    assert (readed <= max_width);
    Readed { state; value }
  | CSuspended { readed; cont } ->
    assert (readed <= max_width);
    if readed = hard_limit
    then (
      let error = "chunkreader requests more bytes but hard limit was reached" in
      Failed { error; state })
    else (
      let maximum_size = state.maximum_size - Src.gotten state.source in
      let stop_hints =
        List.map (fun stop -> stop - Src.gotten state.source) state.stop_hints
      in
      let size_limits =
        List.map (fun limit -> limit - Src.gotten state.source) state.size_limits
      in
      assert (check_stops_and_limits 0 stop_hints maximum_size);
      assert (check_stops_and_limits 0 size_limits maximum_size);
      Suspended
        { state
        ; cont =
            (fun source ->
              let state = internal_mk_state maximum_size size_limits stop_hints source in
              readchunked state cont)
        })
  | CFailed { readed = _; error } ->
    let error = "Error in chunk-reader: " ^ error in
    Failed { state; error }
;;

(* TODO: have the user pass the buffer for [read_bytes] to avoid major
   allocation. *)

let read_bytes state len =
  let scratch = Bytes.make len '\000' in
  let* (), state = readcopy scratch state in
  Readed { value = scratch; state }
;;

let read_string state len =
  let scratch = Bytes.make len '\000' in
  let* (), state = readcopy scratch state in
  Readed { value = Bytes.unsafe_to_string scratch; state }
;;

let%expect_test _ =
  let pp_state fmt state =
    Format.fprintf
      fmt
      "source: %S, readed: %d, stops: [%a], maxlen: %d"
      (Src.to_string state.source)
      (Src.gotten state.source)
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ',')
         Format.pp_print_int)
      state.stop_hints
      state.maximum_size
  in
  let w state ls =
    let state = state () in
    Format.printf "State: %a\n%!" pp_state state;
    match
      List.fold_left
        (fun state l ->
          match read_string state l with
          | Suspended _ ->
            Format.printf "Suspended!\n%!";
            raise Exit
          | Failed { error; state } ->
            Format.printf "Error: %S\nState: %a\n%!" error pp_state state;
            state
          | Readed { value; state } ->
            Format.printf "Ok: %S\nState: %a\n%!" value pp_state state;
            state)
        state
        ls
    with
    | exception Exit -> ()
    | _ -> ()
  in
  let state () = mk_state (Src.of_string "foobarbaz") in
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
  let state () = mk_state ~maximum_size:6 (Src.of_string "foobarbaz") in
  w state [ 3; 0; 6; 3; 1 ];
  [%expect
    {|
    State: source: "foobarbaz", readed: 0, stops: [], maxlen: 6
    Ok: "foo"
    State: source: "foobarbaz", readed: 3, stops: [], maxlen: 6
    Ok: ""
    State: source: "foobarbaz", readed: 3, stops: [], maxlen: 6
    Error: "maximum-size exceeded"
    State: source: "foobarbaz", readed: 3, stops: [], maxlen: 6
    Ok: "bar"
    State: source: "foobarbaz", readed: 6, stops: [], maxlen: 6
    Error: "maximum-size exceeded"
    State: source: "foobarbaz", readed: 6, stops: [], maxlen: 6 |}];
  ();
  let w ss ls =
    let rec go ss cont =
      match cont () with
      | Suspended { state; cont } ->
        Format.printf "Suspended!\nState: %a\n%!" pp_state state;
        let s = List.hd ss in
        let ss = List.tl ss in
        go ss (fun () -> cont (Src.of_string s))
      | Failed { error; state } as failed ->
        Format.printf "Error: %S\nState: %a\n%!" error pp_state state;
        failed, state, ss
      | Readed { value; state } as readed ->
        Format.printf "Ok: %S\nState: %a\n%!" value pp_state state;
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
    Format.printf "Start!\nState: %a\n%!" pp_state state;
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
        "%a\n%!"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_char fmt '.')
           (fun fmt u -> Format.fprintf fmt "x%x" (Uchar.to_int u)))
        uchars
    | Error error -> Format.printf "Error %s\n%!" error
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
let read_int8 state = readf state 1 Src.get_int8
let read_uint16_be state = readf state 2 Src.get_uint16_be
let read_uint16_le state = readf state 2 Src.get_uint16_le
let read_int16_be state = readf state 2 Src.get_int16_be
let read_int16_le state = readf state 2 Src.get_int16_le
let read_int32_be state = readf state 4 Src.get_int32_be
let read_int32_le state = readf state 4 Src.get_int32_le
let read_int64_be state = readf state 8 Src.get_int64_be
let read_int64_le state = readf state 8 Src.get_int64_le

let of_string read s =
  let state = mk_state (Src.of_string s) in
  match read state with
  | Readed { state; value } ->
    if Src.available state.source > 0 then Error "Too many bytes" else Ok value
  | Failed { state = _; error } -> Error error
  | Suspended { state = _; cont = _ } -> Error "Not enough bytes"
;;

let rec of_string_seq_loop seq cont =
  match seq () with
  | Seq.Nil -> Error "Not enough chunks or bytes"
  | Seq.Cons (s, seq) ->
    (match cont (Src.of_string s) with
     | Readed { state; value } ->
       if Src.available state.source > 0
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
