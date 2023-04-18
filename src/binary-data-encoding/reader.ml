[@@@landmark "auto"]

let ( let* ) = Buffy.R.( let* )

(* TODO: in order to read an extrinsic descr we must be provided with an expected stop. *)

let rec readk : type s a. (s, a) Descr.t -> Buffy.R.state -> a Buffy.R.readed = function
  | Unit -> fun state -> Buffy.R.Readed { state; value = () }
  | Bool ->
    fun state ->
      let* v, state = read_uint8_tag state in
      if v = Magic.bool_true
      then Buffy.R.Readed { state; value = true }
      else if v = Magic.bool_false
      then Buffy.R.Readed { state; value = false }
      else Failed { state; error = "Unknown value for Bool" }
  | Numeral { numeral; endianness } -> read_numeral numeral endianness
  | String n ->
    let size = Optint.Int63.to_int (n :> Optint.Int63.t) in
    fun state ->
      (* TODO: support for 32-bit machines: don't use `Int63.to_int`, maybe writef
       should take an int63? *)
      Buffy.R.read_string state size
  | Bytes n ->
    let size = Optint.Int63.to_int (n :> Optint.Int63.t) in
    fun state ->
      (* TODO: support for 32-bit machines: don't use `Int63.to_int`, maybe writef
       should take an int63? *)
      Buffy.R.read_bytes state size
  | LSeq { length; descr } ->
    let readelt = readk descr in
    let intlength = Optint.Int63.to_int (length :> Optint.Int63.t) in
    fun state ->
      let rec fold state reversed_list remaining_length =
        if remaining_length = 0
        then (
          let seq = List.to_seq (List.rev reversed_list) in
          let value = { Descr.seq; length = lazy length } in
          Buffy.R.Readed { state; value })
        else
          let* v, state = readelt state in
          fold state (v :: reversed_list) (remaining_length - 1)
      in
      fold state [] intlength
  | USeq { descr } ->
    let readelt = readk descr in
    fun state ->
      let rec fold (state : Buffy.R.state) reversed_list =
        (* we peek-stop at every iteration in case there was a suspension and
            it has been updated. *)
        (* TODO? don't use let* and instead check for suspension directly? *)
        match Buffy.R.peek_stop state with
        | None ->
          (* TODO: support unsized USeq once we support lazy useq *)
          Buffy.R.Failed { state; error = "unlengthed-seq without a size" }
        | Some expected_stop ->
          assert (Buffy.R.readed state <= expected_stop);
          (* reading fails otherwise *)
          if expected_stop = Buffy.R.readed state
          then Buffy.R.Readed { state; value = List.to_seq (List.rev reversed_list) }
          else
            let* v, state = readelt state in
            fold state (v :: reversed_list)
      in
      fold state []
  | Array { length; descr } ->
    let readelt = readk descr in
    if Optint.Int63.equal (length :> Optint.Int63.t) Optint.Int63.zero
    then fun state -> Buffy.R.Readed { state; value = [||] }
    else (
      let length =
        (* TODO: check for overflow *)
        Optint.Int63.to_int (length :> Optint.Int63.t)
      in
      fun state ->
        let* v, state = readelt state in
        let array = Array.make length v in
        let rec fold state index =
          if index >= length
          then Buffy.R.Readed { state; value = array }
          else
            let* v, state = readelt state in
            Array.set array index v;
            fold state (index + 1)
        in
        fold state 1)
  | Option { optioner = _; descr } ->
    let readt = readk descr in
    fun state ->
      let* tag, state = read_uint8_tag state in
      if tag = Magic.option_none_tag
      then Buffy.R.Readed { state; value = None }
      else if tag = Magic.option_some_tag
      then
        let* v, state = readt state in
        Buffy.R.Readed { state; value = Some v }
      else Failed { state; error = "Unknown tag for Option" }
  | Headered
      { mkheader = _
      ; headerdescr
      ; writers = _
      ; descr_of_header
      ; equal = _
      ; maximum_size = _
      } ->
    let readheader = readk headerdescr in
    fun state ->
      let* header, state = readheader state in
      (match descr_of_header header with
       | Error msg ->
         let error = "error in user-provided encoding function: " ^ msg in
         Failed { state; error }
       | Ok (EDynamic descr) -> readk descr state
       | Ok (EStatic descr) -> readk descr state)
  | Fold { chunkdescr; chunkify = _; readinit; reducer; equal = _; maximum_size = _ } ->
    let readchunk = readk chunkdescr in
    fun state ->
      let rec reduce acc state =
        let* chunk, state = readchunk state in
        match reducer acc chunk with
        | K acc -> reduce acc state
        | Finish value -> Buffy.R.Readed { state; value }
      in
      reduce readinit state
  | Conv { serialisation = _; deserialisation; descr } ->
    let readt = readk descr in
    fun state ->
      let* v, state = readt state in
      (match deserialisation v with
       | Ok value -> Buffy.R.Readed { state; value }
       | Error error -> Failed { state; error })
  | Size_headered { size; descr } ->
    let readsize = read_numeral size Encoding.default_endianness in
    let readt = readk descr in
    fun state ->
      let* readed_size, state = readsize state in
      let readed_size = Query.int_of_numeral size readed_size in
      assert (readed_size >= 0);
      (match Buffy.R.push_stop state readed_size with
       | Ok state ->
         let* v, state = readt state in
         (match Buffy.R.pop_stop state with
          | Ok (expected_stop, state) ->
            if Buffy.R.readed state = expected_stop
            then Buffy.R.Readed { state; value = v }
            else Failed { state; error = "read fewer bytes than expected-length" }
          | Error error -> Failed { state; error })
       | Error msg ->
         (* TODO: context of error message*)
         Failed { state; error = msg })
  | Size_limit { at_most; descr } ->
    let readt = readk descr in
    let requested_size_limit =
      (* TODO: support 32bit plateforms *)
      Optint.Int63.to_int (at_most :> Optint.Int63.t)
    in
    fun state ->
      (* Because the constructors are nested, the effective size-limit may be
       lower than that provided in the constructor. We compute the largest
       possible size-limit. *)
      let possible_size_limit = state.maximum_size - Buffy.R.readed state in
      let possible_size_limit =
        match state.size_limits with
        | [] -> possible_size_limit
        | previous_limit :: _ ->
          min (previous_limit - Buffy.R.readed state) possible_size_limit
      in
      let possible_size_limit =
        match state.stop_hints with
        | [] -> possible_size_limit
        | stop :: _ -> min (stop - Buffy.R.readed state) possible_size_limit
      in
      if requested_size_limit < possible_size_limit
      then (
        match Buffy.R.push_limit state requested_size_limit with
        | Error _ -> assert false (* the [min]s above prevent this *)
        | Ok state ->
          let* value, state = readt state in
          (match Buffy.R.remove_limit state with
           | Error _ ->
             assert false (* the only remove is here and it is paired with the push *)
           | Ok state -> Readed { value; state }))
      else readt state
  | Union { tag; serialisation = _; deserialisation; cases = _ } ->
    let readtag = readk tag in
    fun state ->
      let* found_tag, state = readtag state in
      (match deserialisation found_tag with
       | Ok (AnyC { tag = expected_tag; descr; write = _; inject }) ->
         (* TODO: preapplied query.equal to avoid interpretation *)
         if Query.equal_of tag found_tag expected_tag
         then
           let* payload, state = readk descr state in
           let value = inject payload in
           Buffy.R.Readed { state; value }
         else Buffy.R.Failed { state; error = "inconsistent tag in union" }
       | Error error -> Buffy.R.Failed { state; error })
  | TupNil -> fun state -> Buffy.R.Readed { state; value = [] }
  | TupCons { tupler = TAnyStatic; head; tail } ->
    let readhead = readk head in
    let readtail = readk tail in
    let (Intrinsic (Static n)) = Query.sizability tail in
    fun state ->
      (match Query.sizability head with
       | Extrinsic ->
         let state =
           Buffy.R.bring_first_stop_forward
             state
             (Optint.Int63.to_int (* catch overflow *) (n :> Optint.Int63.t))
         in
         let* v, state = readhead state in
         let* vs, state = readtail state in
         Buffy.R.Readed { state; value = Descr.Hlist.( :: ) (v, vs) }
       | Intrinsic _ ->
         let* v, state = readhead state in
         let* vs, state = readtail state in
         Buffy.R.Readed { state; value = Descr.Hlist.( :: ) (v, vs) })
  | TupCons { tupler = _; head; tail } ->
    let readhead = readk head in
    let readtail = readk tail in
    fun state ->
      let* v, state = readhead state in
      let* vs, state = readtail state in
      Buffy.R.Readed { state; value = Descr.Hlist.( :: ) (v, vs) }

and read_numeral
  : type n. n Descr.numeral -> Descr.endianness -> Buffy.R.state -> n Buffy.R.readed
  =
 fun numeral endianness ->
  match numeral, endianness with
  | Int64, Big_endian -> Buffy.R.read_int64_be
  | Int64, Little_endian -> Buffy.R.read_int64_le
  | Int32, Big_endian -> Buffy.R.read_int32_be
  | Int32, Little_endian -> Buffy.R.read_int32_le
  | Uint62, Big_endian ->
    fun state ->
      let* i64, state = Buffy.R.read_int64_be state in
      (match Commons.Sizedints.Uint62.of_int64 i64 with
       | None ->
         Failed { state; error = "Numeric range exceeded in numeral (u62) reading" }
       | Some value -> Readed { value; state })
  | Uint62, Little_endian ->
    fun state ->
      let* i64, state = Buffy.R.read_int64_le state in
      (match Commons.Sizedints.Uint62.of_int64 i64 with
       | None ->
         Failed { state; error = "Numeric range exceeded in numeral (u62) reading" }
       | Some value -> Readed { value; state })
  | Uint30, Big_endian ->
    fun state ->
      let* i32, state = Buffy.R.read_int32_be state in
      (match Commons.Sizedints.Uint30.of_int32 i32 with
       | None ->
         Failed { state; error = "Numeric range exceeded in numeral (u30) reading" }
       | Some value -> Readed { value; state })
  | Uint30, Little_endian ->
    fun state ->
      let* i32, state = Buffy.R.read_int32_le state in
      (match Commons.Sizedints.Uint30.of_int32 i32 with
       | None ->
         Failed { state; error = "Numeric range exceeded in numeral (u30) reading" }
       | Some value -> Readed { value; state })
  | Uint16, Big_endian ->
    fun state ->
      let* u16, state = Buffy.R.read_uint16_be state in
      let value = Commons.Sizedints.Uint16.unsafe_of_int u16 in
      Readed { value; state }
  | Uint16, Little_endian ->
    fun state ->
      let* u16, state = Buffy.R.read_uint16_le state in
      let value = Commons.Sizedints.Uint16.unsafe_of_int u16 in
      Readed { value; state }
  | Uint8, _ ->
    fun state ->
      let* u8, state = Buffy.R.read_uint8 state in
      let value = Commons.Sizedints.Uint8.unsafe_of_int u8 in
      Readed { value; state }

and read_uint8_tag : Buffy.R.state -> Commons.Sizedints.Uint8.t Buffy.R.readed =
 fun state ->
  Buffy.R.readf state Width.bool (fun s ->
    let v = Buffy.Src.get_uint8 s in
    Commons.Sizedints.Uint8.unsafe_of_int v)
;;

let read_strings
  : type s a. (s, a) Descr.t -> (string * int * int) Seq.t -> (a, string) result
  =
 fun e ->
  let read = readk e in
  fun sources ->
    match sources () with
    | Seq.Nil -> Error "no inputs"
    | Seq.Cons (source, sources) ->
      let rec loop (blob, offset, length) sources k =
        (* TODO: catch exception and wrap message or something like that *)
        let source = Buffy.Src.of_string blob ~offset ~length in
        match k source with
        | Buffy.R.Readed { state = _; value } -> Ok value
        | Buffy.R.Failed { state = _; error } -> Error error
        | Buffy.R.Suspended { state = _; cont } ->
          (match sources () with
           | Seq.Nil -> Error "not enough inputs"
           | Seq.Cons (state, sources) -> loop state sources cont)
      in
      loop source sources (fun source ->
        let state = Buffy.R.mk_state source in
        read state)
;;

let%expect_test _ =
  let w
    : type a.
      (string * int * int) Seq.t
      -> a Encoding.t
      -> (Format.formatter -> a -> unit)
      -> unit
    =
   fun sources e f ->
    let (E descr) = Encoding.introspect e in
    match read_strings descr sources with
    | Ok value -> Format.printf "Ok: %a" f value
    | Error error -> Format.printf "Error: %s" error
  in
  w
    (List.to_seq [ "LooooooL", 0, 8 ])
    Encoding.int64
    (fun fmt i64 -> Format.fprintf fmt "%Lx" i64);
  [%expect {|
    Ok: 4c6f6f6f6f6f6f4c |}];
  w
    (List.to_seq [ "Looo", 0, 4; "oooL", 0, 4 ])
    Encoding.int64
    (fun fmt i64 -> Format.fprintf fmt "%Lx" i64);
  [%expect {|
    Ok: 4c6f6f6f6f6f6f4c |}];
  let trip s = s, 0, String.length s in
  let split_string full_string i =
    String.sub full_string 0 i, String.sub full_string i (String.length full_string - i)
  in
  let ww full_string encoding pp =
    Seq.iter
      (fun i ->
        let l, r = split_string full_string i in
        w (List.to_seq [ trip l; trip r ]) encoding pp)
      (Seq.take 8 (Seq.ints 0))
  in
  ww
    "LooLLooL"
    Encoding.(tuple [ unit; int32; int32; unit ])
    (fun fmt [ (); l; r; () ] -> Format.fprintf fmt "%lx_%lx\n" l r);
  [%expect
    {|
    Ok: 4c6f6f4c_4c6f6f4c
    Ok: 4c6f6f4c_4c6f6f4c
    Ok: 4c6f6f4c_4c6f6f4c
    Ok: 4c6f6f4c_4c6f6f4c
    Ok: 4c6f6f4c_4c6f6f4c
    Ok: 4c6f6f4c_4c6f6f4c
    Ok: 4c6f6f4c_4c6f6f4c
    Ok: 4c6f6f4c_4c6f6f4c |}];
  let www full_string encoding pp =
    Seq.iter
      (fun i ->
        w
          (List.to_seq
             [ full_string, 0, i; full_string, i, String.length full_string - i ])
          encoding
          pp)
      (Seq.take 8 (Seq.ints 0))
  in
  www
    "LooLLooL"
    Encoding.(tuple [ unit; int32; int32; unit ])
    (fun fmt [ (); l; r; () ] -> Format.fprintf fmt "%lx_%lx\n" l r);
  [%expect
    {|
    Ok: 4c6f6f4c_4c6f6f4c
    Ok: 4c6f6f4c_4c6f6f4c
    Ok: 4c6f6f4c_4c6f6f4c
    Ok: 4c6f6f4c_4c6f6f4c
    Ok: 4c6f6f4c_4c6f6f4c
    Ok: 4c6f6f4c_4c6f6f4c
    Ok: 4c6f6f4c_4c6f6f4c
    Ok: 4c6f6f4c_4c6f6f4c |}];
  w
    Seq.(ints 0 |> take 8 |> map (fun i -> "LooLLooL", i, 1))
    Encoding.(tuple [ unit; int32; int32; unit ])
    (fun fmt [ (); l; r; () ] -> Format.fprintf fmt "%lx_%lx" l r);
  [%expect {| Ok: 4c6f6f4c_4c6f6f4c |}];
  w
    Seq.(ints 0 |> take 4 |> map (fun i -> "LooLLooL", i * 2, 2))
    Encoding.(tuple [ unit; int32; int32; unit ])
    (fun fmt [ (); l; r; () ] -> Format.fprintf fmt "%lx_%lx" l r);
  [%expect {| Ok: 4c6f6f4c_4c6f6f4c |}];
  w
    (List.to_seq [ "FOO", 0, 3 ])
    Encoding.(string (`Fixed (Option.get @@ Commons.Sizedints.Uint62.of_int64 3L)))
    Format.pp_print_string;
  [%expect {|
    Ok: FOO |}];
  w
    (List.to_seq [ "\000\000\000\000\000\000\000\000", 0, 8 ])
    Encoding.uint62
    (fun fmt u62 -> Format.fprintf fmt "%a" Optint.Int63.pp (u62 :> Optint.Int63.t));
  [%expect {|
    Ok: 0 |}];
  w
    (List.to_seq [ "<ooooooL", 0, 8 ])
    Encoding.uint62
    (fun fmt u62 -> Format.fprintf fmt "%a" Optint.Int63.pp (u62 :> Optint.Int63.t));
  [%expect {|
    Ok: 4354821889092185932 |}];
  w
    (List.to_seq [ "?\255\255\255\255\255\255\255", 0, 8 ])
    Encoding.uint62
    (fun fmt u62 -> Format.fprintf fmt "%a" Optint.Int63.pp (u62 :> Optint.Int63.t));
  [%expect {|
    Ok: 4611686018427387903 |}];
  w
    (List.to_seq [ "LooooooL", 0, 8 ])
    Encoding.(with_size_limit 0 int64)
    (fun fmt i64 -> Format.fprintf fmt "%Lx" i64);
  [%expect {|
    Error: size-limit exceeded |}];
  w
    (List.to_seq [ "LooooooL", 0, 8 ])
    Encoding.(with_size_limit 2 int64)
    (fun fmt i64 -> Format.fprintf fmt "%Lx" i64);
  [%expect {|
    Error: size-limit exceeded |}];
  w
    (List.to_seq [ "LooooooL", 0, 8 ])
    Encoding.(with_size_limit 8 int64)
    (fun fmt i64 -> Format.fprintf fmt "%Lx" i64);
  [%expect {|
    Ok: 4c6f6f6f6f6f6f4c |}];
  w
    (List.to_seq [ "LooooooL", 0, 8 ])
    Encoding.(with_size_limit max_int int64)
    (fun fmt i64 -> Format.fprintf fmt "%Lx" i64);
  [%expect {|
    Ok: 4c6f6f6f6f6f6f4c |}];
  w
    (List.to_seq [ "LooooooL", 0, 8 ])
    Encoding.(with_size_limit 100 (with_size_limit 1000 int64))
    (fun fmt i64 -> Format.fprintf fmt "%Lx" i64);
  [%expect {|
    Ok: 4c6f6f6f6f6f6f4c |}];
  w
    (List.to_seq [ "LooooooL", 0, 8 ])
    Encoding.(with_size_limit 1000 (with_size_limit 100 int64))
    (fun fmt i64 -> Format.fprintf fmt "%Lx" i64);
  [%expect {|
    Ok: 4c6f6f6f6f6f6f4c |}];
  w
    (List.to_seq [ "LooooooL", 0, 8 ])
    Encoding.(with_size_limit 4 (with_size_limit 8 int64))
    (fun fmt i64 -> Format.fprintf fmt "%Lx" i64);
  [%expect {|
    Error: size-limit exceeded |}];
  w
    (List.to_seq [ "LooooooL", 0, 8 ])
    Encoding.(with_size_limit 8 (with_size_limit 4 int64))
    (fun fmt i64 -> Format.fprintf fmt "%Lx" i64);
  [%expect {|
    Error: size-limit exceeded |}];
  ()
;;

let read_string : type s a. (s, a) Descr.t -> string -> (a, string) result =
 fun e ->
  let read_strings_e = read_strings e in
  fun s -> read_strings_e (Seq.return (s, 0, String.length s))
;;

let read
  : type s a.
    (s, a) Descr.t -> src:string -> offset:int -> length:int -> (a, string) result
  =
 fun descr ->
  let read = readk descr in
  fun ~src ~offset ~length ->
    let source =
      Buffy.R.mk_state ~maximum_size:length (Buffy.Src.of_string src ~offset ~length)
    in
    match read source with
    | Buffy.R.Readed { state = _; value } -> Ok value
    | Buffy.R.Failed { state = _; error } -> Error error
    | Buffy.R.Suspended _ ->
      (* it would be [Failed] because [maximum_length] takes priority over
         suspensions. *)
      assert false
;;
