let ( let* ) = Buffy.R.( let* )

(* TODO: in order to read an extrinsic encoding we must be provided with an expected stop. *)

let rec readk : type s a. Buffy.R.state -> (s, a) Descr.t -> a Buffy.R.readed =
 fun state encoding ->
  match encoding with
  | Unit -> Buffy.R.Readed { state; value = () }
  | Bool ->
    let* v, state = read_uint8_tag state in
    if v = Magic.bool_true
    then Buffy.R.Readed { state; value = true }
    else if v = Magic.bool_false
    then Buffy.R.Readed { state; value = false }
    else Failed { state; error = "Unknown value for Bool" }
  | Numeral { numeral; endianness } -> read_numeral state numeral endianness
  | String n ->
    (* TODO: support for 32-bit machines: don't use `Int63.to_int`, maybe writef
       should take an int63? *)
    let size = Optint.Int63.to_int (n :> Optint.Int63.t) in
    Buffy.R.read_string state size
  | Bytes n ->
    (* TODO: support for 32-bit machines: don't use `Int63.to_int`, maybe writef
       should take an int63? *)
    let size = Optint.Int63.to_int (n :> Optint.Int63.t) in
    Buffy.R.read_bytes state size
  | LSeq { length; elementencoding } ->
    let intlength = Optint.Int63.to_int (length :> Optint.Int63.t) in
    let rec fold state reversed_list remaining_length =
      if remaining_length = 0
      then (
        let seq = List.to_seq (List.rev reversed_list) in
        let value = { Descr.seq; length = lazy length } in
        Buffy.R.Readed { state; value })
      else
        let* v, state = readk state elementencoding in
        fold state (v :: reversed_list) (remaining_length - 1)
    in
    fold state [] intlength
  | USeq { elementencoding } ->
    (match Buffy.R.peek_stop state with
     | None ->
       (* TODO: support unsized USeq once we support lazy useq *)
       Failed { state; error = "unlengthed-seq without a size" }
     | Some expected_stop ->
       let rec fold (state : Buffy.R.state) reversed_list =
         assert (state.readed <= expected_stop);
         (* reading fails otherwise *)
         if expected_stop = state.readed
         then Buffy.R.Readed { state; value = List.to_seq (List.rev reversed_list) }
         else
           (* TODO: in case of suspend, we have to recompute the expected_stop,
             otherwise the expected stop is wrong, this is a bug *)
           let* v, state = readk state elementencoding in
           fold state (v :: reversed_list)
       in
       fold state [])
  | Array { length; elementencoding } ->
    if Optint.Int63.equal (length :> Optint.Int63.t) Optint.Int63.zero
    then Buffy.R.Readed { state; value = [||] }
    else (
      let length =
        (* TODO: check for overflow *)
        Optint.Int63.to_int (length :> Optint.Int63.t)
      in
      let* v, state = readk state elementencoding in
      let array = Array.make length v in
      let rec fold state index =
        if index >= length
        then Buffy.R.Readed { state; value = array }
        else
          let* v, state = readk state elementencoding in
          Array.set array index v;
          fold state (index + 1)
      in
      fold state 1)
  | Option (_, t) ->
    let* tag, state = read_uint8_tag state in
    if tag = Magic.option_none_tag
    then Buffy.R.Readed { state; value = None }
    else if tag = Magic.option_some_tag
    then
      let* v, state = readk state t in
      Buffy.R.Readed { state; value = Some v }
    else Failed { state; error = "Unknown tag for Option" }
  | Headered { mkheader = _; headerencoding; mkencoding; equal = _; maximum_size = _ } ->
    let* header, state = readk state headerencoding in
    (match mkencoding header with
     | Error msg ->
       let error = "error in user-provided encoding function: " ^ msg in
       Failed { state; error }
     | Ok (EDynamic encoding) -> readk state encoding
     | Ok (EStatic encoding) -> readk state encoding)
  | Fold { chunkencoding; chunkify = _; readinit; reducer; equal = _; maximum_size = _ }
    ->
    let rec reduce acc state =
      let* chunk, state = readk state chunkencoding in
      match reducer acc chunk with
      | K acc -> reduce acc state
      | Finish value -> Buffy.R.Readed { state; value }
    in
    reduce readinit state
  | Conv { serialisation = _; deserialisation; encoding } ->
    let* v, state = readk state encoding in
    (match deserialisation v with
     | Ok value -> Buffy.R.Readed { state; value }
     | Error error -> Failed { state; error })
  | Size_headered { size; encoding } ->
    let* readed_size, state = read_numeral state size Encoding.default_endianness in
    let readed_size = Query.int_of_numeral size readed_size in
    assert (readed_size >= 0);
    (match Buffy.R.push_stop state readed_size with
     | Ok state ->
       let* v, state = readk state encoding in
       (match Buffy.R.pop_stop state with
        | Ok (expected_stop, state) ->
          if state.readed = expected_stop
          then Buffy.R.Readed { state; value = v }
          else Failed { state; error = "read fewer bytes than expected-length" }
        | Error error -> Failed { state; error })
     | Error msg ->
       (* TODO: context of error message*)
       Failed { state; error = msg })
  | Size_limit { at_most; encoding } ->
    let maximum_length =
      (* TODO: handle overflow *)
      min state.maximum_length (Optint.Int63.to_int (at_most :> Optint.Int63.t))
    in
    let state = Buffy.R.set_maximum_length state maximum_length in
    readk state encoding
  | Union { tag; serialisation = _; deserialisation; cases = _ } ->
    let* found_tag, state = readk state tag in
    (match deserialisation found_tag with
     | Ok (AnyC { tag = expected_tag; encoding; inject }) ->
       if Query.equal_of tag found_tag expected_tag
       then
         let* payload, state = readk state encoding in
         let value = inject payload in
         Buffy.R.Readed { state; value }
       else Buffy.R.Failed { state; error = "inconsistent tag in union" }
     | Error error -> Buffy.R.Failed { state; error })
  | TupNil -> Buffy.R.Readed { state; value = [] }
  | TupCons (tupler, t, ts) ->
    (match tupler with
     | TDynamicIntrinsic | TStaticIntrinsic ->
       let* v, state = readk state t in
       let* vs, state = readk state ts in
       Buffy.R.Readed { state; value = Descr.Hlist.( :: ) (v, vs) }
     | TIntrinsicExtrinsic ->
       let* v, state = readk state t in
       let* vs, state = readk state ts in
       Buffy.R.Readed { state; value = Descr.Hlist.( :: ) (v, vs) }
     | TExtrinsicStatic ->
       (* ExtrinsicStatic means that we have an extrinsically sized encoding
             on the left and then a static one on the right. This means that the
             size-header (the only constructor that can have made this viable)
             has the size of the whole tuple. We must subtract the static size
             of the rhs to realign the stop on the lhs. *)
       let (Intrinsic (Static n)) = Query.sizability ts in
       let state =
         Buffy.R.bring_first_stop_forward
           state
           (Optint.Int63.to_int (* catch overflow *) (n :> Optint.Int63.t))
       in
       let* v, state = readk state t in
       let* vs, state = readk state ts in
       Buffy.R.Readed { state; value = Descr.Hlist.( :: ) (v, vs) })

and read_numeral
  : type n. Buffy.R.state -> n Descr.numeral -> Descr.endianness -> n Buffy.R.readed
  =
 fun state numeral endianness ->
  match numeral, endianness with
  | Int64, Big_endian -> Buffy.R.readf state Size.int64 Buffy.Src.get_int64_be
  | Int64, Little_endian -> Buffy.R.readf state Size.int64 Buffy.Src.get_int64_le
  | Int32, Big_endian -> Buffy.R.readf state Size.int32 Buffy.Src.get_int32_be
  | Int32, Little_endian -> Buffy.R.readf state Size.int32 Buffy.Src.get_int32_le
  | UInt62, Big_endian ->
    let* i64, state = Buffy.R.readf state Size.int64 Buffy.Src.get_int64_be in
    (match Commons.Sizedints.Uint62.of_int64 i64 with
     | None -> Failed { state; error = "Numeric range exceeded in numeral (u62) reading" }
     | Some value -> Readed { value; state })
  | UInt62, Little_endian ->
    let* i64, state = Buffy.R.readf state Size.int64 Buffy.Src.get_int64_le in
    (match Commons.Sizedints.Uint62.of_int64 i64 with
     | None -> Failed { state; error = "Numeric range exceeded in numeral (u62) reading" }
     | Some value -> Readed { value; state })
  | UInt30, Big_endian ->
    let* i32, state = Buffy.R.readf state Size.int32 Buffy.Src.get_int32_be in
    (match Commons.Sizedints.Uint30.of_int32 i32 with
     | None -> Failed { state; error = "Numeric range exceeded in numeral (u30) reading" }
     | Some value -> Readed { value; state })
  | UInt30, Little_endian ->
    let* i32, state = Buffy.R.readf state Size.int32 Buffy.Src.get_int32_le in
    (match Commons.Sizedints.Uint30.of_int32 i32 with
     | None -> Failed { state; error = "Numeric range exceeded in numeral (u30) reading" }
     | Some value -> Readed { value; state })
  | UInt16, Big_endian ->
    let* u16, state = Buffy.R.readf state Size.uint16 Buffy.Src.get_uint16_be in
    let value = Commons.Sizedints.Uint16.unsafe_of_int u16 in
    Readed { value; state }
  | UInt16, Little_endian ->
    let* u16, state = Buffy.R.readf state Size.uint16 Buffy.Src.get_uint16_le in
    let value = Commons.Sizedints.Uint16.unsafe_of_int u16 in
    Readed { value; state }
  | UInt8, _ ->
    let* u8, state = Buffy.R.readf state Size.uint8 Buffy.Src.get_uint8 in
    let value = Commons.Sizedints.Uint8.unsafe_of_int u8 in
    Readed { value; state }

and read_uint8_tag : Buffy.R.state -> Commons.Sizedints.Uint8.t Buffy.R.readed =
 fun state ->
  Buffy.R.readf state Size.bool (fun s o ->
    let v = Buffy.Src.get_uint8 s o in
    assert (v >= 0);
    assert (v < 256);
    Commons.Sizedints.Uint8.unsafe_of_int v)
;;

let read_strings
  : type s a. (string * int * int) Seq.t -> (s, a) Descr.t -> (a, string) result
  =
 fun sources e ->
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
  match sources () with
  | Seq.Nil -> Error "no inputs"
  | Seq.Cons (source, sources) ->
    loop source sources (fun source ->
      let state = Buffy.R.mk_state source in
      readk state e)
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
    let (E descr) = Encoding.Advanced_low_level.introspect e in
    match read_strings sources descr with
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
  ()
;;

let read_string : type s a. string -> (s, a) Descr.t -> (a, string) result =
 fun s e -> read_strings (Seq.return (s, 0, String.length s)) e
;;

let read
  : type s a.
    src:string -> offset:int -> length:int -> (s, a) Descr.t -> (a, string) result
  =
 fun ~src ~offset ~length encoding ->
  let source =
    Buffy.R.mk_state ~maximum_length:length (Buffy.Src.of_string src ~offset ~length)
  in
  match readk source encoding with
  | Buffy.R.Readed { state = _; value } -> Ok value
  | Buffy.R.Failed { state = _; error } -> Error error
  | Buffy.R.Suspended _ ->
    (* it would be [Failed] because [maximum_length] takes priority over
         suspensions. *)
    assert false
;;

let read_string_e s encoding =
  let (E encoding) = Encoding.Advanced_low_level.introspect encoding in
  read_string s encoding
;;

let%expect_test _ =
  let w : type a. ?pp:(Format.formatter -> a -> unit) -> string -> a Encoding.t -> unit =
   fun ?pp blob e ->
    let (E descr) = Encoding.Advanced_low_level.introspect e in
    match read ~src:blob ~offset:0 ~length:(String.length blob) descr with
    | Ok d ->
      let pp =
        match pp with
        | Some pp -> pp
        | None -> Query.pp_of descr
      in
      Format.printf "Ok: %a\n" pp d
    | Error s -> Format.printf "Error: %s" s
  in
  w ~pp:(fun fmt i64 -> Format.fprintf fmt "%Lx" i64) "LooooooL" Encoding.int64;
  [%expect {|
    Ok: 4c6f6f6f6f6f6f4c |}];
  w "LooLLooL" Encoding.(tuple [ unit; int32; int32; unit ]);
  [%expect {|
    Ok: ();1282371404;1282371404;() |}];
  w "FOO" Encoding.(string (`Fixed (Option.get @@ Commons.Sizedints.Uint62.of_int64 3L)));
  [%expect {|
    Ok: FOO |}];
  w
    "\000\000\000\000\000\000\000\000\001\000\000\000\000"
    Encoding.(tuple [ int64; option int32 ]);
  [%expect {|
    Ok: 0;Some(0) |}];
  let pp_ui30 fmt (u30 : Commons.Sizedints.Uint30.t) =
    Format.fprintf fmt "%x" (u30 :> int)
  in
  w ~pp:pp_ui30 "\x00" Encoding.ellastic_uint30;
  [%expect {|
    Ok: 0 |}];
  w ~pp:pp_ui30 "\x01" Encoding.ellastic_uint30;
  [%expect {|
    Ok: 1 |}];
  w ~pp:pp_ui30 "\x7f" Encoding.ellastic_uint30;
  [%expect {|
    Ok: 7f |}];
  w ~pp:pp_ui30 "\x80\x01" Encoding.ellastic_uint30;
  [%expect {|
    Ok: 80 |}];
  w ~pp:pp_ui30 "\x81\x01" Encoding.ellastic_uint30;
  [%expect {|
    Ok: 81 |}];
  w ~pp:pp_ui30 "\xaa\xd5\x02" Encoding.ellastic_uint30;
  [%expect {|
    Ok: aaaa |}];
  w
    "\x7f\xff"
    Encoding.(array (`Fixed (Option.get @@ Commons.Sizedints.Uint62.of_int64 2L)) uint8);
  [%expect {|
    Ok: [|127;255|] |}];
  w
    "\x7f\xff\x01"
    Encoding.(array (`Fixed (Option.get @@ Commons.Sizedints.Uint62.of_int64 3L)) uint8);
  [%expect {|
    Ok: [|127;255;1|] |}];
  w "\x00" Encoding.(array `UInt8 uint8);
  [%expect {|
    Ok: [||] |}];
  w "\x03\x7f\xff\x01" Encoding.(array `UInt8 uint8);
  [%expect {|
    Ok: [|127;255;1|] |}];
  w "\000" Encoding.(With_size.seq_with_size `UInt8 uint8);
  [%expect {|
    Ok: seq() |}];
  w "\001\000" Encoding.(With_size.seq_with_size `UInt8 uint8);
  [%expect {|
    Ok: seq(0) |}];
  w
    "\006\001\000\001\001\001\002"
    Encoding.(With_size.seq_with_size `UInt8 (option uint8));
  [%expect {|
    Ok: seq(Some(0),Some(1),Some(2)) |}];
  ()
;;
