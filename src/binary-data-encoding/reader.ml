let ( let* ) = Buffy.R.( let* )

let rec readk : type a. Buffy.R.source -> a Descr.t -> a Buffy.R.readed =
 fun source encoding ->
  assert (source.offset >= 0);
  assert (source.offset < String.length source.blob);
  match encoding with
  | Unit -> Buffy.R.Readed { source; value = () }
  | Bool ->
    let* v, source = Buffy.R.readf source Size.bool Commons.Sizedints.Uint8.get in
    if v = Magic.bool_true
    then Buffy.R.Readed { source; value = true }
    else if v = Magic.bool_false
    then Buffy.R.Readed { source; value = false }
    else Failed { source; error = "Unknown value for Bool" }
  | Numeral { numeral; endianness } -> read_numeral source numeral endianness
  | String n ->
    (* TODO: support for 32-bit machines: don't use `Int63.to_int`, maybe writef
       should take an int63? *)
    let size = Optint.Int63.to_int (n :> Optint.Int63.t) in
    Buffy.R.read_large_string source size
  | Bytes n ->
    (* TODO: support for 32-bit machines: don't use `Int63.to_int`, maybe writef
       should take an int63? *)
    let size = Optint.Int63.to_int (n :> Optint.Int63.t) in
    Buffy.R.read_large_bytes source size
  | LSeq { length; elementencoding } ->
    let intlength = Optint.Int63.to_int (length :> Optint.Int63.t) in
    let rec fold source reversed_list remaining_length =
      if remaining_length = 0
      then (
        let seq = List.to_seq (List.rev reversed_list) in
        let value = { Descr.seq; length = lazy length } in
        Buffy.R.Readed { source; value })
      else
        let* v, source = readk source elementencoding in
        fold source (v :: reversed_list) (remaining_length - 1)
    in
    fold source [] intlength
  | USeq { elementencoding } ->
    (match Buffy.R.peak_stop source with
     | None ->
       (* TODO: support unsized USeq once we support lazy useq *)
       Failed { source; error = "unlengthed-seq without a size" }
     | Some expected_stop ->
       let rec fold (source : Buffy.R.source) reversed_list =
         assert (source.offset + source.readed <= expected_stop);
         (* reading fails otherwise *)
         if expected_stop = source.readed
         then Buffy.R.Readed { source; value = List.to_seq (List.rev reversed_list) }
         else
           (* TODO: in case of suspend, we have to recompute the expected_stop,
             otherwise the expected stop is wrong, this is a bug *)
           let* v, source = readk source elementencoding in
           fold source (v :: reversed_list)
       in
       fold source [])
  | Array { length; elementencoding } ->
    if Optint.Int63.equal (length :> Optint.Int63.t) Optint.Int63.zero
    then Buffy.R.Readed { source; value = [||] }
    else (
      let length =
        (* TODO: check for overflow *)
        Optint.Int63.to_int (length :> Optint.Int63.t)
      in
      let* v, source = readk source elementencoding in
      let array = Array.make length v in
      let rec fold source index =
        if index >= length
        then Buffy.R.Readed { source; value = array }
        else
          let* v, source = readk source elementencoding in
          Array.set array index v;
          fold source (index + 1)
      in
      fold source 1)
  | Option t ->
    let* tag, source = Buffy.R.readf source Size.uint8 Commons.Sizedints.Uint8.get in
    if tag = Magic.option_none_tag
    then Buffy.R.Readed { source; value = None }
    else if tag = Magic.option_some_tag
    then
      let* v, source = readk source t in
      Buffy.R.Readed { source; value = Some v }
    else Failed { source; error = "Unknown tag for Option" }
  | Headered { mkheader = _; headerencoding; mkencoding; equal = _; maximum_size = _ } ->
    let* header, source = readk source headerencoding in
    (match mkencoding header with
     | Error msg ->
       let error = "error in user-provided encoding function: " ^ msg in
       Failed { source; error }
     | Ok encoding -> readk source encoding)
  | Fold { chunkencoding; chunkify = _; readinit; reducer; equal = _; maximum_size = _ }
    ->
    let rec reduce acc source =
      let* chunk, source = readk source chunkencoding in
      match reducer acc chunk with
      | K acc -> reduce acc source
      | Finish value -> Buffy.R.Readed { source; value }
    in
    reduce readinit source
  | Conv { serialisation = _; deserialisation; encoding } ->
    let* v, source = readk source encoding in
    (match deserialisation v with
     | Ok value -> Buffy.R.Readed { source; value }
     | Error error -> Failed { source; error })
  | Size_headered { size; encoding } ->
    let* readed_size, source = read_numeral source size Encoding.default_endianness in
    let readed_size = Query.int_of_numeral size readed_size in
    assert (readed_size >= 0);
    (match Buffy.R.push_stop source readed_size with
     | Ok source ->
       let* v, source = readk source encoding in
       (match Buffy.R.pop_stop source with
        | Ok (expected_stop, source) ->
          if source.readed = expected_stop
          then Buffy.R.Readed { source; value = v }
          else Failed { source; error = "read fewer bytes than expected-length" }
        | Error error -> Failed { source; error })
     | Error msg ->
       (* TODO: context of error message*)
       Failed { source; error = msg })
  | Size_limit { at_most; encoding } ->
    let maximum_length =
      (* TODO: handle overflow *)
      min source.maximum_length (Optint.Int63.to_int (at_most :> Optint.Int63.t))
    in
    let source = Buffy.R.set_maximum_length source maximum_length in
    readk source encoding
  | Union { tag; serialisation = _; deserialisation; cases = _ } ->
    let* found_tag, source = readk source tag in
    (match deserialisation found_tag with
     | Ok (AnyC { tag = expected_tag; encoding; inject }) ->
       if Query.equal_of tag found_tag expected_tag
       then
         let* payload, source = readk source encoding in
         let value = inject payload in
         Buffy.R.Readed { source; value }
       else Buffy.R.Failed { source; error = "inconsistent tag in union" }
     | Error error -> Buffy.R.Failed { source; error })
  | [] -> Buffy.R.Readed { source; value = [] }
  | t :: ts ->
    let* v, source = readk source t in
    let* vs, source = readk source ts in
    Buffy.R.Readed { source; value = Descr.Hlist.( :: ) (v, vs) }

and read_numeral
  : type n. Buffy.R.source -> n Descr.numeral -> Descr.endianness -> n Buffy.R.readed
  =
 fun source numeral endianness ->
  match numeral, endianness with
  | Int64, Big_endian -> Buffy.R.readf source Size.int64 String.get_int64_be
  | Int64, Little_endian -> Buffy.R.readf source Size.int64 String.get_int64_le
  | Int32, Big_endian -> Buffy.R.readf source Size.int32 String.get_int32_be
  | Int32, Little_endian -> Buffy.R.readf source Size.int32 String.get_int32_le
  | UInt62, Big_endian -> Buffy.R.readf source Size.uint62 Commons.Sizedints.Uint62.get_be
  | UInt62, Little_endian ->
    Buffy.R.readf source Size.uint62 Commons.Sizedints.Uint62.get_le
  | UInt30, Big_endian -> Buffy.R.readf source Size.uint30 Commons.Sizedints.Uint30.get_be
  | UInt30, Little_endian ->
    Buffy.R.readf source Size.uint30 Commons.Sizedints.Uint30.get_le
  | UInt16, Big_endian -> Buffy.R.readf source Size.uint16 Commons.Sizedints.Uint16.get_be
  | UInt16, Little_endian ->
    Buffy.R.readf source Size.uint16 Commons.Sizedints.Uint16.get_le
  | UInt8, _ -> Buffy.R.readf source Size.uint8 Commons.Sizedints.Uint8.get
;;

let readk : type a. Buffy.R.source -> a Descr.t -> a Buffy.R.readed =
 fun source encoding ->
  if source.offset < 0
  then Buffy.R.Failed { source; error = "offset is negative" }
  else if source.offset + source.length > String.length source.blob
  then Buffy.R.Failed { source; error = "length exceeds buffer size" }
  else readk source encoding
;;

let read_strings : type a. (string * int * int) Seq.t -> a Descr.t -> (a, string) result =
 fun sources e ->
  let rec loop (blob, offset, length) sources k =
    match k blob offset length with
    | Buffy.R.Readed { source = _; value } -> Ok value
    | Buffy.R.Failed { source = _; error } -> Error error
    | Buffy.R.Suspended { source = _; cont } ->
      (match sources () with
       | Seq.Nil -> Error "not enough inputs"
       | Seq.Cons (source, sources) -> loop source sources cont)
  in
  match sources () with
  | Seq.Nil -> Error "no inputs"
  | Seq.Cons (source, sources) ->
    loop source sources (fun blob offset length ->
      let source = Buffy.R.mk_source blob offset length in
      readk source e)
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
    match read_strings sources e with
    | Ok value -> Format.printf "Ok: %a" f value
    | Error error -> Format.printf "Error: %s" error
  in
  w
    (List.to_seq [ "LooooooL", 0, 8 ])
    Encoding.int64
    (fun fmt i64 -> Format.fprintf fmt "%Lx" i64);
  [%expect {| Ok: 4c6f6f6f6f6f6f4c |}];
  w
    (List.to_seq [ "Looo", 0, 4; "oooL", 0, 4 ])
    Encoding.int64
    (fun fmt i64 -> Format.fprintf fmt "%Lx" i64);
  [%expect {| Ok: 4c6f6f6f6f6f6f4c |}];
  w
    (List.to_seq [ "LooLLooL", 0, 8 ])
    Encoding.[ unit; int32; int32; unit ]
    (fun fmt [ (); l; r; () ] -> Format.fprintf fmt "%lx_%lx" l r);
  [%expect {| Ok: 4c6f6f4c_4c6f6f4c |}];
  w
    (List.to_seq [ "LooL", 0, 4; "LooL", 0, 4 ])
    Encoding.[ unit; int32; int32; unit ]
    (fun fmt [ (); l; r; () ] -> Format.fprintf fmt "%lx_%lx" l r);
  [%expect {| Ok: 4c6f6f4c_4c6f6f4c |}];
  w
    (List.to_seq [ "LooLLo", 0, 6; "oL", 0, 2 ])
    Encoding.[ unit; int32; int32; unit ]
    (fun fmt [ (); l; r; () ] -> Format.fprintf fmt "%lx_%lx" l r);
  [%expect {| Ok: 4c6f6f4c_4c6f6f4c |}];
  w
    (List.to_seq [ "LooLLooL", 0, 4; "LooLLooL", 4, 4 ])
    Encoding.[ unit; int32; int32; unit ]
    (fun fmt [ (); l; r; () ] -> Format.fprintf fmt "%lx_%lx" l r);
  [%expect {| Ok: 4c6f6f4c_4c6f6f4c |}];
  w
    Seq.(ints 0 |> take 8 |> map (fun i -> "LooLLooL", i, 1))
    Encoding.[ unit; int32; int32; unit ]
    (fun fmt [ (); l; r; () ] -> Format.fprintf fmt "%lx_%lx" l r);
  [%expect {| Error: new source blob is too small to continue |}];
  w
    Seq.(ints 0 |> take 4 |> map (fun i -> "LooLLooL", i * 2, 2))
    Encoding.[ unit; int32; int32; unit ]
    (fun fmt [ (); l; r; () ] -> Format.fprintf fmt "%lx_%lx" l r);
  [%expect {| Error: new source blob is too small to continue |}];
  w
    (List.to_seq [ "FOO", 0, 3 ])
    Encoding.(String (Option.get @@ Commons.Sizedints.Uint62.of_int64 3L))
    Format.pp_print_string;
  [%expect {| Ok: FOO |}];
  w
    (List.to_seq [ "\000\000\000\000\000\000\000\000", 0, 8 ])
    Encoding.uint62
    (fun fmt u62 -> Format.fprintf fmt "%a" Optint.Int63.pp (u62 :> Optint.Int63.t));
  [%expect {| Ok: 0 |}];
  w
    (List.to_seq [ "<ooooooL", 0, 8 ])
    Encoding.uint62
    (fun fmt u62 -> Format.fprintf fmt "%a" Optint.Int63.pp (u62 :> Optint.Int63.t));
  [%expect {| Ok: 4354821889092185932 |}];
  w
    (List.to_seq [ "?\255\255\255\255\255\255\255", 0, 8 ])
    Encoding.uint62
    (fun fmt u62 -> Format.fprintf fmt "%a" Optint.Int63.pp (u62 :> Optint.Int63.t));
  [%expect {| Ok: 4611686018427387903 |}];
  ()
;;

let read_string : type a. string -> a Descr.t -> (a, string) result =
 fun s e -> read_strings (Seq.return (s, 0, String.length s)) e
;;

let read
  : type a. src:string -> offset:int -> length:int -> a Descr.t -> (a, string) result
  =
 fun ~src ~offset ~length encoding ->
  let source = Buffy.R.mk_source ~maximum_length:length src offset length in
  match readk source encoding with
  | Buffy.R.Readed { source = _; value } -> Ok value
  | Buffy.R.Failed { source = _; error } -> Error error
  | Buffy.R.Suspended _ -> Error "one-shot reading does not support suspension"
;;

let%expect_test _ =
  let w : type a. ?pp:(Format.formatter -> a -> unit) -> string -> a Encoding.t -> unit =
   fun ?pp blob e ->
    match read ~src:blob ~offset:0 ~length:(String.length blob) e with
    | Ok d ->
      let pp =
        match pp with
        | Some pp -> pp
        | None -> Query.pp_of e
      in
      Format.printf "Ok: %a\n" pp d
    | Error s -> Format.printf "Error: %s" s
  in
  w ~pp:(fun fmt i64 -> Format.fprintf fmt "%Lx" i64) "LooooooL" Encoding.int64;
  [%expect {| Ok: 4c6f6f6f6f6f6f4c |}];
  w "LooLLooL" Encoding.[ unit; int32; int32; unit ];
  [%expect {| Ok: ();1282371404;1282371404;() |}];
  w "FOO" Encoding.(String (Option.get @@ Commons.Sizedints.Uint62.of_int64 3L));
  [%expect {| Ok: FOO |}];
  w
    "\000\000\000\000\000\000\000\000\001\000\000\000\000"
    Encoding.[ int64; option int32 ];
  [%expect {| Ok: 0;Some(0) |}];
  let pp_ui30 fmt (u30 : Commons.Sizedints.Uint30.t) =
    Format.fprintf fmt "%x" (u30 :> int)
  in
  w ~pp:pp_ui30 "\x00" Encoding.ellastic_uint30;
  [%expect {| Ok: 0 |}];
  w ~pp:pp_ui30 "\x01" Encoding.ellastic_uint30;
  [%expect {| Ok: 1 |}];
  w ~pp:pp_ui30 "\x7f" Encoding.ellastic_uint30;
  [%expect {| Ok: 7f |}];
  w ~pp:pp_ui30 "\x80\x01" Encoding.ellastic_uint30;
  [%expect {| Ok: 80 |}];
  w ~pp:pp_ui30 "\x81\x01" Encoding.ellastic_uint30;
  [%expect {| Ok: 81 |}];
  w ~pp:pp_ui30 "\xaa\xd5\x02" Encoding.ellastic_uint30;
  [%expect {| Ok: aaaa |}];
  w
    "\x7f\xff"
    Encoding.(array (`Fixed (Option.get @@ Commons.Sizedints.Uint62.of_int64 2L)) uint8);
  [%expect {| Ok: [|127;255|] |}];
  w
    "\x7f\xff\x01"
    Encoding.(array (`Fixed (Option.get @@ Commons.Sizedints.Uint62.of_int64 3L)) uint8);
  [%expect {| Ok: [|127;255;1|] |}];
  w "\x00" Encoding.(array `UInt8 uint8);
  [%expect {| Ok: [||] |}];
  w "\x03\x7f\xff\x01" Encoding.(array `UInt8 uint8);
  [%expect {| Ok: [|127;255;1|] |}];
  w
    "\r\000\000\000\000\000\000\000\000\001\000\000\000\000"
    Encoding.(with_size_header ~sizeencoding:`UInt8 ~encoding:[ int64; option int32 ]);
  [%expect {| Ok: 0;Some(0) |}];
  w
    "\000\007\000\005THERE"
    Encoding.(with_size_header ~sizeencoding:`UInt16 ~encoding:(string `UInt16));
  [%expect {| Ok: THERE |}];
  w
    "\004\003\002\001\000"
    Encoding.(
      with_size_header
        ~sizeencoding:`UInt8
        ~encoding:
          (with_size_header
             ~sizeencoding:`UInt8
             ~encoding:
               (with_size_header
                  ~sizeencoding:`UInt8
                  ~encoding:
                    (with_size_header
                       ~sizeencoding:`UInt8
                       ~encoding:(with_size_header ~sizeencoding:`UInt8 ~encoding:unit)))));
  [%expect {| Ok: () |}];
  w "\000" Encoding.(seq_with_size `UInt8 uint8);
  [%expect {| Ok: seq() |}];
  w "\001\000" Encoding.(seq_with_size `UInt8 uint8);
  [%expect {| Ok: seq(0) |}];
  w "\006\001\000\001\001\001\002" Encoding.(seq_with_size `UInt8 (option uint8));
  [%expect {| Ok: seq(Some(0),Some(1),Some(2)) |}];
  ()
;;
