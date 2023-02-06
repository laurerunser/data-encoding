type source =
  { blob : string
  ; offset : int
  ; length : int
  ; readed : int (* [read] is ambiguous so we make it unambiguously past as [readed] *)
  ; stop_at_readed : int list
        (* this list is grown when there is a size-header in the encoded binary data *)
  ; maximum_length : int
  }

let mk_source ?(maximum_length = max_int) ?(stop_at_readed = []) blob offset length =
  if offset < 0 then failwith "Binary_data_encoding.Backend.mk_source: negative offset";
  if length < 0 then failwith "Binary_data_encoding.Backend.mk_source: negative length";
  if offset + length > String.length blob
  then failwith "Binary_data_encoding.Backend.mk_source: offset+length overflow";
  { blob; offset; length; readed = 0; stop_at_readed; maximum_length }
;;

let bump_readed source reading = { source with readed = source.readed + reading }

let push_stop source length =
  assert (length >= 0);
  if source.readed + length > source.maximum_length
  then Error "expected-stop exceeds maximum-length"
  else (
    let requested_stop = source.readed + length in
    match source.stop_at_readed with
    | [] -> Ok { source with stop_at_readed = [ requested_stop ] }
    | previously_requested_stop :: _ ->
      if requested_stop > previously_requested_stop
      then Error "expected-stop exceeds previously requested stop"
      else Ok { source with stop_at_readed = requested_stop :: source.stop_at_readed })
;;

let pop_stop source =
  assert (source.stop_at_readed <> []);
  match source.stop_at_readed with
  | [] -> assert false
  | stop :: stop_at_readed -> stop, { source with stop_at_readed }
;;

type 'a readed =
  | Readed of
      { source : source
      ; value : 'a
      }
  | Failed of
      { source : source
      ; error : string
      }
  | Suspended of
      { source : source
      ; cont : string -> int -> int -> 'a readed
      }

let source_too_small_to_continue_message = "new source blob is too small to continue"

let read1 source reading read =
  assert (reading >= 0);
  if source.readed + reading > source.maximum_length
  then Failed { source; error = "maximum-length exceeded" }
  else if match source.stop_at_readed with
          | [] -> false
          | stop :: _ -> source.readed + reading > stop
  then Failed { source; error = "expected-stop point exceeded" }
  else if source.readed + reading > source.length
  then
    Suspended
      { source
      ; cont =
          (fun blob offset length ->
            assert (source.readed <= source.length);
            if source.readed = source.length
            then (
              let source =
                mk_source
                  ~maximum_length:source.maximum_length
                  ~stop_at_readed:source.stop_at_readed
                  blob
                  offset
                  length
              in
              if reading > source.length
                 (* TODO: instead of failing here (and below), allow to continue
                 after more feeding, possibly by concatenating bigger and bigger
                 blobs until the value is readable *)
              then Failed { source; error = source_too_small_to_continue_message }
              else (
                let value = read source.blob source.offset in
                let source = bump_readed source reading in
                Readed { source; value }))
            else (
              assert (source.readed < source.length);
              (* First check that the current here small read1 has enough data *)
              let available_length = source.length - source.readed + length in
              if reading > available_length
              then Failed { source; error = source_too_small_to_continue_message }
              else (
                (* prepare for this small here read1 *)
                let source =
                  let blob =
                    String.sub
                      source.blob
                      (source.offset + source.readed)
                      (source.length - source.readed)
                    ^ String.sub blob offset (reading - (source.length - source.readed))
                  in
                  let offset = 0 in
                  let length = reading in
                  mk_source
                    ~maximum_length:source.maximum_length
                    ~stop_at_readed:source.stop_at_readed
                    blob
                    offset
                    length
                in
                (* actually do this small here read *)
                let value = read source.blob source.offset in
                let source = bump_readed source reading in
                assert (source.readed = source.length);
                (* Second prepare the source for giving back *)
                let source =
                  mk_source
                    ~maximum_length:source.maximum_length
                    ~stop_at_readed:source.stop_at_readed
                    blob
                    offset
                    length
                in
                (* delta is the part of the new blob that has already been
                         read by the actual small read above *)
                let delta = reading - (source.length - source.readed) in
                assert (source.readed = 0);
                let source = { source with readed = delta } in
                Readed { source; value })))
      }
  else (
    let value = read source.blob (source.offset + source.readed) in
    let source = bump_readed source reading in
    Readed { source; value })
;;

type 'a chunkreader = string -> int -> int -> 'a chunkreaded

and 'a chunkreaded =
  | K of int * 'a chunkreader
  | Finish of 'a * int

let rec readchunked source read =
  let reading =
    min (source.maximum_length - source.readed) (source.length - source.readed)
  in
  assert (reading >= 0);
  match read source.blob (source.offset + source.readed) reading with
  | Finish (value, readed) ->
    let source = bump_readed source readed in
    Readed { source; value }
  | K (readed, read) ->
    let source = bump_readed source readed in
    Suspended
      { source
      ; cont =
          (fun blob offset length ->
            let source =
              mk_source
                ~maximum_length:(source.maximum_length - source.readed)
                blob
                offset
                length
            in
            readchunked source read)
      }
;;

let rec ( let* ) x f =
  match x with
  | Readed { source; value } -> f (value, source)
  | Failed { source; error } -> Failed { source; error }
  | Suspended { source; cont } ->
    let cont blob offset length =
      let* x = cont blob offset length in
      f x
    in
    Suspended { source; cont }
;;

let rec readk : type a. source -> a Descr.t -> a readed =
 fun source encoding ->
  assert (source.offset >= 0);
  assert (source.offset < String.length source.blob);
  match encoding with
  | Unit -> Readed { source; value = () }
  | Bool ->
    let* v, source = read1 source Size.bool Commons.Sizedints.Uint8.get in
    if v = Magic.bool_true
    then Readed { source; value = true }
    else if v = Magic.bool_false
    then Readed { source; value = false }
    else Failed { source; error = "Unknown value for Bool" }
  | Numeral { numeral; endianness } -> read_numeral source numeral endianness
  | String n ->
    (* TODO: support chunk reading of string so that it's possible to deserialise
     a big blob from several small blobs *)
    (* TODO: support for 32-bit machines: don't use `Int63.to_int`, maybe write1
       should take an int63? *)
    let size = Optint.Int63.to_int (n :> Optint.Int63.t) in
    let dest = Bytes.make size '\000' in
    let rec chunkreader dest_offset blob offset maxreadsize =
      let needsreading = size - dest_offset in
      if needsreading = 0
      then Finish (Bytes.unsafe_to_string dest, 0)
      else if needsreading <= maxreadsize
      then (
        Bytes.blit_string blob offset dest dest_offset needsreading;
        Finish (Bytes.unsafe_to_string dest, needsreading))
      else (
        Bytes.blit_string blob offset dest dest_offset maxreadsize;
        K (maxreadsize, chunkreader maxreadsize))
    in
    readchunked source (chunkreader 0)
  | Bytes n ->
    (* TODO: support chunk reading of bytes so that it's possible to deserialise
     a big blob from several small blobs *)
    (* TODO: support for 32-bit machines: don't use `Int63.to_int`, maybe write1
       should take an int63? *)
    let size = Optint.Int63.to_int (n :> Optint.Int63.t) in
    let dest = Bytes.make size '\000' in
    let rec chunkreader dest_offset blob offset maxreadsize =
      let needsreading = size - dest_offset in
      if needsreading = 0
      then Finish (dest, 0)
      else if needsreading <= maxreadsize
      then (
        Bytes.blit_string blob offset dest dest_offset needsreading;
        Finish (dest, needsreading))
      else (
        Bytes.blit_string blob offset dest dest_offset maxreadsize;
        K (maxreadsize, chunkreader maxreadsize))
    in
    readchunked source (chunkreader 0)
  | LSeq { length; elementencoding } ->
    let intlength = Optint.Int63.to_int (length :> Optint.Int63.t) in
    let rec fold source reversed_list remaining_length =
      if remaining_length = 0
      then (
        let seq = List.to_seq (List.rev reversed_list) in
        let value = { Descr.seq; length = lazy length } in
        Readed { source; value })
      else
        let* v, source = readk source elementencoding in
        fold source (v :: reversed_list) (remaining_length - 1)
    in
    fold source [] intlength
  | USeq { elementencoding } ->
    (match source.stop_at_readed with
    | [] ->
      (* TODO: support unsized USeq once we support lazy useq *)
      Failed { source; error = "unlengthed-seq without a size" }
    | expected_stop :: _ ->
      let rec fold source reversed_list =
        if expected_stop = source.readed
        then Readed { source; value = List.to_seq (List.rev reversed_list) }
        else
          let* v, source = readk source elementencoding in
          fold source (v :: reversed_list)
      in
      fold source [])
  | Array { length; elementencoding } ->
    if Optint.Int63.equal (length :> Optint.Int63.t) Optint.Int63.zero
    then Readed { source; value = [||] }
    else (
      let length =
        (* TODO: check for overflow *)
        Optint.Int63.to_int (length :> Optint.Int63.t)
      in
      let* v, source = readk source elementencoding in
      let array = Array.make length v in
      let rec fold source index =
        if index >= length
        then Readed { source; value = array }
        else
          let* v, source = readk source elementencoding in
          Array.set array index v;
          fold source (index + 1)
      in
      fold source 1)
  | Option t ->
    let* tag, source = read1 source Size.uint8 Commons.Sizedints.Uint8.get in
    if tag = Magic.option_none_tag
    then Readed { source; value = None }
    else if tag = Magic.option_some_tag
    then
      let* v, source = readk source t in
      Readed { source; value = Some v }
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
      | Finish value -> Readed { source; value }
    in
    reduce readinit source
  | Conv { serialisation = _; deserialisation; encoding } ->
    let* v, source = readk source encoding in
    (match deserialisation v with
    | Ok value -> Readed { source; value }
    | Error error -> Failed { source; error })
  | Size_headered { size; encoding } ->
    let* readed_size, source = read_numeral source size Encoding.default_endianness in
    let readed_size = Query.int_of_numeral size readed_size in
    assert (readed_size >= 0);
    (match push_stop source readed_size with
    | Ok source ->
      let* v, source = readk source encoding in
      let expected_stop, source = pop_stop source in
      if source.readed = expected_stop
      then Readed { source; value = v }
      else Failed { source; error = "read fewer bytes than expected-length" }
    | Error msg ->
      (* TODO: context of error message*)
      Failed { source; error = msg })
  | Size_limit { at_most; encoding } ->
    let maximum_length =
      (* TODO: handle overflow *)
      min source.maximum_length (Optint.Int63.to_int (at_most :> Optint.Int63.t))
    in
    let source = { source with maximum_length } in
    readk source encoding
  | [] -> Readed { source; value = [] }
  | t :: ts ->
    let* v, source = readk source t in
    let* vs, source = readk source ts in
    Readed { source; value = Descr.Hlist.( :: ) (v, vs) }

and read_numeral : type n. source -> n Descr.numeral -> Descr.endianness -> n readed =
 fun source numeral endianness ->
  match numeral, endianness with
  | Int64, Big_endian -> read1 source Size.int64 String.get_int64_be
  | Int64, Little_endian -> read1 source Size.int64 String.get_int64_le
  | Int32, Big_endian -> read1 source Size.int32 String.get_int32_be
  | Int32, Little_endian -> read1 source Size.int32 String.get_int32_le
  | UInt62, Big_endian -> read1 source Size.uint62 Commons.Sizedints.Uint62.get_be
  | UInt62, Little_endian -> read1 source Size.uint62 Commons.Sizedints.Uint62.get_le
  | UInt30, Big_endian -> read1 source Size.uint30 Commons.Sizedints.Uint30.get_be
  | UInt30, Little_endian -> read1 source Size.uint30 Commons.Sizedints.Uint30.get_le
  | UInt16, Big_endian -> read1 source Size.uint16 Commons.Sizedints.Uint16.get_be
  | UInt16, Little_endian -> read1 source Size.uint16 Commons.Sizedints.Uint16.get_le
  | UInt8, _ -> read1 source Size.uint8 Commons.Sizedints.Uint8.get
;;

let readk : type a. source -> a Descr.t -> a readed =
 fun source encoding ->
  if source.offset < 0
  then Failed { source; error = "offset is negative" }
  else if source.offset + source.length > String.length source.blob
  then Failed { source; error = "length exceeds buffer size" }
  else readk source encoding
;;

let read_strings : type a. (string * int * int) Seq.t -> a Descr.t -> (a, string) result =
 fun sources e ->
  let rec loop (blob, offset, length) sources k =
    match k blob offset length with
    | Readed { source = _; value } -> Ok value
    | Failed { source = _; error } -> Error error
    | Suspended { source = _; cont } ->
      (match sources () with
      | Seq.Nil -> Error "not enough inputs"
      | Seq.Cons (source, sources) -> loop source sources cont)
  in
  match sources () with
  | Seq.Nil -> Error "no inputs"
  | Seq.Cons (source, sources) ->
    loop source sources (fun blob offset length ->
        let source = mk_source blob offset length in
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

let read
    : type a. src:string -> offset:int -> length:int -> a Descr.t -> (a, string) result
  =
 fun ~src ~offset ~length encoding ->
  let source = mk_source ~maximum_length:length src offset length in
  match readk source encoding with
  | Readed { source = _; value } -> Ok value
  | Failed { source = _; error } -> Error error
  | Suspended _ -> Error "one-shot reading does not support suspension"
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