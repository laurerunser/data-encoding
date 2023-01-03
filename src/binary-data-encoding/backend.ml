(* TODO: more documentation *)
(* TODO: more assertion checks *)
(* TODO: benchmark and optimise (later, after we add more features) *)

type destination =
  { buffer : bytes
  ; offset : int
  ; length : int
  ; written : int
  ; maximum_length : int
  }

let mk_destination ?(maximum_length = max_int) buffer offset length =
  if offset < 0
  then failwith "Binary_data_encoding.Backend.mk_destination: negative offset";
  if length < 0
  then failwith "Binary_data_encoding.Backend.mk_destination: negative length";
  if offset + length > Bytes.length buffer
  then failwith "Binary_data_encoding.Backend.mk_destination: offset+length overflow";
  { buffer; offset; length; written = 0; maximum_length }
;;

let slice_of_destination { buffer; offset; length = _; written; maximum_length = _ } =
  buffer, offset, written
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

let rec writek : type a. destination -> a Encoding.t -> a -> written =
 fun destination encoding v ->
  assert (destination.offset >= 0);
  assert (destination.length >= 0);
  assert (destination.offset + destination.length <= Bytes.length destination.buffer);
  match encoding with
  | Unit -> Written { destination }
  | Bool ->
    write1 destination Size.bool (fun buffer offset ->
        if v
        then Endian.set_uint8 buffer offset Magic.bool_true
        else Endian.set_uint8 buffer offset Magic.bool_false)
  | Int64 ->
    write1 destination Size.int64 (fun buffer offset -> Endian.set_int64 buffer offset v)
  | Int32 ->
    write1 destination Size.int32 (fun buffer offset -> Endian.set_int32 buffer offset v)
  | UInt62 ->
    write1 destination Size.uint62 (fun buffer offset ->
        Endian.set_uint62 buffer offset v)
  | UInt30 ->
    write1 destination Size.uint30 (fun buffer offset ->
        Endian.set_uint30 buffer offset v)
  | UInt16 ->
    write1 destination Size.uint16 (fun buffer offset ->
        Endian.set_uint16 buffer offset v)
  | UInt8 ->
    write1 destination Size.uint8 (fun buffer offset -> Endian.set_uint8 buffer offset v)
  | String n ->
    (* TODO: support chunk writing of strings so that it's possible to serialise
     a big blob onto several small buffers *)
    (* TODO: encoding-specific failure modes could be defined in the Encoding
       module maybe? They are related to encodings more than the serialisation
       process. *)
    (* TODO: support for 32-bit machines: don't use `Int63.to_int`, maybe write1
       should take an int63? *)
    if Optint.Int63.compare (Optint.Int63.of_int (String.length v)) (n :> Optint.Int63.t)
       <> 0
    then Failed { destination; error = "inconsistent length of string" }
    else (
      let size = Optint.Int63.to_int (n :> Optint.Int63.t) in
      write1 destination size (fun buffer offset ->
          Bytes.blit_string v 0 buffer offset size))
  | Bytes n ->
    (* TODO: support chunk writing of bytes so that it's possible to serialise
     a big blob onto several small buffers *)
    (* TODO: encoding-specific failure modes could be defined in the Encoding
       module maybe? They are related to encodings more than the serialisation
       process. *)
    (* TODO: support for 32-bit machines: don't use `Int63.to_int`, maybe write1
       should take an int63? *)
    if Optint.Int63.compare (Optint.Int63.of_int (Bytes.length v)) (n :> Optint.Int63.t)
       <> 0
    then Failed { destination; error = "inconsistent length of bytes" }
    else (
      let size = Optint.Int63.to_int (n :> Optint.Int63.t) in
      write1 destination size (fun buffer offset -> Bytes.blit v 0 buffer offset size))
  | Option t ->
    (match v with
    | None ->
      let* destination =
        write1 destination Size.uint8 (fun buffer offset ->
            Endian.set_uint8 buffer offset Magic.option_none_tag)
      in
      Written { destination }
    | Some v ->
      let* destination =
        write1 destination Size.uint8 (fun buffer offset ->
            Endian.set_uint8 buffer offset Magic.option_some_tag)
      in
      writek destination t v)
  | Headered { mkheader; headerencoding; mkencoding; equal = _; maximum_size = _ } ->
    (match mkheader v with
    | Error msg ->
      let error = "error in user-provided mkheader function: " ^ msg in
      Failed { destination; error }
    | Ok header ->
      let* destination = writek destination headerencoding header in
      (match mkencoding header with
      | Error msg ->
        let error = "error in user-provided encoding function: " ^ msg in
        Failed { destination; error }
      | Ok encoding -> writek destination encoding v))
  | Conv { serialisation; deserialisation = _; encoding } ->
    writek destination encoding (serialisation v)
  | [] ->
    assert (v = []);
    Written { destination }
  | t :: ts ->
    (match v with
    | v :: vs ->
      let* destination = writek destination t v in
      writek destination ts vs)
;;

let write
    : type a.
      dst:bytes
      -> offset:int
      -> length:int
      -> a Encoding.t
      -> a
      -> (int, int * string) result
  =
 fun ~dst:buffer ~offset ~length encoding v ->
  if offset + length > Bytes.length buffer
  then Error (0, "length exceeds buffer size")
  else (
    let destination = mk_destination ~maximum_length:length buffer offset length in
    match writek destination encoding v with
    | Suspended _ -> assert false (* the buffer is bigger than length *)
    | Failed { destination; error } -> Error (destination.written, error)
    | Written { destination } -> Ok destination.written)
;;

let%expect_test _ =
  let scratch = String.make 10 '_' in
  let w : type a. a Encoding.t -> a -> unit =
   fun e v ->
    let dst = Bytes.of_string scratch in
    match write ~dst ~offset:1 ~length:8 e v with
    | Ok n -> Format.printf "Ok(%d): %S\n" n (Bytes.to_string dst)
    | Error (n, s) -> Format.printf "Error(%d,%s): %S" n s (Bytes.to_string dst)
  in
  w Encoding.unit ();
  [%expect {| Ok(0): "__________" |}];
  w Encoding.int64 0x4c_6f_6f_6f_6f_6f_6f_4cL;
  [%expect {| Ok(8): "_LooooooL_" |}];
  w Encoding.int64 0xff_ff_ff_ff_ff_ff_ff_ffL;
  [%expect {| Ok(8): "_\255\255\255\255\255\255\255\255_" |}];
  w Encoding.uint62 (Option.get @@ Commons.Sizedints.Uint62.of_int64 0L);
  [%expect {| Ok(8): "_\000\000\000\000\000\000\000\000_" |}];
  w
    Encoding.uint62
    (Option.get @@ Commons.Sizedints.Uint62.of_int64 0x3c_6f_6f_6f_6f_6f_6f_4cL);
  [%expect {| Ok(8): "_<ooooooL_" |}];
  w Encoding.uint62 Commons.Sizedints.Uint62.max_int;
  [%expect {| Ok(8): "_?\255\255\255\255\255\255\255_" |}];
  w
    Encoding.[ unit; unit; int32; unit; int32 ]
    [ (); (); 0x4c_6f_6f_4cl; (); 0x4c_6f_6f_4cl ];
  [%expect {| Ok(8): "_LooLLooL_" |}];
  w Encoding.[ string `UInt30; unit ] [ "FOO"; () ];
  [%expect {| Ok(7): "_\000\000\000\003FOO__" |}];
  w Encoding.[ string `UInt16; unit ] [ "FOOo0"; () ];
  [%expect {| Ok(7): "_\000\005FOOo0__" |}];
  w Encoding.[ string `UInt8; unit ] [ "FOOo00o"; () ];
  [%expect {| Ok(8): "_\007FOOo00o_" |}];
  ()
;;

let string_of : type a. ?buffer_size:int -> a Encoding.t -> a -> (string, string) result =
 fun ?(buffer_size = 1024) encoding v ->
  let rec chunk buffer acc k =
    match k buffer 0 (Bytes.length buffer) with
    | Written { destination } ->
      Ok (Bytes.sub_string destination.buffer 0 destination.written :: acc)
    | Failed { destination; error } when error = destination_too_small_to_continue_message
      ->
      assert (destination.written = 0);
      let buffer_size = 2 * (1 + Bytes.length buffer) in
      let buffer = Bytes.make buffer_size '\x00' in
      chunk buffer acc k
    | Failed { destination = _; error } -> Error error
    | Suspended { destination; cont } ->
      if destination.written = 0
      then chunk buffer acc cont
      else (
        let acc = Bytes.sub_string destination.buffer 0 destination.written :: acc in
        chunk buffer acc cont)
  in
  let buffer = Bytes.make buffer_size '\x00' in
  match
    chunk buffer [] (fun buffer offset length ->
        let destination = mk_destination ~maximum_length:max_int buffer offset length in
        writek destination encoding v)
  with
  | Ok rev_chunks ->
    let chunks = List.rev rev_chunks in
    Ok (String.concat "" chunks)
  | Error _ as err -> err
;;

let%expect_test _ =
  let w : type a. ?buffer_size:int -> a Encoding.t -> a -> unit =
   fun ?buffer_size e v ->
    match string_of ?buffer_size e v with
    | Ok s -> Format.printf "Ok: %S\n" s
    | Error s -> Format.printf "Error: %s" s
  in
  w ~buffer_size:10 Encoding.unit ();
  [%expect {| Ok: "" |}];
  w ~buffer_size:10 Encoding.int64 0x4c_6f_6f_6f_6f_6f_6f_4cL;
  [%expect {| Ok: "LooooooL" |}];
  w ~buffer_size:5 Encoding.int64 0x4c_6f_6f_6f_6f_6f_6f_4cL;
  [%expect {| Ok: "LooooooL" |}];
  w ~buffer_size:10 Encoding.int64 0xff_ff_ff_ff_ff_ff_ff_ffL;
  [%expect {| Ok: "\255\255\255\255\255\255\255\255" |}];
  w
    ~buffer_size:10
    Encoding.[ unit; unit; int32; unit; int32 ]
    [ (); (); 0x4c_6f_6f_4cl; (); 0x4c_6f_6f_4cl ];
  [%expect {| Ok: "LooLLooL" |}];
  w
    ~buffer_size:10
    Encoding.
      [ String (Option.get @@ Commons.Sizedints.Uint62.of_int64 3L)
      ; String (Option.get @@ Commons.Sizedints.Uint62.of_int64 3L)
      ]
    [ "FOO"; "LOL" ];
  [%expect {| Ok: "FOOLOL" |}];
  w
    ~buffer_size:2
    Encoding.
      [ String (Option.get @@ Commons.Sizedints.Uint62.of_int64 3L)
      ; String (Option.get @@ Commons.Sizedints.Uint62.of_int64 3L)
      ]
    [ "FOO"; "LOL" ];
  [%expect {| Ok: "FOOLOL" |}];
  w
    ~buffer_size:2
    Encoding.
      [ String (Option.get @@ Commons.Sizedints.Uint62.of_int64 3L)
      ; unit
      ; unit
      ; String (Option.get @@ Commons.Sizedints.Uint62.of_int64 8L)
      ]
    [ "FOO"; (); (); "LOLLOLOL" ];
  [%expect {| Ok: "FOOLOLLOLOL" |}];
  w ~buffer_size:10 Encoding.[ int64; option int32 ] [ 0L; Some 0l ];
  [%expect {| Ok: "\000\000\000\000\000\000\000\000\001\000\000\000\000" |}];
  ()
;;

type source =
  { blob : string
  ; offset : int
  ; length : int
  ; readed : int (* [read] is ambiguous so we make it unambiguously past as [readed] *)
  ; maximum_length : int
  }

let mk_source ?(maximum_length = max_int) blob offset length =
  if offset < 0 then failwith "Binary_data_encoding.Backend.mk_source: negative offset";
  if length < 0 then failwith "Binary_data_encoding.Backend.mk_source: negative length";
  if offset + length > String.length blob
  then failwith "Binary_data_encoding.Backend.mk_source: offset+length overflow";
  { blob; offset; length; readed = 0; maximum_length }
;;

let bump_readed source reading = { source with readed = source.readed + reading }

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
                mk_source ~maximum_length:source.maximum_length blob offset length
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
                  mk_source ~maximum_length:source.maximum_length blob offset length
                in
                (* actually do this small here read *)
                let value = read source.blob source.offset in
                let source = bump_readed source reading in
                assert (source.readed = source.length);
                (* Second prepare the source for giving back *)
                let source =
                  mk_source ~maximum_length:source.maximum_length blob offset length
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

let rec readk : type a. source -> a Encoding.t -> a readed =
 fun source encoding ->
  assert (source.offset >= 0);
  assert (source.offset < String.length source.blob);
  match encoding with
  | Unit -> Readed { source; value = () }
  | Bool ->
    let* v, source = read1 source Size.bool Endian.get_uint8_string in
    if v = Magic.bool_true
    then Readed { source; value = true }
    else if v = Magic.bool_false
    then Readed { source; value = false }
    else Failed { source; error = "Unknown value for Bool" }
  | Int64 -> read1 source Size.int64 Endian.get_int64_string
  | Int32 -> read1 source Size.int32 Endian.get_int32_string
  | UInt62 -> read1 source Size.uint62 Endian.get_uint62_string
  | UInt30 -> read1 source Size.uint30 Endian.get_uint30_string
  | UInt16 -> read1 source Size.uint16 Endian.get_uint16_string
  | UInt8 -> read1 source Size.uint8 Endian.get_uint8_string
  | String n ->
    (* TODO: support chunk reading of string so that it's possible to deserialise
     a big blob from several small blobs *)
    (* TODO: support for 32-bit machines: don't use `Int63.to_int`, maybe write1
       should take an int63? *)
    let size = Optint.Int63.to_int (n :> Optint.Int63.t) in
    read1 source size (fun blob offset -> String.sub blob offset size)
  | Bytes n ->
    (* TODO: support chunk reading of bytes so that it's possible to deserialise
     a big blob from several small blobs *)
    (* TODO: support for 32-bit machines: don't use `Int63.to_int`, maybe write1
       should take an int63? *)
    let size = Optint.Int63.to_int (n :> Optint.Int63.t) in
    read1 source size (fun blob offset ->
        Bytes.unsafe_of_string (String.sub blob offset size))
  | Option t ->
    let* tag, source = read1 source Size.uint8 Endian.get_uint8_string in
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
  | Conv { serialisation = _; deserialisation; encoding } ->
    let* v, source = readk source encoding in
    (match deserialisation v with
    | Ok value -> Readed { source; value }
    | Error error -> Failed { source; error })
  | [] -> Readed { source; value = [] }
  | t :: ts ->
    let* v, source = readk source t in
    let* vs, source = readk source ts in
    Readed { source; value = Encoding.Hlist.( :: ) (v, vs) }
;;

let readk : type a. source -> a Encoding.t -> a readed =
 fun source encoding ->
  if source.offset < 0
  then Failed { source; error = "offset is negative" }
  else if source.offset + source.length > String.length source.blob
  then Failed { source; error = "length exceeds buffer size" }
  else readk source encoding
;;

let read_strings
    : type a. (string * int * int) Seq.t -> a Encoding.t -> (a, string) result
  =
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
    : type a. src:string -> offset:int -> length:int -> a Encoding.t -> (a, string) result
  =
 fun ~src ~offset ~length encoding ->
  let source = mk_source ~maximum_length:length src offset length in
  match readk source encoding with
  | Readed { source = _; value } -> Ok value
  | Failed { source = _; error } -> Error error
  | Suspended _ -> Error "one-shot reading does not support suspension"
;;

let%expect_test _ =
  let w : type a. string -> a Encoding.t -> (Format.formatter -> a -> unit) -> unit =
   fun blob e f ->
    match read ~src:blob ~offset:0 ~length:(String.length blob) e with
    | Ok d -> Format.printf "Ok: %a\n" f d
    | Error s -> Format.printf "Error: %s" s
  in
  w "LooooooL" Encoding.int64 (fun fmt i64 -> Format.fprintf fmt "%Lx" i64);
  [%expect {| Ok: 4c6f6f6f6f6f6f4c |}];
  w
    "LooLLooL"
    Encoding.[ unit; int32; int32; unit ]
    (fun fmt [ (); l; r; () ] -> Format.fprintf fmt "%lx_%lx" l r);
  [%expect {| Ok: 4c6f6f4c_4c6f6f4c |}];
  w
    "FOO"
    Encoding.(String (Option.get @@ Commons.Sizedints.Uint62.of_int64 3L))
    Format.pp_print_string;
  [%expect {| Ok: FOO |}];
  w
    "\000\000\000\000\000\000\000\000\001\000\000\000\000"
    Encoding.[ int64; option int32 ]
    (fun fmt [ i64; oi32 ] ->
      Format.fprintf
        fmt
        "%Ld;%a"
        i64
        (fun fmt oi32 ->
          match oi32 with
          | None -> Format.fprintf fmt "None"
          | Some i32 -> Format.fprintf fmt "Some(%ld)" i32)
        oi32);
  [%expect {| Ok: 0;Some(0) |}];
  ()
;;
