(* TODO: more documentation *)
(* TODO: more assertion checks *)
(* TODO: benchmark and optimise (later, after we add more features) *)

let ( let* ) = Buffy.W.( let* )

let rec writek : type a. Buffy.W.destination -> a Descr.t -> a -> Buffy.W.written =
 fun destination encoding v ->
  assert (destination.offset >= 0);
  assert (destination.length >= 0);
  assert (destination.offset + destination.length <= Bytes.length destination.buffer);
  match encoding with
  | Unit -> Written { destination }
  | Bool ->
    Buffy.W.writef destination Size.bool (fun buffer offset ->
        if v
        then Bytes.set_uint8 buffer offset (Magic.bool_true :> int)
        else Bytes.set_uint8 buffer offset (Magic.bool_false :> int))
  | Numeral { numeral; endianness } -> write_numeral destination numeral endianness v
  | String n ->
    (* TODO: support chunk writing of strings so that it's possible to serialise
     a big blob onto several small buffers *)
    (* TODO: encoding-specific failure modes could be defined in the Encoding
       module maybe? They are related to encodings more than the serialisation
       process. *)
    (* TODO: support for 32-bit machines: don't use `Int63.to_int`, maybe writef
       should take an int63? *)
    if Optint.Int63.compare (Optint.Int63.of_int (String.length v)) (n :> Optint.Int63.t)
       <> 0
    then Failed { destination; error = "inconsistent length of string" }
    else Buffy.W.write_large_string destination v
  | Bytes n ->
    (* TODO: support chunk writing of bytes so that it's possible to serialise
     a big blob onto several small buffers *)
    (* TODO: encoding-specific failure modes could be defined in the Encoding
       module maybe? They are related to encodings more than the serialisation
       process. *)
    (* TODO: support for 32-bit machines: don't use `Int63.to_int`, maybe writef
       should take an int63? *)
    if Optint.Int63.compare (Optint.Int63.of_int (Bytes.length v)) (n :> Optint.Int63.t)
       <> 0
    then Failed { destination; error = "inconsistent length of bytes" }
    else Buffy.W.write_large_bytes destination v
  | LSeq { length; elementencoding } ->
    let length = Optint.Int63.to_int (length :> Optint.Int63.t) in
    let rec fold destination s length =
      match s () with
      | Seq.Nil ->
        if length = 0
        then Buffy.W.Written { destination }
        else Buffy.W.Failed { destination; error = "inconsistent length of seq" }
      | Seq.Cons (elt, s) ->
        if length <= 0
        then Buffy.W.Failed { destination; error = "inconsistent length of seq" }
        else
          let* destination = writek destination elementencoding elt in
          fold destination s (length - 1)
    in
    fold destination v.seq length
  | USeq { elementencoding } ->
    let rec fold destination s =
      match s () with
      | Seq.Nil -> Buffy.W.Written { destination }
      | Seq.Cons (elt, s) ->
        let* destination = writek destination elementencoding elt in
        fold destination s
    in
    fold destination v
  | Array { length; elementencoding } ->
    if Option.get @@ Commons.Sizedints.Uint62.of_int64 (Int64.of_int (Array.length v))
       <> length
    then Buffy.W.Failed { destination; error = "inconsistent array length" }
    else (
      let length = Optint.Int63.to_int (length :> Optint.Int63.t) in
      let rec fold destination index =
        if index >= length
        then Buffy.W.Written { destination }
        else
          let* destination = writek destination elementencoding (Array.get v index) in
          fold destination (index + 1)
      in
      fold destination 0)
  | Option t ->
    (match v with
    | None ->
      let* destination =
        Buffy.W.writef destination Size.uint8 (fun buffer offset ->
            Bytes.set_uint8 buffer offset (Magic.option_none_tag :> int))
      in
      Buffy.W.Written { destination }
    | Some v ->
      let* destination =
        Buffy.W.writef destination Size.uint8 (fun buffer offset ->
            Bytes.set_uint8 buffer offset (Magic.option_some_tag :> int))
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
  | Fold
      { chunkencoding; chunkify; readinit = _; reducer = _; equal = _; maximum_size = _ }
    ->
    let rec fold destination chunks =
      match chunks () with
      | Seq.Nil -> Buffy.W.Written { destination }
      | Seq.Cons (chunk, chunks) ->
        let* destination = writek destination chunkencoding chunk in
        fold destination chunks
    in
    fold destination (chunkify v)
  | Conv { serialisation; deserialisation = _; encoding } ->
    writek destination encoding (serialisation v)
  | Size_headered { size; encoding } ->
    let original_destination = destination in
    let size0 = Query.zero_of_numeral size in
    (* TODO: make tests that covers the Written and the Suspended cases *)
    (match write_numeral destination size Encoding.default_endianness size0 with
    | Buffy.W.Written { destination } ->
      let written_before_write = destination.written in
      (* TODO: set a maximum-length so that it always accomodates the size
           header limit (e.g., 256 if uint8) *)
      (match writek destination encoding v with
      | Buffy.W.Written { destination } as written_destination ->
        let written_after_write = destination.written in
        let actual_size =
          Query.numeral_of_int size (written_after_write - written_before_write)
        in
        let* (_ : Buffy.W.destination) =
          write_numeral original_destination size Encoding.default_endianness actual_size
        in
        written_destination
      | Buffy.W.Failed _ as failed -> failed
      | Buffy.W.Suspended _ as suspend_written ->
        (* slow-path: we compute the whole size and we write it before
               returning the suspension. This causes to traverse [v] which is
               the price to pay for chunked writing *)
        (* TODO: make a whole lot of tests that [size_of] and [writek] agree
               on size *)
        (* TODO: make a [size_of] variant which stops early when exceeding
               [maximum_length] *)
        (match Query.size_of encoding v with
        | Error s ->
          (* TODO: wrap error message *)
          Failed { destination; error = s }
        | Ok actual_size ->
          let actual_size = Query.numeral_of_int size (Optint.Int63.to_int actual_size) in
          let* (_ : Buffy.W.destination) =
            write_numeral
              original_destination
              size
              Encoding.default_endianness
              actual_size
          in
          suspend_written))
    | Buffy.W.Failed _ as failed -> failed
    | Buffy.W.Suspended _ as suspended ->
      (* we happen onto this case if we are so close to the end of the
               buffer that even the size header cannot be written *)
      suspended)
  | Size_limit { at_most; encoding } ->
    let maximum_length =
      (* TODO: handle overflow *)
      min destination.maximum_length (Optint.Int63.to_int (at_most :> Optint.Int63.t))
    in
    let destination = Buffy.W.set_maximum_length destination maximum_length in
    writek destination encoding v
  | [] ->
    assert (v = []);
    Written { destination }
  | t :: ts ->
    (match v with
    | v :: vs ->
      let* destination = writek destination t v in
      writek destination ts vs)

and write_numeral
    : type a.
      Buffy.W.destination -> a Descr.numeral -> Descr.endianness -> a -> Buffy.W.written
  =
 fun destination numeral endianness v ->
  match numeral, endianness with
  | Int64, Big_endian ->
    Buffy.W.writef destination Size.int64 (fun buffer offset ->
        Bytes.set_int64_be buffer offset v)
  | Int64, Little_endian ->
    Buffy.W.writef destination Size.int64 (fun buffer offset ->
        Bytes.set_int64_le buffer offset v)
  | Int32, Big_endian ->
    Buffy.W.writef destination Size.int32 (fun buffer offset ->
        Bytes.set_int32_be buffer offset v)
  | Int32, Little_endian ->
    Buffy.W.writef destination Size.int32 (fun buffer offset ->
        Bytes.set_int32_le buffer offset v)
  | UInt62, Big_endian ->
    Buffy.W.writef destination Size.uint62 (fun buffer offset ->
        Commons.Sizedints.Uint62.set_be buffer offset v)
  | UInt62, Little_endian ->
    Buffy.W.writef destination Size.uint62 (fun buffer offset ->
        Commons.Sizedints.Uint62.set_le buffer offset v)
  | UInt30, Big_endian ->
    Buffy.W.writef destination Size.uint30 (fun buffer offset ->
        Commons.Sizedints.Uint30.set_be buffer offset v)
  | UInt30, Little_endian ->
    Buffy.W.writef destination Size.uint30 (fun buffer offset ->
        Commons.Sizedints.Uint30.set_le buffer offset v)
  | UInt16, Big_endian ->
    Buffy.W.writef destination Size.uint16 (fun buffer offset ->
        Bytes.set_uint16_be buffer offset (v :> int))
  | UInt16, Little_endian ->
    Buffy.W.writef destination Size.uint16 (fun buffer offset ->
        Bytes.set_uint16_le buffer offset (v :> int))
  | UInt8, _ ->
    Buffy.W.writef destination Size.uint8 (fun buffer offset ->
        Bytes.set_uint8 buffer offset (v :> int))
;;

let write
    : type a.
      dst:bytes
      -> offset:int
      -> length:int
      -> a Descr.t
      -> a
      -> (int, int * string) result
  =
 fun ~dst:buffer ~offset ~length encoding v ->
  if offset + length > Bytes.length buffer
  then Error (0, "length exceeds buffer size")
  else (
    let destination =
      Buffy.W.mk_destination ~maximum_length:length buffer offset length
    in
    match writek destination encoding v with
    | Suspended _ -> assert false (* the buffer is bigger than length *)
    | Failed { destination; error } -> Error (destination.written, error)
    | Written { destination } -> Ok destination.written)
;;

let%expect_test _ =
  let scratch = String.make 10 '\x00' in
  let pp_bytes fmt b =
    Format.pp_print_seq
      ~pp_sep:(fun _ () -> ())
      (fun fmt ch -> Format.fprintf fmt "%02x" (Char.code ch))
      fmt
      (Bytes.to_seq b)
  in
  let w : type a. a Encoding.t -> a -> unit =
   fun e v ->
    let dst = Bytes.of_string scratch in
    match write ~dst ~offset:1 ~length:8 e v with
    | Ok n -> Format.printf "Ok(%d): %a\n" n pp_bytes dst
    | Error (n, s) -> Format.printf "Error(%d,%s): %a" n s pp_bytes dst
  in
  w Encoding.unit ();
  [%expect {| Ok(0): 00000000000000000000 |}];
  w Encoding.int64 0x4c_6f_6f_6f_6f_6f_6f_4cL;
  [%expect {| Ok(8): 004c6f6f6f6f6f6f4c00 |}];
  w Encoding.int64 0xff_ff_ff_ff_ff_ff_ff_ffL;
  [%expect {| Ok(8): 00ffffffffffffffff00 |}];
  w Encoding.int64 0x4c_6f_6f_6f_6f_4c_4c_4cL;
  [%expect {| Ok(8): 004c6f6f6f6f4c4c4c00 |}];
  w Encoding.Little_endian.int64 0x4c_6f_6f_6f_6f_4c_4c_4cL;
  [%expect {| Ok(8): 004c4c4c6f6f6f6f4c00 |}];
  w Encoding.uint62 (Option.get @@ Commons.Sizedints.Uint62.of_int64 0L);
  [%expect {| Ok(8): 00000000000000000000 |}];
  w
    Encoding.uint62
    (Option.get @@ Commons.Sizedints.Uint62.of_int64 0x3c_6f_6f_6f_6f_6f_6f_4cL);
  [%expect {| Ok(8): 003c6f6f6f6f6f6f4c00 |}];
  w
    Encoding.Little_endian.uint62
    (Option.get @@ Commons.Sizedints.Uint62.of_int64 0x3c_6f_6f_6f_6f_6f_6f_4cL);
  [%expect {| Ok(8): 004c6f6f6f6f6f6f3c00 |}];
  w Encoding.uint62 Commons.Sizedints.Uint62.max_int;
  [%expect {| Ok(8): 003fffffffffffffff00 |}];
  w
    Encoding.[ unit; unit; int32; unit; int32 ]
    [ (); (); 0x4c_6f_6f_4cl; (); 0x4c_6f_6f_4cl ];
  [%expect {| Ok(8): 004c6f6f4c4c6f6f4c00 |}];
  w Encoding.[ string `UInt30; unit ] [ "FOO"; () ];
  [%expect {| Ok(7): 0000000003464f4f0000 |}];
  w Encoding.[ string `UInt16; unit ] [ "FOOo0"; () ];
  [%expect {| Ok(7): 000005464f4f6f300000 |}];
  w Encoding.[ string `UInt8; unit ] [ "FOOo00o"; () ];
  [%expect {| Ok(8): 0007464f4f6f30306f00 |}];
  w Encoding.ellastic_uint30 Commons.Sizedints.Uint30.min_int;
  [%expect {| Ok(1): 00000000000000000000 |}];
  w Encoding.ellastic_uint30 (Option.get @@ Commons.Sizedints.Uint30.of_int 1);
  [%expect {| Ok(1): 00010000000000000000 |}];
  w Encoding.ellastic_uint30 (Option.get @@ Commons.Sizedints.Uint30.of_int 0b0111_1110);
  [%expect {| Ok(1): 007e0000000000000000 |}];
  w Encoding.ellastic_uint30 (Option.get @@ Commons.Sizedints.Uint30.of_int 0b0111_1111);
  [%expect {| Ok(1): 007f0000000000000000 |}];
  w Encoding.ellastic_uint30 (Option.get @@ Commons.Sizedints.Uint30.of_int 0b1000_0000);
  [%expect {| Ok(2): 00800100000000000000 |}];
  w Encoding.ellastic_uint30 (Option.get @@ Commons.Sizedints.Uint30.of_int 0b1000_0001);
  [%expect {| Ok(2): 00810100000000000000 |}];
  w
    Encoding.ellastic_uint30
    (Option.get @@ Commons.Sizedints.Uint30.of_int 0b1010_1010_1010_1010);
  [%expect {| Ok(3): 00aad502000000000000 |}];
  w
    Encoding.(array (`Fixed (Option.get @@ Commons.Sizedints.Uint62.of_int64 2L)) uint8)
    [| Option.get @@ Commons.Sizedints.Uint8.of_int 127
     ; Option.get @@ Commons.Sizedints.Uint8.of_int 255
    |];
  [%expect {| Ok(2): 007fff00000000000000 |}];
  w
    Encoding.(array (`Fixed (Option.get @@ Commons.Sizedints.Uint62.of_int64 3L)) uint8)
    [| Option.get @@ Commons.Sizedints.Uint8.of_int 127
     ; Option.get @@ Commons.Sizedints.Uint8.of_int 255
     ; Option.get @@ Commons.Sizedints.Uint8.of_int 1
    |];
  [%expect {| Ok(3): 007fff01000000000000 |}];
  w Encoding.(array `UInt8 uint8) [||];
  [%expect {| Ok(1): 00000000000000000000 |}];
  w
    Encoding.(array `UInt8 uint8)
    [| Option.get @@ Commons.Sizedints.Uint8.of_int 127
     ; Option.get @@ Commons.Sizedints.Uint8.of_int 255
     ; Option.get @@ Commons.Sizedints.Uint8.of_int 1
    |];
  [%expect {| Ok(4): 00037fff010000000000 |}];
  w
    Encoding.(with_size_header ~sizeencoding:`UInt8 ~encoding:(array `UInt8 uint8))
    [| Option.get @@ Commons.Sizedints.Uint8.of_int 127
     ; Option.get @@ Commons.Sizedints.Uint8.of_int 255
     ; Option.get @@ Commons.Sizedints.Uint8.of_int 1
    |];
  [%expect {| Ok(5): 0004037fff0100000000 |}];
  w
    Encoding.(
      with_size_header
        ~sizeencoding:`UInt8
        ~encoding:(string (`Fixed (Option.get @@ Commons.Sizedints.Uint62.of_int 3))))
    "LOl";
  [%expect {| Ok(4): 00034c4f6c0000000000 |}];
  w Encoding.(seq_with_size `UInt8 uint8) Seq.empty;
  [%expect {| Ok(1): 00000000000000000000 |}];
  w
    Encoding.(seq_with_size `UInt8 uint8)
    (Seq.return (Option.get @@ Commons.Sizedints.Uint8.of_int 0));
  [%expect {| Ok(2): 00010000000000000000 |}];
  w
    Encoding.(seq_with_size `UInt8 (option uint8))
    (Seq.map Commons.Sizedints.Uint8.of_int (List.to_seq [ 0; 1; 2 ]));
  [%expect {| Ok(7): 00060100010101020000 |}];
  ()
;;

let string_of : type a. ?buffer_size:int -> a Descr.t -> a -> (string, string) result =
 fun ?buffer_size encoding v ->
  Buffy.W.to_string ?buffer_size (fun destination -> writek destination encoding v)
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
  w
    ~buffer_size:10
    Encoding.(with_size_header ~sizeencoding:`UInt8 ~encoding:[ int64; option int32 ])
    [ 0L; Some 0l ];
  [%expect {| Ok: "\r\000\000\000\000\000\000\000\000\001\000\000\000\000" |}];
  w
    ~buffer_size:10
    Encoding.(with_size_header ~sizeencoding:`UInt16 ~encoding:(string `UInt16))
    "THERE";
  [%expect {| Ok: "\000\007\000\005THERE" |}];
  w
    ~buffer_size:10
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
                       ~encoding:(with_size_header ~sizeencoding:`UInt8 ~encoding:unit)))))
    ();
  [%expect {| Ok: "\004\003\002\001\000" |}];
  w ~buffer_size:10 Encoding.(seq_with_size `UInt8 uint8) Seq.empty;
  [%expect {| Ok: "\000" |}];
  w
    ~buffer_size:10
    Encoding.(seq_with_size `UInt8 uint8)
    (Seq.return (Option.get @@ Commons.Sizedints.Uint8.of_int 0));
  [%expect {| Ok: "\001\000" |}];
  w
    ~buffer_size:10
    Encoding.(seq_with_size `UInt8 (option uint8))
    (Seq.map Commons.Sizedints.Uint8.of_int (List.to_seq [ 0; 1; 2 ]));
  [%expect {| Ok: "\006\001\000\001\001\001\002" |}];
  ()
;;
