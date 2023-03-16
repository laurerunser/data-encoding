(* TODO: more documentation *)
(* TODO: more assertion checks *)
(* TODO: benchmark and optimise (later, after we add more features) *)

let ( let* ) = Buffy.W.( let* )

let rec writek : type s a. Buffy.W.state -> (s, a) Descr.t -> a -> Buffy.W.written =
 fun state encoding v ->
  match encoding with
  | Unit -> Written { state }
  | Bool ->
    Buffy.W.writef state Size.bool (fun destination offset ->
      if v
      then Buffy.Dst.set_uint8 destination offset (Magic.bool_true :> int)
      else Buffy.Dst.set_uint8 destination offset (Magic.bool_false :> int))
  | Numeral { numeral; endianness } -> write_numeral state numeral endianness v
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
    then Failed { state; error = "inconsistent length of string" }
    else Buffy.W.write_string state v
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
    then Failed { state; error = "inconsistent length of bytes" }
    else Buffy.W.write_bytes state v
  | LSeq { length; elementencoding } ->
    let length = Optint.Int63.to_int (length :> Optint.Int63.t) in
    let rec fold state s length =
      match s () with
      | Seq.Nil ->
        if length = 0
        then Buffy.W.Written { state }
        else Buffy.W.Failed { state; error = "inconsistent length of seq" }
      | Seq.Cons (elt, s) ->
        if length <= 0
        then Buffy.W.Failed { state; error = "inconsistent length of seq" }
        else
          let* state = writek state elementencoding elt in
          fold state s (length - 1)
    in
    fold state v.seq length
  | USeq { elementencoding } ->
    let rec fold state s =
      match s () with
      | Seq.Nil -> Buffy.W.Written { state }
      | Seq.Cons (elt, s) ->
        let* state = writek state elementencoding elt in
        fold state s
    in
    fold state v
  | Array { length; elementencoding } ->
    if Option.get @@ Commons.Sizedints.Uint62.of_int64 (Int64.of_int (Array.length v))
       <> length
    then Buffy.W.Failed { state; error = "inconsistent array length" }
    else (
      let length = Optint.Int63.to_int (length :> Optint.Int63.t) in
      let rec fold state index =
        if index >= length
        then Buffy.W.Written { state }
        else
          let* state = writek state elementencoding (Array.get v index) in
          fold state (index + 1)
      in
      fold state 0)
  | Option (_, t) ->
    (match v with
     | None ->
       let* state =
         Buffy.W.writef state Size.uint8 (fun destination offset ->
           Buffy.Dst.set_uint8 destination offset (Magic.option_none_tag :> int))
       in
       Buffy.W.Written { state }
     | Some v ->
       let* state =
         Buffy.W.writef state Size.uint8 (fun destination offset ->
           Buffy.Dst.set_uint8 destination offset (Magic.option_some_tag :> int))
       in
       writek state t v)
  | Headered { mkheader; headerencoding; mkencoding; equal = _; maximum_size = _ } ->
    (match mkheader v with
     | Error msg ->
       let error = "error in user-provided mkheader function: " ^ msg in
       Failed { state; error }
     | Ok header ->
       let* state = writek state headerencoding header in
       (match mkencoding header with
        | Error msg ->
          let error = "error in user-provided encoding function: " ^ msg in
          Failed { state; error }
        | Ok (EDynamic encoding) -> writek state encoding v
        | Ok (EStatic encoding) -> writek state encoding v))
  | Fold
      { chunkencoding; chunkify; readinit = _; reducer = _; equal = _; maximum_size = _ }
    ->
    let rec fold state chunks =
      match chunks () with
      | Seq.Nil -> Buffy.W.Written { state }
      | Seq.Cons (chunk, chunks) ->
        let* state = writek state chunkencoding chunk in
        fold state chunks
    in
    fold state (chunkify v)
  | Conv { serialisation; deserialisation = _; encoding } ->
    writek state encoding (serialisation v)
  | Size_headered { size; encoding } ->
    let original_state = state in
    let size0 = Query.zero_of_numeral size in
    (* TODO: make tests that covers the Written and the Suspended cases *)
    (match write_numeral state size Encoding.default_endianness size0 with
     | Buffy.W.Written { state } ->
       let written_before_write = state.written in
       (* TODO: set a maximum-length so that it always accomodates the size
           header limit (e.g., 256 if uint8) *)
       (match writek state encoding v with
        | Buffy.W.Written { state } as written_state ->
          let written_after_write = state.written in
          let actual_size =
            Query.numeral_of_int size (written_after_write - written_before_write)
          in
          let* (_ : Buffy.W.state) =
            write_numeral original_state size Encoding.default_endianness actual_size
          in
          written_state
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
             Failed { state; error = s }
           | Ok actual_size ->
             (* NOTE: actual_size fits in the size header otherwise
                [Query.size_of] returns [Error] *)
             let actual_size =
               Query.numeral_of_int size (Optint.Int63.to_int actual_size)
             in
             let* (_ : Buffy.W.state) =
               write_numeral original_state size Encoding.default_endianness actual_size
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
      min state.maximum_length (Optint.Int63.to_int (at_most :> Optint.Int63.t))
    in
    let state = Buffy.W.set_maximum_length state maximum_length in
    writek state encoding v
  | Union { tag = tag_encoding; serialisation; deserialisation = _; cases = _ } ->
    let (AnyP ({ Descr.tag; encoding; inject = _ }, payload)) = serialisation v in
    let* state = writek state tag_encoding tag in
    writek state encoding payload
  | TupNil -> Written { state }
  | TupCons (_, t, ts) ->
    (match v with
     | v :: vs ->
       let* state = writek state t v in
       writek state ts vs)

and write_numeral
  : type a. Buffy.W.state -> a Descr.numeral -> Descr.endianness -> a -> Buffy.W.written
  =
 fun state numeral endianness v ->
  match numeral, endianness with
  | Int64, Big_endian ->
    Buffy.W.writef state Size.int64 (fun destination offset ->
      Buffy.Dst.set_int64_be destination offset v)
  | Int64, Little_endian ->
    Buffy.W.writef state Size.int64 (fun destination offset ->
      Buffy.Dst.set_int64_le destination offset v)
  | Int32, Big_endian ->
    Buffy.W.writef state Size.int32 (fun destination offset ->
      Buffy.Dst.set_int32_be destination offset v)
  | Int32, Little_endian ->
    Buffy.W.writef state Size.int32 (fun destination offset ->
      Buffy.Dst.set_int32_le destination offset v)
  | UInt62, Big_endian ->
    Buffy.W.writef state Size.uint62 (fun destination offset ->
      Buffy.Dst.set_int64_be
        destination
        offset
        (Optint.Int63.to_int64 (v :> Optint.Int63.t)))
  | UInt62, Little_endian ->
    Buffy.W.writef state Size.uint62 (fun destination offset ->
      Buffy.Dst.set_int64_le
        destination
        offset
        (Optint.Int63.to_int64 (v :> Optint.Int63.t)))
  | UInt30, Big_endian ->
    Buffy.W.writef state Size.uint30 (fun destination offset ->
      Buffy.Dst.set_int32_be destination offset (Int32.of_int (v :> int)))
  | UInt30, Little_endian ->
    Buffy.W.writef state Size.uint30 (fun destination offset ->
      Buffy.Dst.set_int32_le destination offset (Int32.of_int (v :> int)))
  | UInt16, Big_endian ->
    Buffy.W.writef state Size.uint16 (fun destination offset ->
      Buffy.Dst.set_uint16_be destination offset (v :> int))
  | UInt16, Little_endian ->
    Buffy.W.writef state Size.uint16 (fun destination offset ->
      Buffy.Dst.set_uint16_le destination offset (v :> int))
  | UInt8, _ ->
    Buffy.W.writef state Size.uint8 (fun destination offset ->
      Buffy.Dst.set_uint8 destination offset (v :> int))
;;

let write
  : type s a.
    dst:bytes
    -> offset:int
    -> length:int
    -> (s, a) Descr.t
    -> a
    -> (int, int * string) result
  =
 fun ~dst:buffer ~offset ~length encoding v ->
  if offset + length > Bytes.length buffer
  then Error (0, "length exceeds buffer size")
  else (
    let destination = Buffy.Dst.of_bytes buffer ~offset ~length in
    let state = Buffy.W.mk_state ~maximum_length:length destination in
    match writek state encoding v with
    | Suspended _ -> assert false (* the buffer is bigger than length *)
    | Failed { state; error } -> Error (state.written, error)
    | Written { state } -> Ok state.written)
;;

let write_e
  : type a.
    dst:bytes
    -> offset:int
    -> length:int
    -> a Encoding.t
    -> a
    -> (int, int * string) result
  =
 fun ~dst ~offset ~length encoding v ->
  let (E encoding) = Encoding.Advanced_low_level.introspect encoding in
  write ~dst ~offset ~length encoding v
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
    match write_e ~dst ~offset:1 ~length:8 e v with
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
    Encoding.(tuple [ unit; unit; int32; unit; int32 ])
    [ (); (); 0x4c_6f_6f_4cl; (); 0x4c_6f_6f_4cl ];
  [%expect {| Ok(8): 004c6f6f4c4c6f6f4c00 |}];
  w Encoding.(tuple [ string `UInt30; unit ]) [ "FOO"; () ];
  [%expect {| Ok(7): 0000000003464f4f0000 |}];
  w Encoding.(tuple [ string `UInt16; unit ]) [ "FOOo0"; () ];
  [%expect {| Ok(7): 000005464f4f6f300000 |}];
  w Encoding.(tuple [ string `UInt8; unit ]) [ "FOOo00o"; () ];
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
    Encoding.(
      with_size_header
        ~sizeencoding:`UInt8
        ~encoding:
          (tuple
             [ Advanced_low_level.(forget (E (USeq { elementencoding = Bool })))
             ; string (`Fixed (Option.get @@ Commons.Sizedints.Uint62.of_int 3))
             ]))
    [ Seq.empty; "LOl" ];
  [%expect {| Ok(4): 00034c4f6c0000000000 |}];
  w Encoding.(With_size.seq_with_size `UInt8 uint8) Seq.empty;
  [%expect {| Ok(1): 00000000000000000000 |}];
  w
    Encoding.(With_size.seq_with_size `UInt8 uint8)
    (Seq.return (Option.get @@ Commons.Sizedints.Uint8.of_int 0));
  [%expect {| Ok(2): 00010000000000000000 |}];
  w
    Encoding.(With_size.seq_with_size `UInt8 (option uint8))
    (Seq.map Commons.Sizedints.Uint8.of_int (List.to_seq [ 0; 1; 2 ]));
  [%expect {| Ok(7): 00060100010101020000 |}];
  ()
;;

let string_of
  : type s a. ?buffer_size:int -> (s, a) Descr.t -> a -> (string, string) result
  =
 fun ?buffer_size encoding v ->
  Buffy.W.to_string ?buffer_size (fun state -> writek state encoding v)
;;

let string_of_e : type a. ?buffer_size:int -> a Encoding.t -> a -> (string, string) result
  =
 fun ?buffer_size encoding v ->
  let (E encoding) = Encoding.Advanced_low_level.introspect encoding in
  string_of ?buffer_size encoding v
;;

let%expect_test _ =
  let w : type a. ?buffer_size:int -> a Encoding.t -> a -> unit =
   fun ?buffer_size e v ->
    match string_of_e ?buffer_size e v with
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
    Encoding.(tuple [ unit; unit; int32; unit; int32 ])
    [ (); (); 0x4c_6f_6f_4cl; (); 0x4c_6f_6f_4cl ];
  [%expect {| Ok: "LooLLooL" |}];
  let str_x_str =
    Encoding.(
      tuple
        [ string (`Fixed (Option.get @@ Commons.Sizedints.Uint62.of_int64 3L))
        ; string (`Fixed (Option.get @@ Commons.Sizedints.Uint62.of_int64 3L))
        ])
  in
  w ~buffer_size:10 str_x_str [ "FOO"; "LOL" ];
  [%expect {| Ok: "FOOLOL" |}];
  w ~buffer_size:2 str_x_str [ "FOO"; "LOL" ];
  [%expect {| Ok: "FOOLOL" |}];
  w
    ~buffer_size:2
    Encoding.(
      tuple
        [ string (`Fixed (Option.get @@ Commons.Sizedints.Uint62.of_int64 3L))
        ; unit
        ; unit
        ; string (`Fixed (Option.get @@ Commons.Sizedints.Uint62.of_int64 8L))
        ])
    [ "FOO"; (); (); "LOLLOLOL" ];
  [%expect {| Ok: "FOOLOLLOLOL" |}];
  w ~buffer_size:10 Encoding.(tuple [ int64; option int32 ]) [ 0L; Some 0l ];
  [%expect {| Ok: "\000\000\000\000\000\000\000\000\001\000\000\000\000" |}];
  w ~buffer_size:10 Encoding.(With_size.seq_with_size `UInt8 uint8) Seq.empty;
  [%expect {| Ok: "\000" |}];
  w
    ~buffer_size:10
    Encoding.(With_size.seq_with_size `UInt8 uint8)
    (Seq.return (Option.get @@ Commons.Sizedints.Uint8.of_int 0));
  [%expect {| Ok: "\001\000" |}];
  w
    ~buffer_size:10
    Encoding.(With_size.seq_with_size `UInt8 (option uint8))
    (Seq.map Commons.Sizedints.Uint8.of_int (List.to_seq [ 0; 1; 2 ]));
  [%expect {| Ok: "\006\001\000\001\001\001\002" |}];
  ()
;;
