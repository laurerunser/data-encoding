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

type chunkwriter = bytes -> int -> int -> chunkwritten

and chunkwritten =
  | K of int * chunkwriter
  | Finish of int

(* similar to [write1] but writes the data in chunks, this allows to write data
   which is larger than the current buffer can accomodate, this is intended for
   writing string and other such potentially big blobs. *)
let rec writechunked destination write =
  let writing =
    min
      (destination.maximum_length - destination.written)
      (destination.length - destination.written)
  in
  assert (writing >= 0);
  match write destination.buffer (destination.offset + destination.written) writing with
  | Finish written ->
    let destination = bump_written destination written in
    Written { destination }
  | K (written, write) ->
    let destination = bump_written destination written in
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
            writechunked destination write)
      }
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

let rec writek : type a. destination -> a Descr.t -> a -> written =
 fun destination encoding v ->
  assert (destination.offset >= 0);
  assert (destination.length >= 0);
  assert (destination.offset + destination.length <= Bytes.length destination.buffer);
  match encoding with
  | Unit -> Written { destination }
  | Bool ->
    write1 destination Size.bool (fun buffer offset ->
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
    (* TODO: support for 32-bit machines: don't use `Int63.to_int`, maybe write1
       should take an int63? *)
    if Optint.Int63.compare (Optint.Int63.of_int (String.length v)) (n :> Optint.Int63.t)
       <> 0
    then Failed { destination; error = "inconsistent length of string" }
    else (
      let size = Optint.Int63.to_int (n :> Optint.Int63.t) in
      let rec chunkwriter source_offset buffer offset maxwritesize =
        let needswriting = size - source_offset in
        if needswriting = 0
        then Finish 0
        else if needswriting <= maxwritesize
        then (
          Bytes.blit_string v source_offset buffer offset needswriting;
          Finish needswriting)
        else (
          Bytes.blit_string v source_offset buffer offset maxwritesize;
          K (maxwritesize, chunkwriter (source_offset + maxwritesize)))
      in
      writechunked destination (chunkwriter 0))
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
      let rec chunkwriter source_offset buffer offset maxwritesize =
        let needswriting = size - source_offset in
        if needswriting = 0
        then Finish 0
        else if needswriting <= maxwritesize
        then (
          Bytes.blit v source_offset buffer offset needswriting;
          Finish needswriting)
        else (
          Bytes.blit v source_offset buffer offset maxwritesize;
          K (maxwritesize, chunkwriter (source_offset + maxwritesize)))
      in
      writechunked destination (chunkwriter 0))
  | LSeq { length; elementencoding } ->
    let length = Optint.Int63.to_int (length :> Optint.Int63.t) in
    let rec fold destination s length =
      match s () with
      | Seq.Nil ->
        if length = 0
        then Written { destination }
        else Failed { destination; error = "inconsistent length of seq" }
      | Seq.Cons (elt, s) ->
        if length <= 0
        then Failed { destination; error = "inconsistent length of seq" }
        else
          let* destination = writek destination elementencoding elt in
          fold destination s (length - 1)
    in
    fold destination v.seq length
  | USeq { elementencoding } ->
    let rec fold destination s =
      match s () with
      | Seq.Nil -> Written { destination }
      | Seq.Cons (elt, s) ->
        let* destination = writek destination elementencoding elt in
        fold destination s
    in
    fold destination v
  | Array { length; elementencoding } ->
    if Option.get @@ Commons.Sizedints.Uint62.of_int64 (Int64.of_int (Array.length v))
       <> length
    then Failed { destination; error = "inconsistent array length" }
    else (
      let length = Optint.Int63.to_int (length :> Optint.Int63.t) in
      let rec fold destination index =
        if index >= length
        then Written { destination }
        else
          let* destination = writek destination elementencoding (Array.get v index) in
          fold destination (index + 1)
      in
      fold destination 0)
  | Option t ->
    (match v with
    | None ->
      let* destination =
        write1 destination Size.uint8 (fun buffer offset ->
            Bytes.set_uint8 buffer offset (Magic.option_none_tag :> int))
      in
      Written { destination }
    | Some v ->
      let* destination =
        write1 destination Size.uint8 (fun buffer offset ->
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
      | Seq.Nil -> Written { destination }
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
    | Written { destination } ->
      let written_before_write = destination.written in
      (* TODO: set a maximum-length so that it always accomodates the size
           header limit (e.g., 256 if uint8) *)
      (match writek destination encoding v with
      | Written { destination } as written_destination ->
        let written_after_write = destination.written in
        let actual_size =
          Query.numeral_of_int size (written_after_write - written_before_write)
        in
        let* (_ : destination) =
          write_numeral original_destination size Encoding.default_endianness actual_size
        in
        written_destination
      | Failed _ as failed -> failed
      | Suspended _ as suspend_written ->
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
          let* (_ : destination) =
            write_numeral
              original_destination
              size
              Encoding.default_endianness
              actual_size
          in
          suspend_written))
    | Failed _ as failed -> failed
    | Suspended _ as suspended ->
      (* we happen onto this case if we are so close to the end of the
               buffer that even the size header cannot be written *)
      suspended)
  | Size_limit { at_most; encoding } ->
    let maximum_length =
      (* TODO: handle overflow *)
      min destination.maximum_length (Optint.Int63.to_int (at_most :> Optint.Int63.t))
    in
    let destination = { destination with maximum_length } in
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
    : type a. destination -> a Descr.numeral -> Descr.endianness -> a -> written
  =
 fun destination numeral endianness v ->
  match numeral, endianness with
  | Int64, Big_endian ->
    write1 destination Size.int64 (fun buffer offset ->
        Bytes.set_int64_be buffer offset v)
  | Int64, Little_endian ->
    write1 destination Size.int64 (fun buffer offset ->
        Bytes.set_int64_le buffer offset v)
  | Int32, Big_endian ->
    write1 destination Size.int32 (fun buffer offset ->
        Bytes.set_int32_be buffer offset v)
  | Int32, Little_endian ->
    write1 destination Size.int32 (fun buffer offset ->
        Bytes.set_int32_le buffer offset v)
  | UInt62, Big_endian ->
    write1 destination Size.uint62 (fun buffer offset ->
        Commons.Sizedints.Uint62.set_be buffer offset v)
  | UInt62, Little_endian ->
    write1 destination Size.uint62 (fun buffer offset ->
        Commons.Sizedints.Uint62.set_le buffer offset v)
  | UInt30, Big_endian ->
    write1 destination Size.uint30 (fun buffer offset ->
        Commons.Sizedints.Uint30.set_be buffer offset v)
  | UInt30, Little_endian ->
    write1 destination Size.uint30 (fun buffer offset ->
        Commons.Sizedints.Uint30.set_le buffer offset v)
  | UInt16, Big_endian ->
    write1 destination Size.uint16 (fun buffer offset ->
        Bytes.set_uint16_be buffer offset (v :> int))
  | UInt16, Little_endian ->
    write1 destination Size.uint16 (fun buffer offset ->
        Bytes.set_uint16_le buffer offset (v :> int))
  | UInt8, _ ->
    write1 destination Size.uint8 (fun buffer offset ->
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
    let destination = mk_destination ~maximum_length:length buffer offset length in
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
