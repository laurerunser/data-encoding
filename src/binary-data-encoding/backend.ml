(* a [destination] is a buffer (with metadata) that the basic serialisation
   function writes into *)
type destination =
  { buffer : bytes (* the bytes to write into *)
  ; offset : int (* the offset at which to start *)
  ; length : int
  ; written : int (* the count of bytes already written into the buffer *)
  ; maximum_length : int
        (* the maximum number of bytes to write throughout a whole writing
           operation: this is passed from destination to destination as the
           suspended/continue mechanism (see below) keeps using more and more
           local buffers *)
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

let bump_written destination writing =
  { destination with written = destination.written + writing }
;;

type written =
  (* The writing has been successful,
     the destination carries the metadata necessary to either keep writing more
     of the value being serialised (e.g., in the case of a product with
     multiple sequential component) or retreive the written bytes *)
  | Written of { destination : destination }
  (* The writing has failed. the destination has accurate metadata (about
     how much of the buffer has been used) and the error has a message
     for the end user explaining the issue *)
  | Failed of
      { destination : destination
      ; error : string
      }
  (* The writing is suspended because the buffer is about to overflow (but
     it hasn't happened yet). The destination has accurate, up-to-date
     information about the buffer used for the writing, and the [cont] has
     a function to resume the serialisation with a buffer at a certain
     offset; you can re-use the same buffer after having used its content,
     or pass a new one. *)
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
              (* if the new destination is too small again we just fail *)
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
  | Int64 ->
    write1 destination Size.int64 (fun buffer offset -> Endian.set_int64 buffer offset v)
  | UInt64 ->
    write1 destination Size.uint64 (fun buffer offset ->
        let v = Unsigned.UInt64.to_int64 v in
        Endian.set_int64 buffer offset v)
  | Int32 ->
    write1 destination Size.int32 (fun buffer offset -> Endian.set_int32 buffer offset v)
  | UInt32 ->
    write1 destination Size.uint32 (fun buffer offset ->
        let v = Unsigned.UInt32.to_int32 v in
        Endian.set_int32 buffer offset v)
  | UInt16 ->
    write1 destination Size.uint16 (fun buffer offset ->
        (* TODO: bug here? All unsigned should have tests for values around their bounds *)
        let v = Unsigned.UInt16.to_int v in
        Endian.set_int16 buffer offset v)
  | String n ->
    let n = Unsigned.UInt32.to_int n in
    if String.length v <> n
    then Failed { destination; error = "inconsistent length of string" }
    else write1 destination n (fun buffer offset -> Bytes.blit_string v 0 buffer offset n)
  | Bytes n ->
    let n = Unsigned.UInt32.to_int n in
    if Bytes.length v <> n
    then Failed { destination; error = "inconsistent length of bytes" }
    else write1 destination n (fun buffer offset -> Bytes.blit v 0 buffer offset n)
  | Option t ->
    (match v with
    | None ->
      let* destination =
        write1 destination Size.uint8 (fun buffer offset ->
            (* TODO: 0 is a magic constant, make it a named tag *)
            Endian.set_uint8 buffer offset 0)
      in
      Written { destination }
    | Some v ->
      let* destination =
        write1 destination Size.uint8 (fun buffer offset ->
            (* TODO: 1 is a magic constant, make it a named tag *)
            Endian.set_uint8 buffer offset 1)
      in
      writek destination t v)
  | Headered { mkheader; headerencoding; encoding } ->
    let header = mkheader v in
    let* destination = writek destination headerencoding header in
    let encoding = encoding header in
    writek destination encoding v
  | [] ->
    assert (v = []);
    Written { destination }
  | t :: ts ->
    (match v with
    | v :: vs ->
      let* destination = writek destination t v in
      writek destination ts vs)
;;

let writek : type a. destination -> a Encoding.t -> a -> written =
 fun destination encoding v ->
  if destination.offset < 0
  then Failed { destination; error = "offset is negative" }
  else if destination.length < 0
  then Failed { destination; error = "length is negative" }
  else if destination.offset + destination.length > Bytes.length destination.buffer
  then Failed { destination; error = "offset+length is beyond length of buffer" }
  else writek destination encoding v
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
    | Ok n -> Format.printf "Ok(%d): %s\n" n (Bytes.to_string dst)
    | Error (n, s) -> Format.printf "Error(%d,%s): %s" n s (Bytes.to_string dst)
  in
  w Encoding.unit ();
  [%expect {| Ok(0): __________ |}];
  w Encoding.int64 0x4c_6f_6f_6f_6f_6f_6f_4cL;
  [%expect {| Ok(8): _LooooooL_ |}];
  w Encoding.int64 0xff_ff_ff_ff_ff_ff_ff_ffL;
  [%expect {| Ok(8): _ÿÿÿÿÿÿÿÿ_ |}];
  w
    Encoding.[ unit; unit; int32; unit; int32 ]
    [ (); (); 0x4c_6f_6f_4cl; (); 0x4c_6f_6f_4cl ];
  [%expect {| Ok(8): _LooLLooL_ |}];
  w Encoding.[ string; unit ] [ "FOO"; () ];
  [%expect {| Ok(7): _   FOO__ |}];
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
    Encoding.[ String (Unsigned.UInt32.of_int 3); String (Unsigned.UInt32.of_int 3) ]
    [ "FOO"; "LOL" ];
  [%expect {| Ok: "FOOLOL" |}];
  w
    ~buffer_size:2
    Encoding.[ String (Unsigned.UInt32.of_int 3); String (Unsigned.UInt32.of_int 3) ]
    [ "FOO"; "LOL" ];
  [%expect {| Ok: "FOOLOL" |}];
  w
    ~buffer_size:2
    Encoding.
      [ String (Unsigned.UInt32.of_int 3); unit; unit; String (Unsigned.UInt32.of_int 8) ]
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
  | Int64 -> read1 source Size.int64 Endian.get_int64_string
  | UInt64 ->
    read1 source Size.uint64 (fun blob offset ->
        let v = Endian.get_int64_string blob offset in
        Unsigned.UInt64.of_int64 v)
  | Int32 -> read1 source Size.int32 Endian.get_int32_string
  | UInt32 ->
    read1 source Size.uint32 (fun blob offset ->
        let v = Endian.get_int32_string blob offset in
        Unsigned.UInt32.of_int32 v)
  | UInt16 ->
    read1 source Size.uint16 (fun blob offset ->
        let v = Endian.get_int16_string blob offset in
        Unsigned.UInt16.of_int v)
  | String n ->
    let n = Unsigned.UInt32.to_int n in
    read1 source n (fun blob offset -> String.sub blob offset n)
  | Bytes n ->
    let n = Unsigned.UInt32.to_int n in
    read1 source n (fun blob offset -> Bytes.unsafe_of_string (String.sub blob offset n))
  | Option t ->
    let* tag, source = read1 source Size.uint8 Endian.get_uint8_string in
    if tag = 0
    then Readed { source; value = None }
    else if tag = 1
    then
      let* v, source = readk source t in
      Readed { source; value = Some v }
    else Failed { source; error = "Unknown tag for Option" }
  | Headered { mkheader = _; headerencoding; encoding } ->
    let* header, source = readk source headerencoding in
    let encoding = encoding header in
    readk source encoding
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

let%expect_test _ =
  let w
      : type a.
        (string * int * int) Seq.t
        -> a Encoding.t
        -> (Format.formatter -> a -> unit)
        -> unit
    =
   fun sources e f ->
    let rec loop (blob, offset, length) sources k =
      match k blob offset length with
      | Readed { source = _; value } -> Format.printf "Ok: %a" f value
      | Failed { source = _; error } -> Format.printf "Error: %s" error
      | Suspended { source = _; cont } ->
        (match sources () with
        | Seq.Nil -> Format.printf "Error: not enough input"
        | Seq.Cons (source, sources) -> loop source sources cont)
    in
    match sources () with
    | Seq.Nil -> assert false
    | Seq.Cons (source, sources) ->
      loop source sources (fun blob offset length ->
          let source = mk_source blob offset length in
          readk source e)
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
    Encoding.(String (Unsigned.UInt32.of_int 3))
    Format.pp_print_string;
  [%expect {| Ok: FOO |}];
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
  w "FOO" Encoding.(String (Unsigned.UInt32.of_int 3)) Format.pp_print_string;
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
