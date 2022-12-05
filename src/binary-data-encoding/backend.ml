(* a [destination] is a buffer (with metadata) that the basic serialisation
   function writes into *)
type destination =
  { buffer : bytes (* the bytes to write into *)
  ; offset : int (* the offset at which to start *)
  ; written : int (* the count of bytes already written into the buffer *)
  ; maximum_length : int
        (* the maximum number of bytes to write; note that
           [maximum_length] can be greater than [buffer] length in which case
           the serialisation function will use the [Suspended] mechanism (see
           below) to serialise over multiple buffers. *)
  }

let mk_destination ?(maximum_length = max_int) buffer offset =
  { buffer; offset; written = 0; maximum_length }
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
      ; cont : bytes -> int -> written
      }

let destination_too_small_to_continue_message =
  "new destination buffer is too small to continue"
;;

let write1 destination writing write =
  if destination.written + writing > destination.maximum_length
  then Failed { destination; error = "maximum-length exceeded" }
  else if destination.offset + destination.written + writing
          > Bytes.length destination.buffer
  then
    Suspended
      { destination
      ; cont =
          (fun buffer offset ->
            let destination =
              mk_destination
                ~maximum_length:(destination.maximum_length - destination.written)
                buffer
                offset
            in
            if destination.offset + writing > Bytes.length destination.buffer
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
    let cont buffer offset =
      let* x = cont buffer offset in
      f x
    in
    Suspended { destination; cont }
;;

let rec writek : type a. destination -> a Encoding.t -> a -> written =
 fun destination encoding v ->
  assert (destination.offset >= 0);
  assert (destination.offset < Bytes.length destination.buffer);
  if destination.offset < 0
  then Failed { destination; error = "offset is negative" }
  else if destination.offset >= Bytes.length destination.buffer
  then Failed { destination; error = "offset is beyond length of buffer" }
  else (
    match encoding with
    | Unit -> Written { destination }
    | Int64 ->
      write1 destination Size.int64 (fun buffer offset ->
          Endian.set_int64 buffer offset v)
    | UInt64 ->
      write1 destination Size.uint64 (fun buffer offset ->
          let v = Unsigned.UInt64.to_int64 v in
          Endian.set_int64 buffer offset v)
    | Int32 ->
      write1 destination Size.int32 (fun buffer offset ->
          Endian.set_int32 buffer offset v)
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
      else
        write1 destination n (fun buffer offset -> Bytes.blit_string v 0 buffer offset n)
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
        writek destination ts vs))
;;

let writek : type a. destination -> a Encoding.t -> a -> written =
 fun destination encoding v ->
  if destination.offset < 0
  then Failed { destination; error = "offset is negative" }
  else if destination.offset >= Bytes.length destination.buffer
  then Failed { destination; error = "offset is beyond length of buffer" }
  else writek destination encoding v
;;

let write
    : type a.
      dst:bytes
      -> offset:int
      -> maximum_length:int
      -> a Encoding.t
      -> a
      -> (int, int * string) result
  =
 fun ~dst:buffer ~offset ~maximum_length encoding v ->
  if offset + maximum_length > Bytes.length buffer
  then Error (0, "maximum-length exceeds buffer size")
  else (
    let destination = { buffer; written = 0; offset; maximum_length } in
    match writek destination encoding v with
    | Suspended _ -> assert false (* the buffer is bigger than max-length *)
    | Failed { destination; error } -> Error (destination.written, error)
    | Written { destination } -> Ok destination.written)
;;

let%expect_test _ =
  let scratch = String.make 10 '_' in
  let w : type a. a Encoding.t -> a -> unit =
   fun e v ->
    let dst = Bytes.of_string scratch in
    match write ~dst ~offset:1 ~maximum_length:8 e v with
    | Ok n -> Format.printf "Ok(%d): %s\n" n (Bytes.to_string dst)
    | Error (n, s) -> Format.printf "Error(%d,%s): %s" n s (Bytes.to_string dst)
  in
  w Encoding.unit ();
  [%expect {| Ok(0): __________ |}];
  w Encoding.int64 0x4c_6f_6f_6f_6f_6f_6f_4cL;
  [%expect {| Ok(8): _LooooooL_ |}];
  w Encoding.int64 0xff_ff_ff_ff_ff_ff_ff_ffL;
  [%expect {| Ok(8): _��������_ |}];
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
    match k buffer 0 with
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
    chunk buffer [] (fun buffer offset ->
        let destination = { buffer; offset; written = 0; maximum_length = max_int } in
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
    | Ok s -> Format.printf "Ok: %s\n" s
    | Error s -> Format.printf "Error: %s" s
  in
  w ~buffer_size:10 Encoding.unit ();
  [%expect {| Ok: |}];
  w ~buffer_size:10 Encoding.int64 0x4c_6f_6f_6f_6f_6f_6f_4cL;
  [%expect {| Ok: LooooooL |}];
  w ~buffer_size:5 Encoding.int64 0x4c_6f_6f_6f_6f_6f_6f_4cL;
  [%expect {| Ok: LooooooL |}];
  w ~buffer_size:10 Encoding.int64 0xff_ff_ff_ff_ff_ff_ff_ffL;
  [%expect {| Ok: �������� |}];
  w
    ~buffer_size:10
    Encoding.[ unit; unit; int32; unit; int32 ]
    [ (); (); 0x4c_6f_6f_4cl; (); 0x4c_6f_6f_4cl ];
  [%expect {| Ok: LooLLooL |}];
  w
    ~buffer_size:10
    Encoding.[ String (Unsigned.UInt32.of_int 3); String (Unsigned.UInt32.of_int 3) ]
    [ "FOO"; "LOL" ];
  [%expect {| Ok: FOOLOL |}];
  w
    ~buffer_size:2
    Encoding.[ String (Unsigned.UInt32.of_int 3); String (Unsigned.UInt32.of_int 3) ]
    [ "FOO"; "LOL" ];
  [%expect {| Ok: FOOLOL |}];
  w
    ~buffer_size:2
    Encoding.
      [ String (Unsigned.UInt32.of_int 3); unit; unit; String (Unsigned.UInt32.of_int 8) ]
    [ "FOO"; (); (); "LOLLOLOL" ];
  [%expect {| Ok: FOOLOLLOLOL |}];
  ()
;;

let rec read
    : type a.
      src:string
      -> offset:int
      -> maximum_length:int
      -> a Encoding.t
      -> (a * int, string) result
  =
 fun ~src ~offset ~maximum_length encoding ->
  if offset < 0
  then Error "offset is negative"
  else if offset + maximum_length > String.length src
  then Error "maximum-length exceeds buffer size"
  else (
    match encoding with
    | Unit -> Ok ((), 0)
    | Int64 ->
      if maximum_length < 8
      then Error "length limited exceeded"
      else (
        let v = Endian.get_int64_string src offset in
        Ok (v, 8))
    | UInt64 ->
      if maximum_length < 8
      then Error "length limited exceeded"
      else (
        let v = Endian.get_int64_string src offset in
        let v = Unsigned.UInt64.of_int64 v in
        Ok (v, 8))
    | Int32 ->
      if maximum_length < 4
      then Error "length limited exceeded"
      else (
        let v = Endian.get_int32_string src offset in
        Ok (v, 4))
    | UInt32 ->
      if maximum_length < 4
      then Error "length limited exceeded"
      else (
        let v = Endian.get_int32_string src offset in
        let v = Unsigned.UInt32.of_int32 v in
        Ok (v, 4))
    | UInt16 ->
      if maximum_length < 2
      then Error "length limited exceeded"
      else (
        let v = Endian.get_int16_string src offset in
        let v = Unsigned.UInt16.of_int v in
        Ok (v, 2))
    | String n ->
      let n = Unsigned.UInt32.to_int n in
      if maximum_length < n
      then Error "length limited exceeded"
      else (
        let s = String.sub src offset n in
        Ok (s, n))
    | Bytes n ->
      let n = Unsigned.UInt32.to_int n in
      if maximum_length < n
      then Error "length limited exceeded"
      else (
        let s = Bytes.unsafe_of_string @@ String.sub src offset n in
        Ok (s, n))
    | Option t ->
      if maximum_length < 1
      then Error "length limited exceeded"
      else (
        let n = Endian.get_uint8_string src offset in
        let offset = offset + 1 in
        let maximum_length = maximum_length - 1 in
        if n = 0
        then Ok (None, 1)
        else if n = 1
        then (
          match read ~src ~offset ~maximum_length t with
          | Error _ as err -> err
          | Ok (v, readed) -> Ok (Some v, 1 + readed))
        else Error "Unknown tag for Option")
    | Headered { mkheader = _; headerencoding; encoding } ->
      (match read ~src ~offset ~maximum_length headerencoding with
      | Error _ as err -> err
      | Ok (header, readed_header) ->
        let offset = offset + readed_header in
        let maximum_length = maximum_length - readed_header in
        let encoding = encoding header in
        (match read ~src ~offset ~maximum_length encoding with
        | Error _ as err -> err
        | Ok (vs, readed) -> Ok (vs, readed_header + readed)))
    | [] -> Ok ([], 0)
    | t :: ts ->
      (match read ~src ~offset ~maximum_length t with
      | Error _ as err -> err
      | Ok (v, readed) ->
        let offset = offset + readed in
        let maximum_length = maximum_length - readed in
        (match read ~src ~offset ~maximum_length ts with
        | Error _ as err -> err
        | Ok (vs, readeds) -> Ok (v :: vs, readed + readeds))))
;;

let read
    : type a.
      src:string
      -> offset:int
      -> maximum_length:int
      -> a Encoding.t
      -> (a * int, string) result
  =
 fun ~src ~offset ~maximum_length encoding -> read ~src ~offset ~maximum_length encoding
;;
