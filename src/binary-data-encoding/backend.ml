let rec write
    : type a.
      dst:bytes
      -> offset:int
      -> maximum_length:int
      -> int
      -> a Encoding.t
      -> a
      -> (int, int * string) result
  =
 fun ~dst ~offset ~maximum_length written encoding v ->
  if offset < 0
  then Error (0, "offset is negative")
  else if offset + maximum_length > Bytes.length dst
  then Error (0, "maximum-length exceeds buffer size")
  else (
    match encoding with
    | Unit -> Ok written
    | Int64 ->
      if maximum_length < 8
      then Error (0, "length limited exceeded")
      else (
        Endian.set_int64 dst offset v;
        let written = written + 8 in
        Ok written)
    | UInt64 ->
      if maximum_length < 8
      then Error (0, "length limited exceeded")
      else (
        let v = Unsigned.UInt64.to_int64 v in
        Endian.set_int64 dst offset v;
        let written = written + 8 in
        Ok written)
    | Int32 ->
      if maximum_length < 4
      then Error (0, "length limited exceeded")
      else (
        Endian.set_int32 dst offset v;
        let written = written + 4 in
        Ok written)
    | UInt32 ->
      if maximum_length < 4
      then Error (0, "length limited exceeded")
      else (
        let v = Unsigned.UInt32.to_int32 v in
        Endian.set_int32 dst offset v;
        let written = written + 4 in
        Ok written)
    | UInt16 ->
      if maximum_length < 2
      then Error (0, "length limited exceeded")
      else (
        let v = Unsigned.UInt16.to_int v in
        Endian.set_int16 dst offset v;
        let written = written + 2 in
        Ok written)
    | [] ->
      assert (v = []);
      Ok written
    | t :: ts ->
      (match v with
      | v :: vs ->
        (match write ~dst ~offset ~maximum_length 0 t v with
        | Error _ as err -> err
        | Ok written_t_v ->
          let written = written + written_t_v in
          let offset = offset + written_t_v in
          let maximum_length = maximum_length - written_t_v in
          write ~dst ~offset ~maximum_length written ts vs)))
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
 fun ~dst ~offset ~maximum_length encoding v ->
  write ~dst ~offset ~maximum_length 0 encoding v
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
  [%expect {| Ok(8): _ÿÿÿÿÿÿÿÿ_ |}];
  w
    Encoding.[ unit; unit; int32; unit; int32 ]
    [ (); (); 0x4c_6f_6f_4cl; (); 0x4c_6f_6f_4cl ];
  [%expect {| Ok(8): _LooLLooL_ |}];
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
