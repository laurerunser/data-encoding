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
  if offset < 0
  then Error (0, "offset is negative")
  else if offset + maximum_length > Bytes.length dst
  then Error (0, "maximum-length exceeds buffer size")
  else (
    match encoding with
    | Unit -> Ok 0
    | Int64 ->
      if maximum_length < 8
      then Error (0, "length limited exceeded")
      else (
        Endian.set_int64 dst offset v;
        Ok 8))
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
  ()
;;

let read
    : type a.
      src:string -> offset:int -> maximum_length:int -> a Encoding.t -> (a, string) result
  =
 fun ~src ~offset ~maximum_length encoding ->
  if offset < 0
  then Error "offset is negative"
  else if offset + maximum_length > String.length src
  then Error "maximum-length exceeds buffer size"
  else (
    match encoding with
    | Unit -> Ok ()
    | Int64 ->
      if maximum_length < 8
      then Error "length limited exceeded"
      else (
        let v = Endian.get_int64_string src offset in
        Ok v))
;;
