let rec size_of : type t. t Encoding.t -> t -> Unsigned.UInt32.t =
 fun encoding v ->
  match encoding with
  | Unit -> Unsigned.UInt32.zero
  | Int64 -> Unsigned.UInt32.of_int 8
  | UInt64 -> Unsigned.UInt32.of_int 8
  | Int32 -> Unsigned.UInt32.of_int 4
  | UInt32 -> Unsigned.UInt32.of_int 4
  | UInt16 -> Unsigned.UInt32.of_int 2
  | String n -> n
  | Bytes n -> n
  | Option encoding ->
    (match v with
    | None -> Unsigned.UInt32.one
    | Some v -> Unsigned.UInt32.add Unsigned.UInt32.one (size_of encoding v))
  | Headered { mkheader; headerencoding; encoding } ->
    let header = mkheader v in
    Unsigned.UInt32.add (size_of headerencoding header) (size_of (encoding header) v)
  | Conv { serialisation; deserialisation = _; encoding } ->
    size_of encoding (serialisation v)
  | [] -> Unsigned.UInt32.zero
  | ehead :: etail ->
    (match v with
    | vhead :: vtail -> Unsigned.UInt32.add (size_of ehead vhead) (size_of etail vtail))
;;

let%expect_test _ =
  let w : type a. a Encoding.t -> a -> unit =
   fun e v -> Format.printf "%d\n" (Unsigned.UInt32.to_int @@ size_of e v)
  in
  w Encoding.unit ();
  [%expect {| 0 |}];
  w Encoding.int64 0x00L;
  [%expect {| 8 |}];
  w Encoding.int64 0xffL;
  [%expect {| 8 |}];
  w Encoding.[ unit; unit; int32; unit; int32 ] [ (); (); 0x00l; (); 0x00l ];
  [%expect {| 8 |}];
  w Encoding.[ string; unit ] [ "FOO"; () ];
  [%expect {| 7 |}];
  w Encoding.(option int32) None;
  [%expect {| 1 |}];
  w Encoding.(option int32) (Some 0xff_ffl);
  [%expect {| 5 |}];
  ()
;;

let rec maximum_size_of : type t. t Encoding.t -> Unsigned.UInt32.t =
 fun encoding ->
  match encoding with
  | Unit -> Unsigned.UInt32.zero
  | Int64 -> Unsigned.UInt32.of_int 8
  | UInt64 -> Unsigned.UInt32.of_int 8
  | Int32 -> Unsigned.UInt32.of_int 4
  | UInt32 -> Unsigned.UInt32.of_int 4
  | UInt16 -> Unsigned.UInt32.of_int 2
  | String n -> n
  | Bytes n -> n
  | Option encoding -> Unsigned.UInt32.add Unsigned.UInt32.one (maximum_size_of encoding)
  | Headered _ -> failwith "TODO"
  | Conv { serialisation = _; deserialisation = _; encoding } -> maximum_size_of encoding
  | [] -> Unsigned.UInt32.zero
  | head :: tail -> Unsigned.UInt32.add (maximum_size_of head) (maximum_size_of tail)
;;

let%expect_test _ =
  let w : type a. a Encoding.t -> unit =
   fun e -> Format.printf "%d\n" (Unsigned.UInt32.to_int @@ maximum_size_of e)
  in
  w Encoding.unit;
  [%expect {| 0 |}];
  w Encoding.int64;
  [%expect {| 8 |}];
  w Encoding.[ unit; unit; int32; unit; int32 ];
  [%expect {| 8 |}];
  w Encoding.(option int32);
  [%expect {| 5 |}];
  ()
;;
