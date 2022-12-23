let ( let* ) = Result.bind

let rec size_of : type t. t Encoding.t -> t -> (Unsigned.UInt32.t, string) result =
 fun encoding v ->
  match encoding with
  | Unit -> Ok Unsigned.UInt32.zero
  | Int64 -> Ok (Unsigned.UInt32.of_int 8)
  | UInt64 -> Ok (Unsigned.UInt32.of_int 8)
  | Int32 -> Ok (Unsigned.UInt32.of_int 4)
  | UInt32 -> Ok (Unsigned.UInt32.of_int 4)
  | UInt16 -> Ok (Unsigned.UInt32.of_int 2)
  | UInt8 -> Ok (Unsigned.UInt32.of_int 1)
  | String n -> Ok n
  | Bytes n -> Ok n
  | Option encoding ->
    (match v with
    | None -> Ok Unsigned.UInt32.one
    | Some v ->
      let* size = size_of encoding v in
      Ok (Unsigned.UInt32.add Unsigned.UInt32.one size))
  | Headered { mkheader; headerencoding; encoding } ->
    let* header = mkheader v in
    let* headersize = size_of headerencoding header in
    let* encoding = encoding header in
    let* payloadsize = size_of encoding v in
    Ok (Unsigned.UInt32.add headersize payloadsize)
  | Conv { serialisation; deserialisation = _; encoding } ->
    size_of encoding (serialisation v)
  | [] -> Ok Unsigned.UInt32.zero
  | ehead :: etail ->
    (match v with
    | vhead :: vtail ->
      let* shead = size_of ehead vhead in
      let* stail = size_of etail vtail in
      Ok (Unsigned.UInt32.add shead stail))
;;

let%expect_test _ =
  let w : type a. a Encoding.t -> a -> unit =
   fun e v ->
    match size_of e v with
    | Ok s -> Format.printf "%d\n" (Unsigned.UInt32.to_int s)
    | Error msg -> Format.printf "Error: %s\n" msg
  in
  w Encoding.unit ();
  [%expect {| 0 |}];
  w Encoding.int64 0x00L;
  [%expect {| 8 |}];
  w Encoding.int64 0xffL;
  [%expect {| 8 |}];
  w Encoding.[ unit; unit; int32; unit; int32 ] [ (); (); 0x00l; (); 0x00l ];
  [%expect {| 8 |}];
  w Encoding.[ string `UInt32; unit ] [ "FOO"; () ];
  [%expect {| 7 |}];
  w Encoding.[ string `UInt16; unit ] [ "FOO"; () ];
  [%expect {| 5 |}];
  w Encoding.[ string `UInt8; unit ] [ "FOO"; () ];
  [%expect {| 4 |}];
  w Encoding.[ string (`Fixed (Unsigned.UInt32.of_int 3)); unit ] [ "FOO"; () ];
  [%expect {| 3 |}];
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
  | UInt8 -> Unsigned.UInt32.of_int 1
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
