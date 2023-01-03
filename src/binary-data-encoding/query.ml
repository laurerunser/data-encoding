let ( let* ) = Result.bind

let rec size_of : type t. t Encoding.t -> t -> (Optint.t, string) result =
 fun encoding v ->
  match encoding with
  | Unit -> Ok Optint.zero
  | Bool -> Ok Optint.one
  | Int64 -> Ok (Optint.of_int 8)
  | Int32 -> Ok (Optint.of_int 4)
  | UInt30 -> Ok (Optint.of_int 4)
  | UInt16 -> Ok (Optint.of_int 2)
  | UInt8 -> Ok Optint.one
  | String n -> Ok (Optint.of_int (n :> int))
  | Bytes n -> Ok (Optint.of_int (n :> int))
  | Option encoding ->
    (match v with
    | None -> Ok Optint.one
    | Some v ->
      let* size = size_of encoding v in
      Ok (Optint.add Optint.one size))
  | Headered { mkheader; headerencoding; encoding; equal = _ } ->
    let* header = mkheader v in
    let* headersize = size_of headerencoding header in
    let* encoding = encoding header in
    let* payloadsize = size_of encoding v in
    Ok (Optint.add headersize payloadsize)
  | Conv { serialisation; deserialisation = _; encoding } ->
    size_of encoding (serialisation v)
  | [] -> Ok Optint.zero
  | ehead :: etail ->
    (match v with
    | vhead :: vtail ->
      let* shead = size_of ehead vhead in
      let* stail = size_of etail vtail in
      Ok (Optint.add shead stail))
;;

let%expect_test _ =
  let w : type a. a Encoding.t -> a -> unit =
   fun e v ->
    match size_of e v with
    | Ok s -> Format.printf "%a\n" Optint.pp s
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
  w Encoding.[ string `UInt30; unit ] [ "FOO"; () ];
  [%expect {| 7 |}];
  w Encoding.[ string `UInt16; unit ] [ "FOO"; () ];
  [%expect {| 5 |}];
  w Encoding.[ string `UInt8; unit ] [ "FOO"; () ];
  [%expect {| 4 |}];
  w
    Encoding.[ string (`Fixed (Option.get @@ Sizedints.Uint30.of_int 3)); unit ]
    [ "FOO"; () ];
  [%expect {| 3 |}];
  w Encoding.(option int32) None;
  [%expect {| 1 |}];
  w Encoding.(option int32) (Some 0xff_ffl);
  [%expect {| 5 |}];
  ()
;;

let rec maximum_size_of : type t. t Encoding.t -> Optint.t =
 fun encoding ->
  match encoding with
  | Unit -> Optint.zero
  | Bool -> Optint.one
  | Int64 -> Optint.of_int 8
  | Int32 -> Optint.of_int 4
  | UInt30 -> Optint.of_int 4
  | UInt16 -> Optint.of_int 2
  | UInt8 -> Optint.one
  | String n -> Optint.of_int (n :> int)
  | Bytes n -> Optint.of_int (n :> int)
  | Option encoding -> Optint.add Optint.one (maximum_size_of encoding)
  | Headered _ -> failwith "TODO"
  | Conv { serialisation = _; deserialisation = _; encoding } -> maximum_size_of encoding
  | [] -> Optint.zero
  | head :: tail -> Optint.add (maximum_size_of head) (maximum_size_of tail)
;;

let%expect_test _ =
  let w : type a. a Encoding.t -> unit =
   fun e -> Format.printf "%a\n" Optint.pp (maximum_size_of e)
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
