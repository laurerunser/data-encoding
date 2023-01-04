let ( let* ) = Result.bind

let rec size_of : type t. t Encoding.t -> t -> (Optint.Int63.t, string) result =
 fun encoding v ->
  match encoding with
  | Unit -> Ok Optint.Int63.zero
  | Bool -> Ok Optint.Int63.one
  | Numeral { numeral = Int64; endianness = _ } -> Ok (Optint.Int63.of_int 8)
  | Numeral { numeral = Int32; endianness = _ } -> Ok (Optint.Int63.of_int 4)
  | Numeral { numeral = UInt62; endianness = _ } -> Ok (Optint.Int63.of_int 8)
  | Numeral { numeral = UInt30; endianness = _ } -> Ok (Optint.Int63.of_int 4)
  | Numeral { numeral = UInt16; endianness = _ } -> Ok (Optint.Int63.of_int 2)
  | Numeral { numeral = UInt8; endianness = _ } -> Ok Optint.Int63.one
  | String n -> Ok (n :> Optint.Int63.t)
  | Bytes n -> Ok (n :> Optint.Int63.t)
  | Option encoding ->
    (match v with
    | None -> Ok Optint.Int63.one
    | Some v ->
      let* size = size_of encoding v in
      Ok (Optint.Int63.add Optint.Int63.one size))
  | Headered { mkheader; headerencoding; mkencoding; equal = _; maximum_size = _ } ->
    let* header = mkheader v in
    let* headersize = size_of headerencoding header in
    let* encoding = mkencoding header in
    let* payloadsize = size_of encoding v in
    Ok (Optint.Int63.add headersize payloadsize)
  | Fold
      { chunkencoding; chunkify; readinit = _; reducer = _; equal = _; maximum_size = _ }
    ->
    let chunks = chunkify v in
    let rec fold acc s =
      match s () with
      | Seq.Nil -> Ok acc
      | Seq.Cons (chunk, s) ->
        (match size_of chunkencoding chunk with
        | Ok chunksize ->
          let acc = Optint.Int63.add chunksize chunksize in
          fold acc s
        | Error e -> Error e)
    in
    fold Optint.Int63.zero chunks
  | Conv { serialisation; deserialisation = _; encoding } ->
    size_of encoding (serialisation v)
  | [] -> Ok Optint.Int63.zero
  | ehead :: etail ->
    (match v with
    | vhead :: vtail ->
      let* shead = size_of ehead vhead in
      let* stail = size_of etail vtail in
      Ok (Optint.Int63.add shead stail))
;;

let%expect_test _ =
  let w : type a. a Encoding.t -> a -> unit =
   fun e v ->
    match size_of e v with
    | Ok s -> Format.printf "%a\n" Optint.Int63.pp s
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
    Encoding.[ string (`Fixed (Option.get @@ Sizedints.Uint62.of_int64 3L)); unit ]
    [ "FOO"; () ];
  [%expect {| 3 |}];
  w Encoding.(option int32) None;
  [%expect {| 1 |}];
  w Encoding.(option int32) (Some 0xff_ffl);
  [%expect {| 5 |}];
  ()
;;

let rec maximum_size_of : type t. t Encoding.t -> Optint.Int63.t =
 fun encoding ->
  match encoding with
  | Unit -> Optint.Int63.zero
  | Bool -> Optint.Int63.one
  | Numeral { numeral = Int64; endianness = _ } -> Optint.Int63.of_int 8
  | Numeral { numeral = Int32; endianness = _ } -> Optint.Int63.of_int 4
  | Numeral { numeral = UInt62; endianness = _ } -> Optint.Int63.of_int 8
  | Numeral { numeral = UInt30; endianness = _ } -> Optint.Int63.of_int 4
  | Numeral { numeral = UInt16; endianness = _ } -> Optint.Int63.of_int 2
  | Numeral { numeral = UInt8; endianness = _ } -> Optint.Int63.one
  | String n -> (n :> Optint.Int63.t)
  | Bytes n -> (n :> Optint.Int63.t)
  | Option encoding -> Optint.Int63.add Optint.Int63.one (maximum_size_of encoding)
  | Headered { mkheader = _; headerencoding; mkencoding = _; equal = _; maximum_size } ->
    Optint.Int63.add (maximum_size_of headerencoding) maximum_size
  | Fold
      { chunkencoding = _
      ; chunkify = _
      ; readinit = _
      ; reducer = _
      ; equal = _
      ; maximum_size
      } -> maximum_size
  | Conv { serialisation = _; deserialisation = _; encoding } -> maximum_size_of encoding
  | [] -> Optint.Int63.zero
  | head :: tail -> Optint.Int63.add (maximum_size_of head) (maximum_size_of tail)
;;

let%expect_test _ =
  let w : type a. a Encoding.t -> unit =
   fun e -> Format.printf "%a\n" Optint.Int63.pp (maximum_size_of e)
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
