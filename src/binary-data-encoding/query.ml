let ( let* ) = Result.bind

let rec size_of : type t. t Descr.t -> t -> (Optint.Int63.t, string) result =
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
  let w : type a. a Descr.t -> a -> unit =
   fun e v ->
    match size_of e v with
    | Ok s -> Format.printf "%a\n" Optint.Int63.pp s
    | Error msg -> Format.printf "Error: %s\n" msg
  in
  w Descr.Unit ();
  [%expect {| 0 |}];
  w Descr.[ Unit; Unit ] [ (); () ];
  [%expect {| 0 |}];
  w Descr.(Numeral { numeral = Int64; endianness = Big_endian }) 0x00L;
  [%expect {| 8 |}];
  w Descr.(Option (Numeral { numeral = Int32; endianness = Big_endian })) None;
  [%expect {| 1 |}];
  w Descr.(Option (Numeral { numeral = Int32; endianness = Big_endian })) (Some 0xff_ffl);
  [%expect {| 5 |}];
  ()
;;

let rec maximum_size_of : type t. t Descr.t -> Optint.Int63.t =
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
  let w : type a. a Descr.t -> unit =
   fun e -> Format.printf "%a\n" Optint.Int63.pp (maximum_size_of e)
  in
  w Descr.Unit;
  [%expect {| 0 |}];
  w Descr.[ Unit; Unit ];
  [%expect {| 0 |}];
  w Descr.(Numeral { numeral = Int64; endianness = Big_endian });
  [%expect {| 8 |}];
  w Descr.(Option (Numeral { numeral = Int32; endianness = Big_endian }));
  [%expect {| 5 |}];
  ()
;;

let rec equal_of : type t. t Descr.t -> t -> t -> bool =
 fun encoding ->
  match encoding with
  | Unit -> Unit.equal
  | Bool -> Bool.equal
  | Numeral { numeral = Int64; endianness = _ } -> Int64.equal
  | Numeral { numeral = Int32; endianness = _ } -> Int32.equal
  | Numeral { numeral = UInt62; endianness = _ } ->
    fun a b -> Optint.Int63.equal (a :> Optint.Int63.t) (b :> Optint.Int63.t)
  | Numeral { numeral = UInt30; endianness = _ } ->
    fun a b -> Int.equal (a :> int) (b :> int)
  | Numeral { numeral = UInt16; endianness = _ } ->
    fun a b -> Int.equal (a :> int) (b :> int)
  | Numeral { numeral = UInt8; endianness = _ } ->
    fun a b -> Int.equal (a :> int) (b :> int)
  | String _ -> String.equal
  | Bytes _ -> Bytes.equal
  | Option t ->
    let t = equal_of t in
    Option.equal t
  | Headered { mkheader = _; headerencoding = _; mkencoding = _; equal; maximum_size = _ }
    -> equal
  | Fold
      { chunkencoding = _
      ; chunkify = _
      ; readinit = _
      ; reducer = _
      ; equal
      ; maximum_size = _
      } -> equal
  | Conv { serialisation; deserialisation = _; encoding } ->
    fun x y -> (equal_of encoding) (serialisation x) (serialisation y)
  | [] -> fun [] [] -> true
  | head :: tail ->
    let head = equal_of head in
    let tail = equal_of tail in
    fun (ah :: at) (bh :: bt) -> head ah bh && tail at bt
;;

let rec pp_of : type t. t Descr.t -> Format.formatter -> t -> unit =
 fun encoding fmt v ->
  match encoding with
  | Unit -> Format.fprintf fmt "()"
  | Bool -> Format.fprintf fmt "%b" v
  | Numeral { numeral = Int64; endianness = _ } -> Format.fprintf fmt "%Ld" v
  | Numeral { numeral = Int32; endianness = _ } -> Format.fprintf fmt "%ld" v
  | Numeral { numeral = UInt62; endianness = _ } ->
    Format.fprintf fmt "%Ld" (Optint.Int63.to_int64 (v :> Optint.Int63.t))
  | Numeral { numeral = UInt30; endianness = _ } -> Format.fprintf fmt "%d" (v :> int)
  | Numeral { numeral = UInt16; endianness = _ } -> Format.fprintf fmt "%d" (v :> int)
  | Numeral { numeral = UInt8; endianness = _ } -> Format.fprintf fmt "%d" (v :> int)
  | String _ -> Format.fprintf fmt "%s" v
  | Bytes _ -> Format.fprintf fmt "%s" (Bytes.unsafe_to_string v)
  | Option t ->
    (match v with
    | None -> Format.fprintf fmt "None"
    | Some v ->
      let pp = pp_of t in
      Format.fprintf fmt "Some(%a)" pp v)
  | Headered { mkheader; headerencoding = _; mkencoding; equal = _; maximum_size = _ } ->
    let ( let* ) = Result.bind in
    (match
       let* header = mkheader v in
       let* encoding = mkencoding header in
       Ok encoding
     with
    | Ok encoding ->
      let pp = pp_of encoding in
      Format.fprintf fmt "%a" pp v
    | Error msg -> Format.fprintf fmt "Error: %s" msg)
  | Fold
      { chunkencoding; chunkify; readinit = _; reducer = _; equal = _; maximum_size = _ }
    ->
    Format.fprintf
      fmt
      "chunked(%a)"
      (Format.pp_print_seq
         ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ',')
         (pp_of chunkencoding))
      (chunkify v)
  | Conv { serialisation; deserialisation = _; encoding } ->
    let pp fmt v = pp_of encoding fmt (serialisation v) in
    Format.fprintf fmt "conved(%a)" pp v
  | [] -> ()
  | [ head ] ->
    let [ v ] = v in
    let pp = pp_of head in
    Format.fprintf fmt "%a" pp v
  | head :: tail ->
    let (v :: vs) = v in
    let pph = pp_of head in
    let ppt = pp_of tail in
    Format.fprintf fmt "%a;%a" pph v ppt vs
;;
