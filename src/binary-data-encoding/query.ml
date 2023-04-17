let zero_of_numeral : type n. n Descr.numeral -> n = function
  | UInt8 -> Commons.Sizedints.Uint8.zero
  | UInt16 -> Commons.Sizedints.Uint16.zero
  | UInt30 -> Commons.Sizedints.Uint30.zero
  | UInt62 -> Commons.Sizedints.Uint62.zero
  | Int32 -> Int32.zero
  | Int64 -> Int64.zero
;;

let max_int_of : type n. n Descr.numeral -> Commons.Sizedints.Uint62.t = function
  | UInt8 -> Commons.Sizedints.Uint8.(to_uint62 max_int)
  | UInt16 -> Commons.Sizedints.Uint16.(to_uint62 max_int)
  | UInt30 -> Commons.Sizedints.Uint30.(to_uint62 max_int)
  | UInt62 -> Commons.Sizedints.Uint62.max_int
  | Int32 ->
    Option.get @@ Commons.Sizedints.Uint62.of_int64 (Int64.of_int32 Int32.max_int)
  | Int64 -> Option.get @@ Commons.Sizedints.Uint62.of_int64 Int64.max_int
;;

let size_of_numeral : type n. n Descr.numeral -> Commons.Sizedints.Uint62.t = function
  | UInt8 -> Size.As_uint62.uint8
  | UInt16 -> Size.As_uint62.uint16
  | UInt30 -> Size.As_uint62.uint30
  | UInt62 -> Size.As_uint62.uint62
  | Int32 -> Size.As_uint62.int32
  | Int64 -> Size.As_uint62.int64
;;

let numeral_of_int : type n. n Descr.numeral -> int -> n =
 fun n i ->
  (* TODO: handle overflow uniformly accross all numerals, possibly returning
     option *)
  match n with
  | UInt8 -> Option.get @@ Commons.Sizedints.Uint8.of_int i
  | UInt16 -> Option.get @@ Commons.Sizedints.Uint16.of_int i
  | UInt30 -> Option.get @@ Commons.Sizedints.Uint30.of_int i
  | UInt62 -> Option.get @@ Commons.Sizedints.Uint62.of_int i
  | Int32 -> Int32.of_int i
  | Int64 -> Int64.of_int i
;;

let int_of_numeral : type n. n Descr.numeral -> n -> int =
 fun n i ->
  (* TODO: handle 32 bit arch *)
  match n with
  | UInt8 -> (i :> int)
  | UInt16 -> (i :> int)
  | UInt30 -> (i :> int)
  | UInt62 -> Optint.Int63.to_int (i :> Optint.Int63.t)
  | Int32 -> Int32.to_int i
  | Int64 ->
    (* TODO: handle overflow *)
    Int64.to_int i
;;

let ( let* ) = Result.bind

let rec size_of : type s t. (s, t) Descr.t -> t -> (Optint.Int63.t, string) result =
 fun encoding v ->
  match encoding with
  | Unit -> Ok Optint.Int63.zero
  | Bool -> Ok Optint.Int63.one
  | Numeral { numeral; endianness = _ } -> Ok (size_of_numeral numeral :> Optint.Int63.t)
  | String n -> Ok (n :> Optint.Int63.t)
  | Bytes n -> Ok (n :> Optint.Int63.t)
  | LSeq { length; elementencoding } ->
    let length =
      (* TODO: catch overflow. *)
      Optint.Int63.to_int (length :> Optint.Int63.t)
    in
    (* TODO: classify the sizability of the elementencoding: if fixed just do a
       multiplication (and still check for size??) *)
    let rec fold len size s =
      match s () with
      | Seq.Nil ->
        if len < length
        then
          raise
            (Invalid_argument
               "data-encoding.binary.query.size_of: inconsistent Seq length");
        Ok size
      | Seq.Cons (elt, s) ->
        if len > length
        then
          raise
            (Invalid_argument
               "data-encoding.binary.query.size_of: inconsistent Seq length");
        let* elt_size = size_of elementencoding elt in
        fold (len + 1) (Optint.Int63.add size elt_size) s
    in
    fold 0 Optint.Int63.zero v.seq
  | USeq { elementencoding } ->
    let rec fold size s =
      match s () with
      | Seq.Nil -> Ok size
      | Seq.Cons (elt, s) ->
        let* elt_size = size_of elementencoding elt in
        fold (Optint.Int63.add size elt_size) s
    in
    fold Optint.Int63.zero v
  | Array { length; elementencoding } ->
    if Option.get @@ Commons.Sizedints.Uint62.of_int64 (Int64.of_int (Array.length v))
       <> length
    then
      raise
        (Invalid_argument "data-encoding.binary.query.size_of: inconsistent Array length");
    let length =
      (* TODO: catch overflow. Can overflow happen? Wouldn't that prevent the Array to even exist in the first place? *)
      Optint.Int63.to_int (length :> Optint.Int63.t)
    in
    (* TODO: classify the sizability of the elementencoding: if fixed just do a
       multiplication *)
    let rec fold acc index =
      if index >= length
      then Ok acc
      else
        let* size = size_of elementencoding (Array.get v index) in
        let acc = Optint.Int63.add acc size in
        fold acc (index + 1)
    in
    fold Optint.Int63.zero 0
  | Option (_, encoding) ->
    (match v with
     | None -> Ok Optint.Int63.one
     | Some v ->
       let* size = size_of encoding v in
       Ok (Optint.Int63.add Optint.Int63.one size))
  | Headered { mkheader; headerencoding; mkencoding; equal = _; maximum_size = _ } ->
    let* header = mkheader v in
    let* headersize = size_of headerencoding header in
    let* encoding = mkencoding header in
    let* payloadsize =
      match encoding with
      | EDynamic encoding -> size_of encoding v
      | EStatic encoding ->
        (* TODO? don't use v and use the sizability instead? *)
        size_of encoding v
    in
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
           let acc = Optint.Int63.add acc chunksize in
           fold acc s
         | Error e -> Error e)
    in
    fold Optint.Int63.zero chunks
  | Conv { serialisation; deserialisation = _; encoding } ->
    size_of encoding (serialisation v)
  | Size_headered { size; encoding } ->
    let sizesize = size_of_numeral size in
    let* payloadsize = size_of encoding v in
    if Optint.Int63.compare payloadsize (max_int_of size :> Optint.Int63.t) > 0
    then Error "Oversized payload for size header"
    else Ok (Optint.Int63.add (sizesize :> Optint.Int63.t) payloadsize)
  | Size_limit { at_most; encoding } ->
    let* size = size_of encoding v in
    (* TODO: earlier failing ? *)
    if size > (at_most :> Optint.Int63.t) then Error "size exceeds limit" else Ok size
  | Union { tag = tag_encoding; serialisation; deserialisation = _; cases = _ } ->
    let (AnyP ({ Descr.tag; encoding; _ }, payload)) = serialisation v in
    let* tag_size = size_of tag_encoding tag in
    let* payload_size = size_of encoding payload in
    (* TODO: overflow *)
    Ok (Optint.Int63.add tag_size payload_size)
  | TupNil -> Ok Optint.Int63.zero
  | TupCons (_, ehead, etail) ->
    (match v with
     | vhead :: vtail ->
       let* shead = size_of ehead vhead in
       let* stail = size_of etail vtail in
       Ok (Optint.Int63.add shead stail))
;;

let%expect_test _ =
  let w : type s a. (s, a) Descr.t -> a -> unit =
   fun e v ->
    match size_of e v with
    | Ok s -> Format.printf "%a\n" Optint.Int63.pp s
    | Error msg -> Format.printf "Error: %s\n" msg
  in
  w Descr.Unit ();
  [%expect {| 0 |}];
  w Descr.(TupCons (TAnyStatic, Unit, TupCons (TAnyStatic, Unit, TupNil))) [ (); () ];
  [%expect {| 0 |}];
  w Descr.(Numeral { numeral = Int64; endianness = Big_endian }) 0x00L;
  [%expect {| 8 |}];
  w Descr.(Option (OIntrinsic, Numeral { numeral = Int32; endianness = Big_endian })) None;
  [%expect {| 1 |}];
  w
    Descr.(Option (OIntrinsic, Numeral { numeral = Int32; endianness = Big_endian }))
    (Some 0xff_ffl);
  [%expect {| 5 |}];
  ()
;;

let rec maximum_size_of : type s t. (s, t) Descr.t -> Optint.Int63.t =
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
  | LSeq { length; elementencoding } ->
    Optint.Int63.mul (length :> Optint.Int63.t) (maximum_size_of elementencoding)
  | USeq { elementencoding = _ } -> Optint.Int63.max_int
  | Array { length; elementencoding } ->
    Optint.Int63.mul (length :> Optint.Int63.t) (maximum_size_of elementencoding)
  | Option (_, encoding) -> Optint.Int63.add Optint.Int63.one (maximum_size_of encoding)
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
  | Size_headered { size; encoding } ->
    let maxsize :> Optint.Int63.t = max_int_of size in
    let maxencodingsize = maximum_size_of encoding in
    if Optint.Int63.compare maxsize maxencodingsize < 0 then maxsize else maxencodingsize
  | Size_limit { at_most; encoding } ->
    let maxsize = maximum_size_of encoding in
    if Optint.Int63.compare (at_most :> Optint.Int63.t) maxsize < 0
    then
      (* TODO: a pedantic mode which errors here to warn that the max-size is
           not needed *)
      maxsize
    else (at_most :> Optint.Int63.t)
  | Union { tag; serialisation = _; deserialisation = _; cases } ->
    assert (cases <> []);
    let tag_size = maximum_size_of tag in
    let payload_size =
      match cases with
      | [] -> assert false
      | AnyC { tag = _; encoding; inject = _ } :: cases ->
        List.fold_left
          (fun sz (Descr.AnyC { tag = _; encoding; inject = _ }) ->
            let nsz = maximum_size_of encoding in
            if Optint.Int63.compare sz nsz > 0 then nsz else sz)
          (maximum_size_of encoding)
          cases
    in
    Optint.Int63.add tag_size payload_size
  | TupNil -> Optint.Int63.zero
  | TupCons (_, head, tail) ->
    Optint.Int63.add (maximum_size_of head) (maximum_size_of tail)
;;

let%expect_test _ =
  let w : type s a. (s, a) Descr.t -> unit =
   fun e -> Format.printf "%a\n" Optint.Int63.pp (maximum_size_of e)
  in
  w Descr.Unit;
  [%expect {| 0 |}];
  w Descr.(TupCons (TAnyStatic, Unit, TupCons (TAnyStatic, Unit, TupNil)));
  [%expect {| 0 |}];
  w Descr.(Numeral { numeral = Int64; endianness = Big_endian });
  [%expect {| 8 |}];
  w Descr.(Option (OIntrinsic, Numeral { numeral = Int32; endianness = Big_endian }));
  [%expect {| 5 |}];
  ()
;;

let rec equal_of : type s t. (s, t) Descr.t -> t -> t -> bool =
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
  | Array { length; elementencoding } ->
    fun a b ->
      let lengtha =
        Option.get @@ Commons.Sizedints.Uint62.of_int64 (Int64.of_int (Array.length a))
      in
      if Optint.Int63.compare (lengtha :> Optint.Int63.t) (length :> Optint.Int63.t) <> 0
      then
        raise
          (Invalid_argument
             "data-encoding.binary.query.equal_of[array]: Inconsistent array length");
      let lengthb =
        Option.get @@ Commons.Sizedints.Uint62.of_int64 (Int64.of_int (Array.length b))
      in
      if Optint.Int63.compare (lengthb :> Optint.Int63.t) (length :> Optint.Int63.t) <> 0
      then
        raise
          (Invalid_argument
             "data-encoding.binary.query.equal_of[array]: Inconsistent array length");
      let eq = equal_of elementencoding in
      Array.for_all2 eq a b
  | Option (_, t) ->
    let t = equal_of t in
    Option.equal t
  | LSeq { elementencoding; length = _ } ->
    fun s1 s2 -> Seq.equal (equal_of elementencoding) s1.seq s2.seq
  | USeq { elementencoding } -> fun s1 s2 -> Seq.equal (equal_of elementencoding) s1 s2
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
  | Size_headered { size = _; encoding } -> equal_of encoding
  | Size_limit { at_most = _; encoding } -> equal_of encoding
  | Union { tag; serialisation; deserialisation = _; cases = _ } ->
    fun v1 v2 ->
      let (AnyP (descr1, payload1)) = serialisation v1 in
      let (AnyP (descr2, payload2)) = serialisation v2 in
      equal_of tag descr1.Descr.tag descr2.Descr.tag
      &&
      ((* TODO: equality witness if tag equality is checked *)
       ignore payload1;
       ignore payload2;
       true)
  | TupNil -> fun [] [] -> true
  | TupCons (_, head, tail) ->
    fun (ah :: at) (bh :: bt) ->
      let head = equal_of head in
      head ah bh
      &&
      let tail = equal_of tail in
      tail at bt
;;

let rec pp_of : type s t. (s, t) Descr.t -> Format.formatter -> t -> unit =
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
  | String _ -> Format.fprintf fmt "%S" v
  | Bytes _ -> Format.fprintf fmt "%S" (Bytes.unsafe_to_string v)
  | Array { length = _; elementencoding } ->
    Format.(
      fprintf
        fmt
        "[|%a|]"
        (pp_print_seq
           ~pp_sep:(fun fmt () -> pp_print_char fmt ';')
           (pp_of elementencoding))
        (Array.to_seq v))
  | Option (_, t) ->
    (match v with
     | None -> Format.fprintf fmt "None"
     | Some v ->
       let pp = pp_of t in
       Format.fprintf fmt "Some(%a)" pp v)
  | LSeq { length = _; elementencoding } ->
    let { Descr.seq; length = _ } = v in
    Format.fprintf
      fmt
      "seq(%a)"
      Format.(
        pp_print_seq ~pp_sep:(fun fmt () -> pp_print_char fmt ',') (pp_of elementencoding))
      seq
  | USeq { elementencoding } ->
    Format.fprintf
      fmt
      "seq(%a)"
      Format.(
        pp_print_seq ~pp_sep:(fun fmt () -> pp_print_char fmt ',') (pp_of elementencoding))
      v
  | Headered { mkheader; headerencoding = _; mkencoding; equal = _; maximum_size = _ } ->
    let ( let* ) = Result.bind in
    (match
       let* header = mkheader v in
       let* encoding = mkencoding header in
       Ok encoding
     with
     | Ok (EDynamic encoding) ->
       let pp = pp_of encoding in
       Format.fprintf fmt "%a" pp v
     | Ok (EStatic encoding) ->
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
  | Size_headered { size = _; encoding } -> pp_of encoding fmt v
  | Size_limit { at_most = _; encoding } -> pp_of encoding fmt v
  | Union { tag = tag_encoding; serialisation; deserialisation = _; cases = _ } ->
    let (AnyP ({ Descr.tag; encoding; inject = _ }, payload)) = serialisation v in
    Format.fprintf fmt "case(%a:%a)" (pp_of tag_encoding) tag (pp_of encoding) payload
  | TupNil -> ()
  | TupCons (_, head, TupNil) ->
    let [ v ] = v in
    let pp = pp_of head in
    Format.fprintf fmt "%a" pp v
  | TupCons (_, head, tail) ->
    let (v :: vs) = v in
    let pph = pp_of head in
    let ppt = pp_of tail in
    Format.fprintf fmt "%a;%a" pph v ppt vs
;;

let rec sizability : type s a. (s, a) Descr.t -> s = function
  | Unit -> Intrinsic (Static Commons.Sizedints.Uint62.zero)
  | Bool -> Intrinsic (Static Size.As_uint62.bool)
  | Numeral { numeral; endianness = _ } -> Intrinsic (Static (size_of_numeral numeral))
  | String n -> Intrinsic (Static n)
  | Bytes n -> Intrinsic (Static n)
  | Array { length; elementencoding } ->
    (match sizability elementencoding with
     | Intrinsic (Static n) ->
       (* TODO: catch overflow *)
       let s = Commons.Sizedints.Uint62.mul length n in
       Sizability.Intrinsic (Static s)
     | Intrinsic Dynamic -> Intrinsic Dynamic)
  | Option (optionator, _encoding) ->
    (match optionator with
     | OIntrinsic -> Intrinsic Dynamic
     | OExtrinsic -> Extrinsic)
  | LSeq { length; elementencoding } ->
    (match sizability elementencoding with
     | Intrinsic (Static n) ->
       (* TODO: catch overflow *)
       let s = Commons.Sizedints.Uint62.mul length n in
       Sizability.Intrinsic (Static s)
     | Intrinsic Dynamic -> Intrinsic Dynamic)
  | USeq { elementencoding } ->
    (match sizability elementencoding with
     | Intrinsic (Static n) ->
       if n = Commons.Sizedints.Uint62.zero
       then raise (Failure "Non-lengthed sequences cannot have zero-size elements")
       else Extrinsic
     | Intrinsic Dynamic -> Extrinsic)
  | Headered { mkheader = _; headerencoding; mkencoding = _; equal = _; maximum_size = _ }
    ->
    (* TODO? should we support zero-sized headers? I don't think we should *)
    (match sizability headerencoding with
     | Intrinsic (Static n) ->
       if n = Commons.Sizedints.Uint62.zero
       then raise (Failure "Zero-size headers not supported")
       else Intrinsic Dynamic
     | Intrinsic Dynamic -> Intrinsic Dynamic)
  | Fold
      { chunkencoding
      ; chunkify = _
      ; readinit = _
      ; reducer = _
      ; equal = _
      ; maximum_size = _
      } ->
    (match sizability chunkencoding with
     | Intrinsic (Static n) ->
       if n = Commons.Sizedints.Uint62.zero
       then raise (Failure "Zero-size chunks not supported")
       else Intrinsic Dynamic
     | Intrinsic Dynamic -> Intrinsic Dynamic)
  | Conv { serialisation = _; deserialisation = _; encoding } -> sizability encoding
  | Size_headered { size = _; encoding = _ } ->
    (* TODO: a pedantic mode which checks that the encoding is Extrinsic (used
         to sanity check that an encoding is not wasteful) *)
    Intrinsic Dynamic
  | Size_limit { at_most = _; encoding } -> sizability encoding
  | Union { tag = _; serialisation = _; deserialisation = _; cases = _ } ->
    (* TODO? be more thourough and check if all the cases have the same size *)
    Intrinsic Dynamic
  | TupNil -> Intrinsic (Static Commons.Sizedints.Uint62.zero)
  | TupCons (TAnyStatic, head, tail) ->
    (match sizability head with
     | Intrinsic (Static h) ->
       let (Intrinsic (Static t)) = sizability tail in
       (* TODO: catch overflow *)
       Intrinsic (Static (Commons.Sizedints.Uint62.add h t))
     | Intrinsic Dynamic -> Intrinsic Dynamic
     | Extrinsic -> Extrinsic)
  | TupCons (TIntrinsicExtrinsic, _, _) -> Extrinsic
  | TupCons (TIntrinsicDynamic, _, _) -> Intrinsic Dynamic
;;
