[@@@landmark "auto"]

let zero_of_numeral : type n. n Descr.numeral -> n = function
  | Uint8 -> Commons.Sizedints.Uint8.zero
  | Uint16 -> Commons.Sizedints.Uint16.zero
  | Uint30 -> Commons.Sizedints.Uint30.zero
  | Uint62 -> Commons.Sizedints.Uint62.zero
  | Int32 -> Int32.zero
  | Int64 -> Int64.zero
;;

let max_int_of : type n. n Descr.numeral -> Commons.Sizedints.Uint62.t = function
  | Uint8 -> Commons.Sizedints.Uint8.(to_uint62 max_int)
  | Uint16 -> Commons.Sizedints.Uint16.(to_uint62 max_int)
  | Uint30 -> Commons.Sizedints.Uint30.(to_uint62 max_int)
  | Uint62 -> Commons.Sizedints.Uint62.max_int
  | Int32 ->
    Option.get @@ Commons.Sizedints.Uint62.of_int64 (Int64.of_int32 Int32.max_int)
  | Int64 -> Option.get @@ Commons.Sizedints.Uint62.of_int64 Int64.max_int
;;

let size_of_numeral : type n. n Descr.numeral -> Commons.Sizedints.Uint62.t = function
  | Uint8 -> Width.As_uint62.uint8
  | Uint16 -> Width.As_uint62.uint16
  | Uint30 -> Width.As_uint62.uint30
  | Uint62 -> Width.As_uint62.uint62
  | Int32 -> Width.As_uint62.int32
  | Int64 -> Width.As_uint62.int64
;;

let numeral_of_int : type n. n Descr.numeral -> int -> n =
 fun n i ->
  (* TODO: handle overflow uniformly accross all numerals, possibly returning
     option *)
  match n with
  | Uint8 -> Option.get @@ Commons.Sizedints.Uint8.of_int i
  | Uint16 -> Option.get @@ Commons.Sizedints.Uint16.of_int i
  | Uint30 -> Option.get @@ Commons.Sizedints.Uint30.of_int i
  | Uint62 -> Option.get @@ Commons.Sizedints.Uint62.of_int i
  | Int32 -> Int32.of_int i
  | Int64 -> Int64.of_int i
;;

let int_of_numeral : type n. n Descr.numeral -> n -> int =
 fun n i ->
  (* TODO: handle 32 bit arch *)
  match n with
  | Uint8 -> (i :> int)
  | Uint16 -> (i :> int)
  | Uint30 -> (i :> int)
  | Uint62 -> Optint.Int63.to_int (i :> Optint.Int63.t)
  | Int32 -> Int32.to_int i
  | Int64 ->
    (* TODO: handle overflow *)
    Int64.to_int i
;;

let ( let* ) = Result.bind

let rec size_of : type s t. (s, t) Descr.t -> t -> (Optint.Int63.t, string) result =
 fun descr v ->
  match descr with
  | Unit -> Ok Optint.Int63.zero
  | Bool -> Ok Optint.Int63.one
  | Numeral { numeral; endianness = _ } -> Ok (size_of_numeral numeral :> Optint.Int63.t)
  | String n -> Ok (n :> Optint.Int63.t)
  | Bytes n -> Ok (n :> Optint.Int63.t)
  | LSeq { length; descr } ->
    let length =
      (* TODO: catch overflow. *)
      Optint.Int63.to_int (length :> Optint.Int63.t)
    in
    (* TODO: classify the sizability of the descr: if fixed just do a
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
        let* elt_size = size_of descr elt in
        fold (len + 1) (Optint.Int63.add size elt_size) s
    in
    fold 0 Optint.Int63.zero v.seq
  | USeq { descr } ->
    let rec fold size s =
      match s () with
      | Seq.Nil -> Ok size
      | Seq.Cons (elt, s) ->
        let* elt_size = size_of descr elt in
        fold (Optint.Int63.add size elt_size) s
    in
    fold Optint.Int63.zero v
  | Array { length; descr } ->
    if Option.get @@ Commons.Sizedints.Uint62.of_int64 (Int64.of_int (Array.length v))
       <> length
    then
      raise
        (Invalid_argument "data-encoding.binary.query.size_of: inconsistent Array length");
    let length =
      (* TODO: catch overflow. Can overflow happen? Wouldn't that prevent the Array to even exist in the first place? *)
      Optint.Int63.to_int (length :> Optint.Int63.t)
    in
    (* TODO: classify the sizability of the descr: if fixed just do a
       multiplication *)
    let rec fold acc index =
      if index >= length
      then Ok acc
      else
        let* size = size_of descr (Array.get v index) in
        let acc = Optint.Int63.add acc size in
        fold acc (index + 1)
    in
    fold Optint.Int63.zero 0
  | Option { optioner = _; descr } ->
    (match v with
     | None -> Ok Optint.Int63.one
     | Some v ->
       let* size = size_of descr v in
       Ok (Optint.Int63.add Optint.Int63.one size))
  | Headered { mkheader; headerdescr; descr_of_header; equal = _; maximum_size = _ } ->
    let* header = mkheader v in
    let* headersize = size_of headerdescr header in
    let* descr = descr_of_header header in
    let* payloadsize =
      match descr with
      | EDynamic descr -> size_of descr v
      | EStatic descr ->
        (* TODO? don't use v and use the sizability instead? *)
        size_of descr v
    in
    Ok (Optint.Int63.add headersize payloadsize)
  | Fold { chunkdescr; chunkify; readinit = _; reducer = _; equal = _; maximum_size = _ }
    ->
    let chunks = chunkify v in
    let rec fold acc s =
      match s () with
      | Seq.Nil -> Ok acc
      | Seq.Cons (chunk, s) ->
        (match size_of chunkdescr chunk with
         | Ok chunksize ->
           let acc = Optint.Int63.add acc chunksize in
           fold acc s
         | Error e -> Error e)
    in
    fold Optint.Int63.zero chunks
  | Conv { serialisation; deserialisation = _; descr } -> size_of descr (serialisation v)
  | Size_headered { size; descr } ->
    let sizesize = size_of_numeral size in
    let* payloadsize = size_of descr v in
    if Optint.Int63.compare payloadsize (max_int_of size :> Optint.Int63.t) > 0
    then Error "Oversized payload for size header"
    else Ok (Optint.Int63.add (sizesize :> Optint.Int63.t) payloadsize)
  | Size_limit { at_most; descr } ->
    let* size = size_of descr v in
    (* TODO: earlier failing ? *)
    if size > (at_most :> Optint.Int63.t) then Error "size exceeds limit" else Ok size
  | Union { tag = tag_encoding; serialisation; deserialisation = _; cases = _ } ->
    let (AnyP ({ Descr.tag; descr; _ }, payload)) = serialisation v in
    let* tag_size = size_of tag_encoding tag in
    let* payload_size = size_of descr payload in
    (* TODO: overflow *)
    Ok (Optint.Int63.add tag_size payload_size)
  | TupNil -> Ok Optint.Int63.zero
  | TupCons { tupler = _; head; tail } ->
    (match v with
     | vhead :: vtail ->
       let* shead = size_of head vhead in
       let* stail = size_of tail vtail in
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
  w
    Descr.(
      TupCons
        { tupler = TAnyStatic
        ; head = Unit
        ; tail = TupCons { tupler = TAnyStatic; head = Unit; tail = TupNil }
        })
    [ (); () ];
  [%expect {| 0 |}];
  w Descr.(Numeral { numeral = Int64; endianness = Big_endian }) 0x00L;
  [%expect {| 8 |}];
  w
    Descr.(
      Option
        { optioner = OIntrinsic
        ; descr = Numeral { numeral = Int32; endianness = Big_endian }
        })
    None;
  [%expect {| 1 |}];
  w
    Descr.(
      Option
        { optioner = OIntrinsic
        ; descr = Numeral { numeral = Int32; endianness = Big_endian }
        })
    (Some 0xff_ffl);
  [%expect {| 5 |}];
  ()
;;

let rec maximum_size_of : type s t. (s, t) Descr.t -> Optint.Int63.t = function
  | Unit -> Optint.Int63.zero
  | Bool -> Optint.Int63.one
  | Numeral { numeral = Int64; endianness = _ } -> Optint.Int63.of_int 8
  | Numeral { numeral = Int32; endianness = _ } -> Optint.Int63.of_int 4
  | Numeral { numeral = Uint62; endianness = _ } -> Optint.Int63.of_int 8
  | Numeral { numeral = Uint30; endianness = _ } -> Optint.Int63.of_int 4
  | Numeral { numeral = Uint16; endianness = _ } -> Optint.Int63.of_int 2
  | Numeral { numeral = Uint8; endianness = _ } -> Optint.Int63.one
  | String n -> (n :> Optint.Int63.t)
  | Bytes n -> (n :> Optint.Int63.t)
  | LSeq { length; descr } ->
    Optint.Int63.mul (length :> Optint.Int63.t) (maximum_size_of descr)
  | USeq { descr = _ } -> Optint.Int63.max_int
  | Array { length; descr } ->
    Optint.Int63.mul (length :> Optint.Int63.t) (maximum_size_of descr)
  | Option { optioner = _; descr } ->
    Optint.Int63.add Optint.Int63.one (maximum_size_of descr)
  | Headered { mkheader = _; headerdescr; descr_of_header = _; equal = _; maximum_size }
    -> Optint.Int63.add (maximum_size_of headerdescr) maximum_size
  | Fold
      { chunkdescr = _; chunkify = _; readinit = _; reducer = _; equal = _; maximum_size }
    -> maximum_size
  | Conv { serialisation = _; deserialisation = _; descr } -> maximum_size_of descr
  | Size_headered { size; descr } ->
    let maxsize :> Optint.Int63.t = max_int_of size in
    let maxencodingsize = maximum_size_of descr in
    if Optint.Int63.compare maxsize maxencodingsize < 0 then maxsize else maxencodingsize
  | Size_limit { at_most; descr } ->
    let maxsize = maximum_size_of descr in
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
      | AnyC { tag = _; descr; inject = _ } :: cases ->
        List.fold_left
          (fun sz (Descr.AnyC { tag = _; descr; inject = _ }) ->
            let nsz = maximum_size_of descr in
            if Optint.Int63.compare sz nsz > 0 then nsz else sz)
          (maximum_size_of descr)
          cases
    in
    Optint.Int63.add tag_size payload_size
  | TupNil -> Optint.Int63.zero
  | TupCons { tupler = _; head; tail } ->
    Optint.Int63.add (maximum_size_of head) (maximum_size_of tail)
;;

let%expect_test _ =
  let w : type s a. (s, a) Descr.t -> unit =
   fun e -> Format.printf "%a\n" Optint.Int63.pp (maximum_size_of e)
  in
  w Descr.Unit;
  [%expect {| 0 |}];
  w
    Descr.(
      TupCons
        { tupler = TAnyStatic
        ; head = Unit
        ; tail = TupCons { tupler = TAnyStatic; head = Unit; tail = TupNil }
        });
  [%expect {| 0 |}];
  w Descr.(Numeral { numeral = Int64; endianness = Big_endian });
  [%expect {| 8 |}];
  w
    Descr.(
      Option
        { optioner = OIntrinsic
        ; descr = Numeral { numeral = Int32; endianness = Big_endian }
        });
  [%expect {| 5 |}];
  ()
;;

let rec equal_of : type s t. (s, t) Descr.t -> t -> t -> bool = function
  | Unit -> Unit.equal
  | Bool -> Bool.equal
  | Numeral { numeral = Int64; endianness = _ } -> Int64.equal
  | Numeral { numeral = Int32; endianness = _ } -> Int32.equal
  | Numeral { numeral = Uint62; endianness = _ } ->
    fun a b -> Optint.Int63.equal (a :> Optint.Int63.t) (b :> Optint.Int63.t)
  | Numeral { numeral = Uint30; endianness = _ } ->
    fun a b -> Int.equal (a :> int) (b :> int)
  | Numeral { numeral = Uint16; endianness = _ } ->
    fun a b -> Int.equal (a :> int) (b :> int)
  | Numeral { numeral = Uint8; endianness = _ } ->
    fun a b -> Int.equal (a :> int) (b :> int)
  | String _ -> String.equal
  | Bytes _ -> Bytes.equal
  | Array { length; descr } ->
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
      let eq = equal_of descr in
      Array.for_all2 eq a b
  | Option { optioner = _; descr } ->
    let eq = equal_of descr in
    Option.equal eq
  | LSeq { descr; length = _ } -> fun s1 s2 -> Seq.equal (equal_of descr) s1.seq s2.seq
  | USeq { descr } -> fun s1 s2 -> Seq.equal (equal_of descr) s1 s2
  | Headered
      { mkheader = _; headerdescr = _; descr_of_header = _; equal; maximum_size = _ } ->
    equal
  | Fold
      { chunkdescr = _; chunkify = _; readinit = _; reducer = _; equal; maximum_size = _ }
    -> equal
  | Conv { serialisation; deserialisation = _; descr } ->
    fun x y -> (equal_of descr) (serialisation x) (serialisation y)
  | Size_headered { size = _; descr } -> equal_of descr
  | Size_limit { at_most = _; descr } -> equal_of descr
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
  | TupCons { tupler = _; head; tail } ->
    fun (ah :: at) (bh :: bt) ->
      let head = equal_of head in
      head ah bh
      &&
      let tail = equal_of tail in
      tail at bt
;;

let rec pp_of : type s t. (s, t) Descr.t -> Format.formatter -> t -> unit =
 fun descr fmt v ->
  match descr with
  | Unit -> Format.fprintf fmt "()"
  | Bool -> Format.fprintf fmt "%b" v
  | Numeral { numeral = Int64; endianness = _ } -> Format.fprintf fmt "%Ld" v
  | Numeral { numeral = Int32; endianness = _ } -> Format.fprintf fmt "%ld" v
  | Numeral { numeral = Uint62; endianness = _ } ->
    Format.fprintf fmt "%Ld" (Optint.Int63.to_int64 (v :> Optint.Int63.t))
  | Numeral { numeral = Uint30; endianness = _ } -> Format.fprintf fmt "%d" (v :> int)
  | Numeral { numeral = Uint16; endianness = _ } -> Format.fprintf fmt "%d" (v :> int)
  | Numeral { numeral = Uint8; endianness = _ } -> Format.fprintf fmt "%d" (v :> int)
  | String _ -> Format.fprintf fmt "%S" v
  | Bytes _ -> Format.fprintf fmt "%S" (Bytes.unsafe_to_string v)
  | Array { length = _; descr } ->
    Format.(
      fprintf
        fmt
        "[|%a|]"
        (pp_print_seq ~pp_sep:(fun fmt () -> pp_print_char fmt ';') (pp_of descr))
        (Array.to_seq v))
  | Option { optioner = _; descr } ->
    (match v with
     | None -> Format.fprintf fmt "None"
     | Some v ->
       let pp = pp_of descr in
       Format.fprintf fmt "Some(%a)" pp v)
  | LSeq { length = _; descr } ->
    let { Descr.seq; length = _ } = v in
    Format.fprintf
      fmt
      "seq(%a)"
      Format.(pp_print_seq ~pp_sep:(fun fmt () -> pp_print_char fmt ',') (pp_of descr))
      seq
  | USeq { descr } ->
    Format.fprintf
      fmt
      "seq(%a)"
      Format.(pp_print_seq ~pp_sep:(fun fmt () -> pp_print_char fmt ',') (pp_of descr))
      v
  | Headered { mkheader; headerdescr = _; descr_of_header; equal = _; maximum_size = _ }
    ->
    let ( let* ) = Result.bind in
    (match
       let* header = mkheader v in
       let* descr = descr_of_header header in
       Ok descr
     with
     | Ok (EDynamic descr) ->
       let pp = pp_of descr in
       Format.fprintf fmt "%a" pp v
     | Ok (EStatic descr) ->
       let pp = pp_of descr in
       Format.fprintf fmt "%a" pp v
     | Error msg -> Format.fprintf fmt "Error: %s" msg)
  | Fold { chunkdescr; chunkify; readinit = _; reducer = _; equal = _; maximum_size = _ }
    ->
    Format.fprintf
      fmt
      "chunked(%a)"
      (Format.pp_print_seq
         ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ',')
         (pp_of chunkdescr))
      (chunkify v)
  | Conv { serialisation; deserialisation = _; descr } ->
    let pp fmt v = pp_of descr fmt (serialisation v) in
    Format.fprintf fmt "conved(%a)" pp v
  | Size_headered { size = _; descr } -> pp_of descr fmt v
  | Size_limit { at_most = _; descr } -> pp_of descr fmt v
  | Union { tag = tag_encoding; serialisation; deserialisation = _; cases = _ } ->
    let (AnyP ({ Descr.tag; descr; inject = _ }, payload)) = serialisation v in
    Format.fprintf fmt "case(%a:%a)" (pp_of tag_encoding) tag (pp_of descr) payload
  | TupNil -> ()
  | TupCons { tupler = _; head; tail = TupNil } ->
    let [ v ] = v in
    let pp = pp_of head in
    Format.fprintf fmt "%a" pp v
  | TupCons { tupler = _; head; tail } ->
    let (v :: vs) = v in
    let pph = pp_of head in
    let ppt = pp_of tail in
    Format.fprintf fmt "%a;%a" pph v ppt vs
;;

let rec sizability : type s a. (s, a) Descr.t -> s = function
  | Unit -> Intrinsic (Static Commons.Sizedints.Uint62.zero)
  | Bool -> Intrinsic (Static Width.As_uint62.bool)
  | Numeral { numeral; endianness = _ } -> Intrinsic (Static (size_of_numeral numeral))
  | String n -> Intrinsic (Static n)
  | Bytes n -> Intrinsic (Static n)
  | Array { length; descr } ->
    (match sizability descr with
     | Intrinsic (Static n) ->
       (* TODO: catch overflow *)
       let s = Commons.Sizedints.Uint62.mul length n in
       Sizability.Intrinsic (Static s)
     | Intrinsic Dynamic -> Intrinsic Dynamic)
  | Option { optioner; descr = _ } ->
    (match optioner with
     | OIntrinsic -> Intrinsic Dynamic
     | OExtrinsic -> Extrinsic)
  | LSeq { length; descr } ->
    (match sizability descr with
     | Intrinsic (Static n) ->
       (* TODO: catch overflow *)
       let s = Commons.Sizedints.Uint62.mul length n in
       Sizability.Intrinsic (Static s)
     | Intrinsic Dynamic -> Intrinsic Dynamic)
  | USeq { descr } ->
    (match sizability descr with
     | Intrinsic (Static n) ->
       if n = Commons.Sizedints.Uint62.zero
       then raise (Failure "Non-lengthed sequences cannot have zero-size elements")
       else Extrinsic
     | Intrinsic Dynamic -> Extrinsic)
  | Headered
      { mkheader = _; headerdescr; descr_of_header = _; equal = _; maximum_size = _ } ->
    (* TODO? should we support zero-sized headers? I don't think we should *)
    (match sizability headerdescr with
     | Intrinsic (Static n) ->
       if n = Commons.Sizedints.Uint62.zero
       then raise (Failure "Zero-size headers not supported")
       else Intrinsic Dynamic
     | Intrinsic Dynamic -> Intrinsic Dynamic)
  | Fold
      { chunkdescr; chunkify = _; readinit = _; reducer = _; equal = _; maximum_size = _ }
    ->
    (match sizability chunkdescr with
     | Intrinsic (Static n) ->
       if n = Commons.Sizedints.Uint62.zero
       then raise (Failure "Zero-size chunks not supported")
       else Intrinsic Dynamic
     | Intrinsic Dynamic -> Intrinsic Dynamic)
  | Conv { serialisation = _; deserialisation = _; descr } -> sizability descr
  | Size_headered { size = _; descr = _ } ->
    (* TODO: a pedantic mode which checks that the descr is Extrinsic (used
         to sanity check that an descr is not wasteful) *)
    Intrinsic Dynamic
  | Size_limit { at_most = _; descr } -> sizability descr
  | Union { tag = _; serialisation = _; deserialisation = _; cases = _ } ->
    (* TODO? be more thourough and check if all the cases have the same size *)
    Intrinsic Dynamic
  | TupNil -> Intrinsic (Static Commons.Sizedints.Uint62.zero)
  | TupCons { tupler = TAnyStatic; head; tail } ->
    (match sizability head with
     | Intrinsic (Static h) ->
       let (Intrinsic (Static t)) = sizability tail in
       (* TODO: catch overflow *)
       Intrinsic (Static (Commons.Sizedints.Uint62.add h t))
     | Intrinsic Dynamic -> Intrinsic Dynamic
     | Extrinsic -> Extrinsic)
  | TupCons { tupler = TIntrinsicExtrinsic; head = _; tail = _ } -> Extrinsic
  | TupCons { tupler = TIntrinsicDynamic; head = _; tail = _ } -> Intrinsic Dynamic
;;
