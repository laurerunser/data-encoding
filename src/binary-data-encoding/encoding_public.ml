module Hlist = Commons.Hlist
module Sizedints = Commons.Sizedints

type 'a t = 'a Descr.introspectable = E : (_ Sizability.t, 'a) Descr.t -> 'a t

let introspect : 'a t -> 'a Descr.introspectable = Fun.id
let forget : 'a Descr.introspectable -> 'a t = Fun.id

type 'a seq_with_length = 'a Descr.seq_with_length =
  { seq : 'a Seq.t
  ; length : Sizedints.Uint62.t Lazy.t
  }

type variable_count_spec =
  [ `UInt62
  | `UInt30
  | `UInt16
  | `UInt8
  ]

type count_spec =
  [ `Fixed of Sizedints.Uint62.t
  | variable_count_spec
  ]

type 'a numeral = 'a Descr.numeral =
  | UInt8 : Sizedints.Uint8.t numeral
  | UInt16 : Sizedints.Uint16.t numeral
  | UInt30 : Sizedints.Uint30.t numeral
  | UInt62 : Sizedints.Uint62.t numeral
  | Int32 : int32 numeral
  | Int64 : int64 numeral

type endianness = Descr.endianness =
  | Big_endian
  | Little_endian

type _ tuple =
  | [] : unit Hlist.t tuple
  | ( :: ) : 'a t * 'b Hlist.t tuple -> ('a * 'b) Hlist.t tuple

let rec tuple : type a. a tuple -> a t = function
  | [] -> E TupNil
  | E head :: tail ->
    let open Sizability in
    (match Query.sizability head with
     | Intrinsic (Static _) ->
       let (E tail) = tuple tail in
       (match Query.sizability tail with
        | Intrinsic (Static _) ->
          (* TODO: catch overflow *)
          E (TupCons (TStaticIntrinsic, head, tail))
        | Intrinsic Dynamic -> E (TupCons (TStaticIntrinsic, head, tail))
        | Extrinsic -> E (TupCons (TIntrinsicExtrinsic, head, tail)))
     | Intrinsic Dynamic ->
       let (E tail) = tuple tail in
       (match Query.sizability tail with
        | Intrinsic _ -> E (TupCons (TDynamicIntrinsic, head, tail))
        | Extrinsic -> E (TupCons (TIntrinsicExtrinsic, head, tail)))
     | Extrinsic ->
       let (E tail) = tuple tail in
       (match Query.sizability tail with
        | Intrinsic (Static _) -> E (TupCons (TExtrinsicStatic, head, tail))
        | Intrinsic Dynamic ->
          failwith "forbidden extrinsic-dynamic construction in tuple"
        | Extrinsic -> failwith "forbidden extrinsic-extrinsic construction in tuple"))
;;

let unit = E Unit
let bool = E Bool

module Big_endian = struct
  let int64 = E (Numeral { numeral = Int64; endianness = Big_endian })
  let int32 = E (Numeral { numeral = Int32; endianness = Big_endian })
  let uint30 = E (Numeral { numeral = UInt30; endianness = Big_endian })
  let uint62 = E (Numeral { numeral = UInt62; endianness = Big_endian })
  let uint16 = E (Numeral { numeral = UInt16; endianness = Big_endian })
  let uint8 = E (Numeral { numeral = UInt8; endianness = Big_endian })
end

include Big_endian

let default_endianness = Big_endian

module Little_endian = struct
  let int64 = E (Numeral { numeral = Int64; endianness = Little_endian })
  let int32 = E (Numeral { numeral = Int32; endianness = Little_endian })
  let uint30 = E (Numeral { numeral = UInt30; endianness = Little_endian })
  let uint62 = E (Numeral { numeral = UInt62; endianness = Little_endian })
  let uint16 = E (Numeral { numeral = UInt16; endianness = Little_endian })
  let uint8 = E (Numeral { numeral = UInt8; endianness = Little_endian })
end

let option (E t) =
  (* TODO? only allow extrinsic in the advanced low-level module?? *)
  match Query.sizability t with
  | Intrinsic _ -> E (Option (OIntrinsic, t))
  | Extrinsic -> E (Option (OExtrinsic, t))
;;

let conv ~serialisation ~deserialisation (E encoding) =
  E (Conv { serialisation; deserialisation; encoding })
;;

let seq count_spec (E encoding) =
  match Query.sizability encoding with
  | Extrinsic -> raise (Invalid_argument "extrinsic element encoding in seq")
  | Intrinsic _ ->
    conv
      ~serialisation:(fun seq ->
        let length =
          lazy (Option.get (Sizedints.Uint62.of_int64 (Int64.of_int (Seq.length seq))))
        in
        { seq; length })
      ~deserialisation:(fun { seq; length = _ } -> Ok seq)
      (Descr.E (Encoding_internals.seq_with_length count_spec encoding))
;;

let list count_spec (E encoding) =
  match Query.sizability encoding with
  | Extrinsic -> raise (Invalid_argument "extrinsic element encoding in list")
  | Intrinsic _ ->
    conv
      ~serialisation:(fun l ->
        let length =
          lazy (Option.get (Sizedints.Uint62.of_int64 (Int64.of_int (List.length l))))
        in
        { seq = List.to_seq l; length })
      ~deserialisation:(fun { seq; length = _ } -> Ok (List.of_seq seq))
      (Descr.E (Encoding_internals.seq_with_length count_spec encoding))
;;

let array : type a. count_spec -> a t -> a array t =
 fun lengthencoding (E elementencoding) ->
  match lengthencoding with
  | `Fixed length ->
    (match Query.sizability elementencoding with
     | Extrinsic -> raise (Invalid_argument "extrinsic in array")
     | Intrinsic _ -> E (Array { length; elementencoding }))
  | #variable_count_spec as lengthencoding ->
    (match Query.sizability elementencoding with
     | Extrinsic -> raise (Invalid_argument "extrinsic in array")
     | Intrinsic _ ->
       let descr =
         Encoding_internals.with_length_header
           ~lengthencoding
           ~length:(fun a -> Option.get (Sizedints.Uint62.of_int (Array.length a)))
           ~mkencoding:(fun length ->
             Ok
               (Encoding_internals.wrap_intrinsic
                  (Query.sizability elementencoding)
                  (Array { length; elementencoding })))
           ~equal:(fun xs ys ->
             Array.length xs = Array.length ys
             && Array.for_all2 (Query.equal_of elementencoding) xs ys)
           ~maximum_size:
             (let maximum_length =
                match lengthencoding with
                | `UInt8 -> Sizedints.Uint8.(to_uint62 max_int)
                | `UInt16 -> Sizedints.Uint16.(to_uint62 max_int)
                | `UInt30 -> Sizedints.Uint30.(to_uint62 max_int)
                | `UInt62 -> Sizedints.Uint62.max_int
              in
              Optint.Int63.mul
                (maximum_length :> Optint.Int63.t)
                (Query.maximum_size_of elementencoding))
       in
       E descr)
;;

module With_size = struct
  let seq_with_size (sizeencoding : variable_count_spec) (E elementencoding) =
    match Query.sizability elementencoding with
    | Intrinsic (Static n) ->
      if n = Sizedints.Uint62.zero
      then
        raise (Invalid_argument "zero-size elements in sized (rather than lengthed) seq")
      else
        E
          (Encoding_internals.with_size_header
             ~sizeencoding
             ~encoding:(USeq { elementencoding }))
    | Extrinsic -> raise (Invalid_argument "extrinsic-size elements in sized seq")
    | Intrinsic _ ->
      E
        (Encoding_internals.with_size_header
           ~sizeencoding
           ~encoding:(USeq { elementencoding }))
  ;;
end

let string lengthencoding =
  match lengthencoding with
  | `Fixed length -> E (String length)
  | #variable_count_spec as lengthencoding ->
    let descr =
      Encoding_internals.with_length_header
        ~lengthencoding
        ~length:(fun s ->
          Option.get (Sizedints.Uint62.of_int64 (Int64.of_int (String.length s))))
        ~mkencoding:(fun n -> Ok (EStatic (String n)))
        ~equal:String.equal
        ~maximum_size:
          (match lengthencoding with
           | `UInt8 -> (Sizedints.Uint8.(to_uint62 max_int) :> Optint.Int63.t)
           | `UInt16 -> (Sizedints.Uint16.(to_uint62 max_int) :> Optint.Int63.t)
           | `UInt30 -> (Sizedints.Uint30.(to_uint62 max_int) :> Optint.Int63.t)
           | `UInt62 -> (Sizedints.Uint62.max_int :> Optint.Int63.t))
    in
    E descr
;;

let bytes lengthencoding =
  match lengthencoding with
  | `Fixed length -> E (Bytes length)
  | #variable_count_spec as lengthencoding ->
    let descr =
      Encoding_internals.with_length_header
        ~lengthencoding
        ~length:(fun b ->
          Option.get (Sizedints.Uint62.of_int64 (Int64.of_int (Bytes.length b))))
        ~mkencoding:(fun n -> Ok (EStatic (Bytes n)))
        ~equal:Bytes.equal
        ~maximum_size:
          (match lengthencoding with
           | `Fixed n -> (n :> Optint.Int63.t)
           | `UInt8 -> (Sizedints.Uint8.(to_uint62 max_int) :> Optint.Int63.t)
           | `UInt16 -> (Sizedints.Uint16.(to_uint62 max_int) :> Optint.Int63.t)
           | `UInt30 -> (Sizedints.Uint30.(to_uint62 max_int) :> Optint.Int63.t)
           | `UInt62 -> (Sizedints.Uint62.max_int :> Optint.Int63.t))
    in
    E descr
;;

let ellastic_uint30 : Sizedints.Uint30.t t =
  let payload_mask = (* significant bits of each byte *) 0b0111_1111 in
  let tag_mask = (* metadata bits of each byte *) 0b1000_0000 in
  let payload_width = (* number of significant bits in each byte of payload *) 7 in
  let descr =
    Encoding_internals.fold
      ~chunkencoding:(Numeral { numeral = UInt8; endianness = Big_endian })
      ~chunkify:(fun (u30 : Sizedints.Uint30.t) ->
        let u30 = (u30 :> int) in
        let rec chunkify u30 () =
          assert (u30 >= 0);
          if u30 land payload_mask = u30
          then (
            let chunk =
              (* TODO: optimise by avoiding boxing w/ option *)
              Option.get @@ Sizedints.Uint8.of_int u30
            in
            Seq.Cons (chunk, Seq.empty))
          else (
            let chunk = u30 land payload_mask lor tag_mask in
            let chunk =
              (* TODO: optimise by avoiding boxing w/ option *)
              Option.get @@ Sizedints.Uint8.of_int chunk
            in
            let rest = u30 lsr payload_width in
            Seq.Cons (chunk, chunkify rest))
        in
        chunkify u30)
      ~readinit:(0, 0)
      ~reducer:(fun (acc, shift) chunk ->
        let chunk = (chunk :> int) in
        let acc = acc lor ((chunk land payload_mask) lsl shift) in
        if chunk land tag_mask = 0
        then
          (* TODO: handle errors here *)
          (* TODO: check overflow as we go along to avoid decoding too many bytes;
         maybe limit the number of chunks too? *)
          Finish (Option.get @@ Sizedints.Uint30.of_int acc)
        else K (acc, shift + payload_width))
      ~equal:(fun a b -> Int.equal (a :> int) (b :> int))
      ~maximum_size:(Optint.Int63.of_int 5)
  in
  E descr
;;

module Union = struct
  type ('tag, 'payload, 'union) case_descr =
    | ECase :
        (_, 'tag, 'payload, 'union) Descr.case_descr
        -> ('tag, 'payload, 'union) case_descr

  type ('tag, 'a) anycase = ('tag, 'a) Descr.anycase =
    | AnyC : (_, 'tag, _, 'a) Descr.case_descr -> ('tag, 'a) anycase

  let anycase (ECase c) = AnyC c

  type ('tag, 'a) anycaseandpayload = ('tag, 'a) Descr.anycaseandpayload =
    | AnyP :
        (_, 'tag, 'payload, 'a) Descr.case_descr * 'payload
        -> ('tag, 'a) anycaseandpayload

  let case_and_payload (ECase case) payload = AnyP (case, payload)

  let case tag (E encoding) inject =
    match Query.sizability encoding with
    | Extrinsic -> raise (Invalid_argument "extrinsic payload encoding in tag")
    | Intrinsic _ -> ECase { tag; encoding; inject }
  ;;

  let case_unit tag inject = ECase { tag; encoding = Unit; inject }

  let union
    : type a tag.
      tag t
      -> (tag, a) anycase list
      -> (a -> (tag, a) anycaseandpayload)
      -> (tag -> ((tag, a) anycase, string) result)
      -> a t
    =
   fun (E tag) cases serialisation deserialisation ->
    match Query.sizability tag with
    | Extrinsic -> raise (Invalid_argument "extrinsic tag in union")
    | Intrinsic Dynamic -> raise (Invalid_argument "dynamic tag in union")
    | Intrinsic (Static _) -> E (Union { tag; serialisation; deserialisation; cases })
 ;;

  let either l r =
    let (ECase cl) = case true l Either.left in
    let anycl = AnyC cl in
    let (ECase cr) = case false r Either.right in
    let anycr = AnyC cr in
    union
      bool
      [ anycl; anycr ]
      (function
       | Either.Left l -> AnyP (cl, l)
       | Either.Right r -> AnyP (cr, r))
      (function
       | true -> Ok anycl
       | false -> Ok anycr)
  ;;
end

include Union

let with_size_header ~sizeencoding ~encoding:(E encoding) =
  match Query.sizability encoding with
  | Intrinsic _ -> raise (Invalid_argument "intrinsic cannot have size header")
  | Extrinsic ->
    let descr = Encoding_internals.with_size_header ~sizeencoding ~encoding in
    E descr
;;

let with_length_header ~lengthencoding ~length ~mkencoding ~equal ~maximum_size =
  let descr =
    Encoding_internals.with_length_header
      ~lengthencoding
      ~length
      ~mkencoding:(fun n ->
        match mkencoding n with
        | Error _ as error -> error
        | Ok (E descr) ->
          (match Query.sizability descr with
           | Extrinsic -> Error "with_length_header has extrinsic length"
           | Intrinsic Dynamic -> Ok (EDynamic descr)
           | Intrinsic (Static _) -> Ok (EStatic descr)))
      ~equal
      ~maximum_size
  in
  E descr
;;
