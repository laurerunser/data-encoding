module Hlist = Commons.Hlist
module Sizedints = Commons.Sizedints

type variable_count_spec =
  [ `Uint62
  | `Uint30
  | `Uint16
  | `Uint8
  ]

type count_spec =
  [ `Fixed of Sizedints.Uint62.t
  | variable_count_spec
  ]

type 'a numeral = 'a Sizedints.numeral =
  | Uint8 : Sizedints.Uint8.t numeral
  | Uint16 : Sizedints.Uint16.t numeral
  | Uint30 : Sizedints.Uint30.t numeral
  | Uint62 : Sizedints.Uint62.t numeral
  | Int32 : int32 numeral
  | Int64 : int64 numeral

type endianness = Descr.endianness =
  | Big_endian
  | Little_endian

module Helpers = struct
  type 'a seq_with_length = 'a Descr.seq_with_length =
    { seq : 'a Seq.t
    ; length : Sizedints.Uint62.t Lazy.t
    }

  type ('step, 'finish) reducer = ('step, 'finish) Descr.reducer =
    | K of 'step
    | Finish of 'finish

  let with_header ~headerencoding ~mkheader ~mkencoding ~equal ~maximum_size =
    Descr.Headered { mkheader; headerencoding; mkencoding; equal; maximum_size }
  ;;

  let fold ~chunkencoding ~chunkify ~readinit ~reducer ~equal ~maximum_size =
    Descr.Fold { chunkencoding; chunkify; readinit; reducer; equal; maximum_size }
  ;;

  let with_length_header
    : type a.
      lengthencoding:variable_count_spec
      -> length:(a -> Sizedints.Uint62.t)
      -> mkencoding:(Sizedints.Uint62.t -> (a Descr.anyintrinsic, string) result)
      -> equal:(a -> a -> bool)
      -> maximum_size:Optint.Int63.t
      -> (Sizability.dynamic, a) Descr.t
    =
   fun ~lengthencoding ~length ~mkencoding ~equal ~maximum_size ->
    match lengthencoding with
    | `Uint62 ->
      with_header
        ~headerencoding:(Numeral { numeral = Uint62; endianness = Big_endian })
        ~mkheader:(fun v -> Ok (length v))
        ~mkencoding
        ~equal
        ~maximum_size
    | `Uint30 ->
      with_header
        ~headerencoding:(Numeral { numeral = Uint30; endianness = Big_endian })
        ~mkheader:(fun v ->
          let length = length v in
          if Sizedints.Uint30.(to_uint62 max_int) < length
          then Error "Length larger than header-size (30 bits) can encode"
          else
            Ok
              (Option.get
                 (Sizedints.Uint30.of_int
                    (Optint.Int63.to_int (length :> Optint.Int63.t)))))
        ~mkencoding:(fun n -> mkencoding (Sizedints.Uint30.to_uint62 n))
        ~equal
        ~maximum_size
    | `Uint16 ->
      with_header
        ~headerencoding:(Numeral { numeral = Uint16; endianness = Big_endian })
        ~mkheader:(fun v ->
          let length = length v in
          if Sizedints.Uint16.(to_uint62 max_int) < length
          then Error "Length larger than header-size (16 bits) can encode"
          else
            Ok
              (Option.get
                 (Sizedints.Uint16.of_int
                    (Optint.Int63.to_int (length :> Optint.Int63.t)))))
        ~mkencoding:(fun n -> mkencoding (Sizedints.Uint16.to_uint62 n))
        ~equal
        ~maximum_size
    | `Uint8 ->
      with_header
        ~headerencoding:(Numeral { numeral = Uint8; endianness = Big_endian })
        ~mkheader:(fun v ->
          let length = length v in
          if Sizedints.Uint8.(to_uint62 max_int) < length
          then Error "Length larger than header-size (16 bits) can encode"
          else
            Ok
              (Option.get
                 (Sizedints.Uint8.of_int (Optint.Int63.to_int (length :> Optint.Int63.t)))))
        ~mkencoding:(fun n -> mkencoding (Sizedints.Uint8.to_uint62 n))
        ~equal
        ~maximum_size
 ;;

  let with_size_header
    : type a.
      sizeencoding:variable_count_spec
      -> encoding:(Sizability.extrinsic, a) Descr.t
      -> (Sizability.dynamic, a) Descr.t
    =
   fun ~sizeencoding ~encoding ->
    match sizeencoding with
    | `Uint62 -> Size_headered { size = Uint62; encoding }
    | `Uint30 -> Size_headered { size = Uint30; encoding }
    | `Uint16 -> Size_headered { size = Uint16; encoding }
    | `Uint8 -> Size_headered { size = Uint8; encoding }
 ;;

  let seq_with_length_fixed length elementencoding =
    Descr.E (LSeq { length; elementencoding })
  ;;

  let wrap_intrinsic
    : type sz.
      sz Sizability.intrinsic
      -> (sz Sizability.intrinsic, 'a) Descr.t
      -> 'a Descr.anyintrinsic
    =
   fun sz d ->
    match sz with
    | Intrinsic Dynamic -> EDynamic d
    | Intrinsic (Static _) -> EStatic d
 ;;

  let seq_with_length
    : type eltsz eltt.
      variable_count_spec
      -> (eltsz Sizability.intrinsic, eltt) Descr.t
      -> (Sizability.dynamic, eltt seq_with_length) Descr.t
    =
   fun lengthencoding elementencoding ->
    let maximum_size =
      (* TODO *)
      Optint.Int63.max_int
    in
    let equal a b = Seq.equal (Query.equal_of elementencoding) a.seq b.seq in
    let mkencoding length =
      Ok
        (wrap_intrinsic
           (Query.sizability elementencoding)
           (LSeq { length; elementencoding }))
    in
    match lengthencoding with
    | `Uint62 ->
      with_header
        ~headerencoding:(Numeral { numeral = Uint62; endianness = Big_endian })
        ~mkheader:(fun (s : eltt seq_with_length) -> Ok (Lazy.force s.length))
        ~mkencoding
        ~equal
        ~maximum_size
    | `Uint30 ->
      with_header
        ~headerencoding:(Numeral { numeral = Uint30; endianness = Big_endian })
        ~mkheader:(fun v ->
          let length = Lazy.force v.length in
          if Sizedints.Uint30.(to_uint62 max_int) < length
          then Error "Length larger than header-size (30 bits) can encode"
          else
            Ok
              (Option.get
                 (Sizedints.Uint30.of_int
                    (Optint.Int63.to_int (length :> Optint.Int63.t)))))
        ~mkencoding:(fun length ->
          let length = Sizedints.Uint30.to_uint62 length in
          mkencoding length)
        ~equal
        ~maximum_size
    | `Uint16 ->
      with_header
        ~headerencoding:(Numeral { numeral = Uint16; endianness = Big_endian })
        ~mkheader:(fun v ->
          let length = Lazy.force v.length in
          if Sizedints.Uint16.(to_uint62 max_int) < length
          then Error "Length larger than header-size (16 bits) can encode"
          else
            Ok
              (Option.get
                 (Sizedints.Uint16.of_int
                    (Optint.Int63.to_int (length :> Optint.Int63.t)))))
        ~mkencoding:(fun length ->
          let length = Sizedints.Uint16.to_uint62 length in
          mkencoding length)
        ~equal
        ~maximum_size
    | `Uint8 ->
      with_header
        ~headerencoding:(Numeral { numeral = Uint8; endianness = Big_endian })
        ~mkheader:(fun v ->
          let length = Lazy.force v.length in
          if Sizedints.Uint8.(to_uint62 max_int) < length
          then Error "Length larger than header-size (16 bits) can encode"
          else
            Ok
              (Option.get
                 (Sizedints.Uint8.of_int (Optint.Int63.to_int (length :> Optint.Int63.t)))))
        ~mkencoding:(fun length ->
          let length = Sizedints.Uint8.to_uint62 length in
          mkencoding length)
        ~equal
        ~maximum_size
 ;;
end

type 'a t = 'a Descr.introspectable = E : (_ Sizability.t, 'a) Descr.t -> 'a t

let introspect : 'a t -> 'a Descr.introspectable = Fun.id
let detrospect : 'a Descr.introspectable -> 'a t = Fun.id

type _ tuple =
  | [] : unit Hlist.t tuple
  | ( :: ) : 'a Descr.introspectable * 'b Hlist.t tuple -> ('a * 'b) Hlist.t tuple

let rec tuple : type a. a tuple -> a t = function
  | [] -> E TupNil
  | E head :: tail ->
    let open Sizability in
    let (E tail) = tuple tail in
    (match Query.sizability tail with
     | Intrinsic (Static _) -> E (TupCons (TAnyStatic, head, tail))
     | Intrinsic Dynamic ->
       (match Query.sizability head with
        | Intrinsic _ -> E (TupCons (TIntrinsicDynamic, head, tail))
        | Extrinsic -> failwith "forbidden extrinsic-dynamic construction in tuple")
     | Extrinsic ->
       (match Query.sizability head with
        | Intrinsic _ -> E (TupCons (TIntrinsicExtrinsic, head, tail))
        | Extrinsic -> failwith "forbidden extrinsic-extrinsic construction in tuple"))
;;

let unit = E Unit
let bool = E Bool

module type Endianed = sig
  val int64 : int64 t
  val int32 : int32 t
  val uint30 : Sizedints.Uint30.t t
  val uint62 : Sizedints.Uint62.t t
  val uint16 : Sizedints.Uint16.t t
  val uint8 : Sizedints.Uint8.t t
end

module Big_endian : Endianed = struct
  let int64 = E (Numeral { numeral = Int64; endianness = Big_endian })
  let int32 = E (Numeral { numeral = Int32; endianness = Big_endian })
  let uint30 = E (Numeral { numeral = Uint30; endianness = Big_endian })
  let uint62 = E (Numeral { numeral = Uint62; endianness = Big_endian })
  let uint16 = E (Numeral { numeral = Uint16; endianness = Big_endian })
  let uint8 = E (Numeral { numeral = Uint8; endianness = Big_endian })
end

include Big_endian

let default_endianness = Big_endian

module Little_endian : Endianed = struct
  let int64 = E (Numeral { numeral = Int64; endianness = Little_endian })
  let int32 = E (Numeral { numeral = Int32; endianness = Little_endian })
  let uint30 = E (Numeral { numeral = Uint30; endianness = Little_endian })
  let uint62 = E (Numeral { numeral = Uint62; endianness = Little_endian })
  let uint16 = E (Numeral { numeral = Uint16; endianness = Little_endian })
  let uint8 = E (Numeral { numeral = Uint8; endianness = Little_endian })
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

module With_length = struct
  let seq count_spec (E encoding) =
    match Query.sizability encoding with
    | Extrinsic -> raise (Invalid_argument "extrinsic element encoding in seq")
    | Intrinsic _ ->
      (match count_spec with
       | #variable_count_spec as count_spec ->
         conv
           ~serialisation:(fun seq ->
             let length =
               lazy
                 (Option.get (Sizedints.Uint62.of_int64 (Int64.of_int (Seq.length seq))))
             in
             { Helpers.seq; length })
           ~deserialisation:(fun { Helpers.seq; length = _ } -> Ok seq)
           (Descr.E (Helpers.seq_with_length count_spec encoding))
       | `Fixed n ->
         conv
           ~serialisation:(fun seq ->
             let length =
               lazy
                 (Option.get (Sizedints.Uint62.of_int64 (Int64.of_int (Seq.length seq))))
             in
             { Helpers.seq; length })
           ~deserialisation:(fun { Helpers.seq; length = _ } -> Ok seq)
           (Helpers.seq_with_length_fixed n encoding))
  ;;

  let list count_spec (E encoding) =
    match Query.sizability encoding with
    | Extrinsic -> raise (Invalid_argument "extrinsic element encoding in list")
    | Intrinsic _ ->
      (match count_spec with
       | #variable_count_spec as count_spec ->
         conv
           ~serialisation:(fun l ->
             let length =
               lazy
                 (Option.get (Sizedints.Uint62.of_int64 (Int64.of_int (List.length l))))
             in
             { Helpers.seq = List.to_seq l; length })
           ~deserialisation:(fun { Helpers.seq; length = _ } -> Ok (List.of_seq seq))
           (Descr.E (Helpers.seq_with_length count_spec encoding))
       | `Fixed n ->
         conv
           ~serialisation:(fun l ->
             let length =
               lazy
                 (Option.get (Sizedints.Uint62.of_int64 (Int64.of_int (List.length l))))
             in
             { Helpers.seq = List.to_seq l; length })
           ~deserialisation:(fun { Helpers.seq; length = _ } -> Ok (List.of_seq seq))
           (Helpers.seq_with_length_fixed n encoding))
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
           Helpers.with_length_header
             ~lengthencoding
             ~length:(fun a -> Option.get (Sizedints.Uint62.of_int (Array.length a)))
             ~mkencoding:(fun length ->
               Ok
                 (Helpers.wrap_intrinsic
                    (Query.sizability elementencoding)
                    (Array { length; elementencoding })))
             ~equal:(fun xs ys ->
               Array.length xs = Array.length ys
               && Array.for_all2 (Query.equal_of elementencoding) xs ys)
             ~maximum_size:
               (let maximum_length =
                  match lengthencoding with
                  | `Uint8 -> Sizedints.Uint8.(to_uint62 max_int)
                  | `Uint16 -> Sizedints.Uint16.(to_uint62 max_int)
                  | `Uint30 -> Sizedints.Uint30.(to_uint62 max_int)
                  | `Uint62 -> Sizedints.Uint62.max_int
                in
                Optint.Int63.mul
                  (maximum_length :> Optint.Int63.t)
                  (Query.maximum_size_of elementencoding))
         in
         E descr)
 ;;
end

include With_length

module With_size = struct
  let seq (sizeencoding : variable_count_spec) (E elementencoding) =
    match Query.sizability elementencoding with
    | Intrinsic (Static n) ->
      if n = Sizedints.Uint62.zero
      then
        raise (Invalid_argument "zero-size elements in sized (rather than lengthed) seq")
      else E (Helpers.with_size_header ~sizeencoding ~encoding:(USeq { elementencoding }))
    | Extrinsic -> raise (Invalid_argument "extrinsic-size elements in sized seq")
    | Intrinsic _ ->
      E (Helpers.with_size_header ~sizeencoding ~encoding:(USeq { elementencoding }))
  ;;

  let list (sizeencoding : variable_count_spec) e_elementencoding =
    conv
      ~serialisation:List.to_seq
      ~deserialisation:(fun s -> Ok (List.of_seq s))
      (seq sizeencoding e_elementencoding)
  ;;

  let array (sizeencoding : variable_count_spec) e_elementencoding =
    conv
      ~serialisation:Array.to_seq
      ~deserialisation:(fun s -> Ok (Array.of_seq s))
      (seq sizeencoding e_elementencoding)
  ;;
end

let string lengthencoding =
  match lengthencoding with
  | `Fixed length -> E (String length)
  | #variable_count_spec as lengthencoding ->
    let descr =
      Helpers.with_length_header
        ~lengthencoding
        ~length:(fun s ->
          Option.get (Sizedints.Uint62.of_int64 (Int64.of_int (String.length s))))
        ~mkencoding:(fun n -> Ok (EStatic (String n)))
        ~equal:String.equal
        ~maximum_size:
          (match lengthencoding with
           | `Uint8 -> (Sizedints.Uint8.(to_uint62 max_int) :> Optint.Int63.t)
           | `Uint16 -> (Sizedints.Uint16.(to_uint62 max_int) :> Optint.Int63.t)
           | `Uint30 -> (Sizedints.Uint30.(to_uint62 max_int) :> Optint.Int63.t)
           | `Uint62 -> (Sizedints.Uint62.max_int :> Optint.Int63.t))
    in
    E descr
;;

let bytes lengthencoding =
  match lengthencoding with
  | `Fixed length -> E (Bytes length)
  | #variable_count_spec as lengthencoding ->
    let descr =
      Helpers.with_length_header
        ~lengthencoding
        ~length:(fun b ->
          Option.get (Sizedints.Uint62.of_int64 (Int64.of_int (Bytes.length b))))
        ~mkencoding:(fun n -> Ok (EStatic (Bytes n)))
        ~equal:Bytes.equal
        ~maximum_size:
          (match lengthencoding with
           | `Fixed n -> (n :> Optint.Int63.t)
           | `Uint8 -> (Sizedints.Uint8.(to_uint62 max_int) :> Optint.Int63.t)
           | `Uint16 -> (Sizedints.Uint16.(to_uint62 max_int) :> Optint.Int63.t)
           | `Uint30 -> (Sizedints.Uint30.(to_uint62 max_int) :> Optint.Int63.t)
           | `Uint62 -> (Sizedints.Uint62.max_int :> Optint.Int63.t))
    in
    E descr
;;

let ellastic_uint30 : Sizedints.Uint30.t t =
  let payload_mask = (* significant bits of each byte *) 0b0111_1111 in
  let tag_mask = (* metadata bits of each byte *) 0b1000_0000 in
  let payload_width = (* number of significant bits in each byte of payload *) 7 in
  let descr =
    Helpers.fold
      ~chunkencoding:(Numeral { numeral = Uint8; endianness = Big_endian })
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

let with_size_limit limit (E encoding) =
  match Commons.Sizedints.Uint62.of_int limit with
  | None -> raise (Invalid_argument "size limit cannot be negative")
  | Some at_most -> E (Size_limit { at_most; encoding })
;;

let with_length_header ~lengthencoding ~length ~mkencoding ~equal ~maximum_size =
  let descr =
    Helpers.with_length_header
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
