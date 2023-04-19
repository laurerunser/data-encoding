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

  let with_header ~headerdescr ~mkheader ~descr_of_header ~equal ~maximum_size =
    Descr.Headered { mkheader; headerdescr; descr_of_header; equal; maximum_size }
  ;;

  let fold ~chunkdescr ~chunkify ~readinit ~reducer ~equal ~maximum_size =
    Descr.Fold { chunkdescr; chunkify; readinit; reducer; equal; maximum_size }
  ;;

  let with_length_header
    : type a.
      length_spec:variable_count_spec
      -> length:(a -> Sizedints.Uint62.t)
      -> descr_of_header:(Sizedints.Uint62.t -> (a Descr.anyintrinsic, string) result)
      -> equal:(a -> a -> bool)
      -> maximum_size:Optint.Int63.t
      -> (Sizability.dynamic, a) Descr.t
    =
   fun ~length_spec ~length ~descr_of_header ~equal ~maximum_size ->
    match length_spec with
    | `Uint62 ->
      with_header
        ~headerdescr:(Numeral { numeral = Uint62; endianness = Big_endian })
        ~mkheader:(fun v -> Ok (length v))
        ~descr_of_header
        ~equal
        ~maximum_size
    | `Uint30 ->
      with_header
        ~headerdescr:(Numeral { numeral = Uint30; endianness = Big_endian })
        ~mkheader:(fun v ->
          let length = length v in
          if Sizedints.Uint30.(to_uint62 max_int) < length
          then Error "Length larger than header-size (30 bits) can encode"
          else
            Ok
              (Option.get
                 (Sizedints.Uint30.of_int
                    (Optint.Int63.to_int (length :> Optint.Int63.t)))))
        ~descr_of_header:(fun n -> descr_of_header (Sizedints.Uint30.to_uint62 n))
        ~equal
        ~maximum_size
    | `Uint16 ->
      with_header
        ~headerdescr:(Numeral { numeral = Uint16; endianness = Big_endian })
        ~mkheader:(fun v ->
          let length = length v in
          if Sizedints.Uint16.(to_uint62 max_int) < length
          then Error "Length larger than header-size (16 bits) can encode"
          else
            Ok
              (Option.get
                 (Sizedints.Uint16.of_int
                    (Optint.Int63.to_int (length :> Optint.Int63.t)))))
        ~descr_of_header:(fun n -> descr_of_header (Sizedints.Uint16.to_uint62 n))
        ~equal
        ~maximum_size
    | `Uint8 ->
      with_header
        ~headerdescr:(Numeral { numeral = Uint8; endianness = Big_endian })
        ~mkheader:(fun v ->
          let length = length v in
          if Sizedints.Uint8.(to_uint62 max_int) < length
          then Error "Length larger than header-size (16 bits) can encode"
          else
            Ok
              (Option.get
                 (Sizedints.Uint8.of_int (Optint.Int63.to_int (length :> Optint.Int63.t)))))
        ~descr_of_header:(fun n -> descr_of_header (Sizedints.Uint8.to_uint62 n))
        ~equal
        ~maximum_size
 ;;

  let with_size_header
    : type a.
      size_spec:variable_count_spec
      -> descr:(Sizability.extrinsic, a) Descr.t
      -> (Sizability.dynamic, a) Descr.t
    =
   fun ~size_spec ~descr ->
    match size_spec with
    | `Uint62 -> Size_headered { size = Uint62; descr }
    | `Uint30 -> Size_headered { size = Uint30; descr }
    | `Uint16 -> Size_headered { size = Uint16; descr }
    | `Uint8 -> Size_headered { size = Uint8; descr }
 ;;

  let seq_with_length_fixed length descr = Descr.E (LSeq { length; descr })

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
   fun length_spec descr ->
    let maximum_size =
      (* TODO *)
      Optint.Int63.max_int
    in
    let equal a b = Seq.equal (Query.equal_of descr) a.seq b.seq in
    let descr_of_header length =
      Ok (wrap_intrinsic (Query.sizability descr) (LSeq { length; descr }))
    in
    match length_spec with
    | `Uint62 ->
      with_header
        ~headerdescr:(Numeral { numeral = Uint62; endianness = Big_endian })
        ~mkheader:(fun (s : eltt seq_with_length) -> Ok (Lazy.force s.length))
        ~descr_of_header
        ~equal
        ~maximum_size
    | `Uint30 ->
      with_header
        ~headerdescr:(Numeral { numeral = Uint30; endianness = Big_endian })
        ~mkheader:(fun v ->
          let length = Lazy.force v.length in
          if Sizedints.Uint30.(to_uint62 max_int) < length
          then Error "Length larger than header-size (30 bits) can encode"
          else
            Ok
              (Option.get
                 (Sizedints.Uint30.of_int
                    (Optint.Int63.to_int (length :> Optint.Int63.t)))))
        ~descr_of_header:(fun length ->
          let length = Sizedints.Uint30.to_uint62 length in
          descr_of_header length)
        ~equal
        ~maximum_size
    | `Uint16 ->
      with_header
        ~headerdescr:(Numeral { numeral = Uint16; endianness = Big_endian })
        ~mkheader:(fun v ->
          let length = Lazy.force v.length in
          if Sizedints.Uint16.(to_uint62 max_int) < length
          then Error "Length larger than header-size (16 bits) can encode"
          else
            Ok
              (Option.get
                 (Sizedints.Uint16.of_int
                    (Optint.Int63.to_int (length :> Optint.Int63.t)))))
        ~descr_of_header:(fun length ->
          let length = Sizedints.Uint16.to_uint62 length in
          descr_of_header length)
        ~equal
        ~maximum_size
    | `Uint8 ->
      with_header
        ~headerdescr:(Numeral { numeral = Uint8; endianness = Big_endian })
        ~mkheader:(fun v ->
          let length = Lazy.force v.length in
          if Sizedints.Uint8.(to_uint62 max_int) < length
          then Error "Length larger than header-size (16 bits) can encode"
          else
            Ok
              (Option.get
                 (Sizedints.Uint8.of_int (Optint.Int63.to_int (length :> Optint.Int63.t)))))
        ~descr_of_header:(fun length ->
          let length = Sizedints.Uint8.to_uint62 length in
          descr_of_header length)
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
     | Intrinsic (Static _) -> E (TupCons { tupler = TAnyStatic; head; tail })
     | Intrinsic Dynamic ->
       (match Query.sizability head with
        | Intrinsic _ -> E (TupCons { tupler = TIntrinsicDynamic; head; tail })
        | Extrinsic -> failwith "forbidden extrinsic-dynamic construction in tuple")
     | Extrinsic ->
       (match Query.sizability head with
        | Intrinsic _ -> E (TupCons { tupler = TIntrinsicExtrinsic; head; tail })
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

let option (E descr) =
  (* TODO? only allow extrinsic in the advanced low-level module?? *)
  match Query.sizability descr with
  | Intrinsic _ -> E (Option { optioner = OIntrinsic; descr })
  | Extrinsic -> E (Option { optioner = OExtrinsic; descr })
;;

let conv ~serialisation ~deserialisation (E descr) =
  E (Conv { serialisation; deserialisation; descr })
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
   fun length_spec (E descr) ->
    match length_spec with
    | `Fixed length ->
      (match Query.sizability descr with
       | Extrinsic -> raise (Invalid_argument "extrinsic in array")
       | Intrinsic _ -> E (Array { length; descr }))
    | #variable_count_spec as length_spec ->
      (match Query.sizability descr with
       | Extrinsic -> raise (Invalid_argument "extrinsic in array")
       | Intrinsic _ ->
         let descr =
           Helpers.with_length_header
             ~length_spec
             ~length:(fun a -> Option.get (Sizedints.Uint62.of_int (Array.length a)))
             ~descr_of_header:(fun length ->
               Ok
                 (Helpers.wrap_intrinsic
                    (Query.sizability descr)
                    (Array { length; descr })))
             ~equal:(fun xs ys ->
               Array.length xs = Array.length ys
               && Array.for_all2 (Query.equal_of descr) xs ys)
             ~maximum_size:
               (let maximum_length =
                  match length_spec with
                  | `Uint8 -> Sizedints.Uint8.(to_uint62 max_int)
                  | `Uint16 -> Sizedints.Uint16.(to_uint62 max_int)
                  | `Uint30 -> Sizedints.Uint30.(to_uint62 max_int)
                  | `Uint62 -> Sizedints.Uint62.max_int
                in
                Optint.Int63.mul
                  (maximum_length :> Optint.Int63.t)
                  (Query.maximum_size_of descr))
         in
         E descr)
 ;;
end

include With_length

module With_size = struct
  let seq (size_spec : variable_count_spec) (E descr) =
    match Query.sizability descr with
    | Intrinsic (Static n) ->
      if n = Sizedints.Uint62.zero
      then
        raise (Invalid_argument "zero-size elements in sized (rather than lengthed) seq")
      else E (Helpers.with_size_header ~size_spec ~descr:(USeq { descr }))
    | Extrinsic -> raise (Invalid_argument "extrinsic-size elements in sized seq")
    | Intrinsic _ -> E (Helpers.with_size_header ~size_spec ~descr:(USeq { descr }))
  ;;

  let list (size_spec : variable_count_spec) e_descr =
    conv
      ~serialisation:List.to_seq
      ~deserialisation:(fun s -> Ok (List.of_seq s))
      (seq size_spec e_descr)
  ;;

  let array (size_spec : variable_count_spec) e_descr =
    conv
      ~serialisation:Array.to_seq
      ~deserialisation:(fun s -> Ok (Array.of_seq s))
      (seq size_spec e_descr)
  ;;
end

let string length_spec =
  match length_spec with
  | `Fixed length -> E (String length)
  | #variable_count_spec as length_spec ->
    let descr =
      Helpers.with_length_header
        ~length_spec
        ~length:(fun s ->
          Option.get (Sizedints.Uint62.of_int64 (Int64.of_int (String.length s))))
        ~descr_of_header:(fun n -> Ok (EStatic (String n)))
        ~equal:String.equal
        ~maximum_size:
          (match length_spec with
           | `Uint8 -> (Sizedints.Uint8.(to_uint62 max_int) :> Optint.Int63.t)
           | `Uint16 -> (Sizedints.Uint16.(to_uint62 max_int) :> Optint.Int63.t)
           | `Uint30 -> (Sizedints.Uint30.(to_uint62 max_int) :> Optint.Int63.t)
           | `Uint62 -> (Sizedints.Uint62.max_int :> Optint.Int63.t))
    in
    E descr
;;

let bytes length_spec =
  match length_spec with
  | `Fixed length -> E (Bytes length)
  | #variable_count_spec as length_spec ->
    let descr =
      Helpers.with_length_header
        ~length_spec
        ~length:(fun b ->
          Option.get (Sizedints.Uint62.of_int64 (Int64.of_int (Bytes.length b))))
        ~descr_of_header:(fun n -> Ok (EStatic (Bytes n)))
        ~equal:Bytes.equal
        ~maximum_size:
          (match length_spec with
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
      ~chunkdescr:(Numeral { numeral = Uint8; endianness = Big_endian })
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

  let case tag (E descr) inject =
    match Query.sizability descr with
    | Extrinsic -> raise (Invalid_argument "extrinsic payload encoding in tag")
    | Intrinsic _ -> ECase { tag; descr; inject }
  ;;

  let case_unit tag inject = ECase { tag; descr = Unit; inject }

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

let with_size_limit limit (E descr) =
  match Commons.Sizedints.Uint62.of_int limit with
  | None -> raise (Invalid_argument "size limit cannot be negative")
  | Some at_most -> E (Size_limit { at_most; descr })
;;

let with_length_header ~length_spec ~length ~mkencoding ~equal ~maximum_size =
  let descr =
    Helpers.with_length_header
      ~length_spec
      ~length
      ~descr_of_header:(fun n ->
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
