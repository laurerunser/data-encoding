module Hlist = Commons.Hlist
module Sizedints = Commons.Sizedints

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
  | `UInt62 ->
    with_header
      ~headerencoding:(Numeral { numeral = UInt62; endianness = Big_endian })
      ~mkheader:(fun v -> Ok (length v))
      ~mkencoding
      ~equal
      ~maximum_size
  | `UInt30 ->
    with_header
      ~headerencoding:(Numeral { numeral = UInt30; endianness = Big_endian })
      ~mkheader:(fun v ->
        let length = length v in
        if Sizedints.Uint30.(to_uint62 max_int) < length
        then Error "Length larger than header-size (30 bits) can encode"
        else
          Ok
            (Option.get
               (Sizedints.Uint30.of_int (Optint.Int63.to_int (length :> Optint.Int63.t)))))
      ~mkencoding:(fun n -> mkencoding (Sizedints.Uint30.to_uint62 n))
      ~equal
      ~maximum_size
  | `UInt16 ->
    with_header
      ~headerencoding:(Numeral { numeral = UInt16; endianness = Big_endian })
      ~mkheader:(fun v ->
        let length = length v in
        if Sizedints.Uint16.(to_uint62 max_int) < length
        then Error "Length larger than header-size (16 bits) can encode"
        else
          Ok
            (Option.get
               (Sizedints.Uint16.of_int (Optint.Int63.to_int (length :> Optint.Int63.t)))))
      ~mkencoding:(fun n -> mkencoding (Sizedints.Uint16.to_uint62 n))
      ~equal
      ~maximum_size
  | `UInt8 ->
    with_header
      ~headerencoding:(Numeral { numeral = UInt8; endianness = Big_endian })
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
  | `UInt62 -> Size_headered { size = UInt62; encoding }
  | `UInt30 -> Size_headered { size = UInt30; encoding }
  | `UInt16 -> Size_headered { size = UInt16; encoding }
  | `UInt8 -> Size_headered { size = UInt8; encoding }
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
  | `UInt62 ->
    with_header
      ~headerencoding:(Numeral { numeral = UInt62; endianness = Big_endian })
      ~mkheader:(fun (s : eltt seq_with_length) -> Ok (Lazy.force s.length))
      ~mkencoding
      ~equal
      ~maximum_size
  | `UInt30 ->
    with_header
      ~headerencoding:(Numeral { numeral = UInt30; endianness = Big_endian })
      ~mkheader:(fun v ->
        let length = Lazy.force v.length in
        if Sizedints.Uint30.(to_uint62 max_int) < length
        then Error "Length larger than header-size (30 bits) can encode"
        else
          Ok
            (Option.get
               (Sizedints.Uint30.of_int (Optint.Int63.to_int (length :> Optint.Int63.t)))))
      ~mkencoding:(fun length ->
        let length = Sizedints.Uint30.to_uint62 length in
        mkencoding length)
      ~equal
      ~maximum_size
  | `UInt16 ->
    with_header
      ~headerencoding:(Numeral { numeral = UInt16; endianness = Big_endian })
      ~mkheader:(fun v ->
        let length = Lazy.force v.length in
        if Sizedints.Uint16.(to_uint62 max_int) < length
        then Error "Length larger than header-size (16 bits) can encode"
        else
          Ok
            (Option.get
               (Sizedints.Uint16.of_int (Optint.Int63.to_int (length :> Optint.Int63.t)))))
      ~mkencoding:(fun length ->
        let length = Sizedints.Uint16.to_uint62 length in
        mkencoding length)
      ~equal
      ~maximum_size
  | `UInt8 ->
    with_header
      ~headerencoding:(Numeral { numeral = UInt8; endianness = Big_endian })
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
