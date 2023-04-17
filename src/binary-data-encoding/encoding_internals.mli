module Hlist = Commons.Hlist
module Sizedints = Commons.Sizedints

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

val wrap_intrinsic
  :  'sz Sizability.intrinsic
  -> ('sz Sizability.intrinsic, 'a) Descr.t
  -> 'a Descr.anyintrinsic

val with_header
  :  headerencoding:('a Sizability.intrinsic, 'b) Descr.t
  -> mkheader:('c -> ('b, string) result)
  -> mkencoding:('b -> ('c Descr.anyintrinsic, string) result)
  -> equal:('c -> 'c -> bool)
  -> maximum_size:Optint.Int63.t
  -> (Sizability.dynamic, 'c) Descr.t

type ('step, 'finish) reducer = ('step, 'finish) Descr.reducer =
  | K of 'step
  | Finish of 'finish

val fold
  :  chunkencoding:('s Sizability.intrinsic, 'chunk) Descr.t
  -> chunkify:('a -> 'chunk Seq.t)
  -> readinit:'acc
  -> reducer:('acc -> 'chunk -> ('acc, 'a) reducer)
  -> equal:('a -> 'a -> bool)
  -> maximum_size:Optint.Int63.t
  -> (Sizability.dynamic, 'a) Descr.t

val with_length_header
  :  lengthencoding:variable_count_spec
  -> length:('a -> Sizedints.Uint62.t)
  -> mkencoding:(Sizedints.Uint62.t -> ('a Descr.anyintrinsic, string) result)
  -> equal:('a -> 'a -> bool)
  -> maximum_size:Optint.Int63.t
  -> (Sizability.dynamic, 'a) Descr.t

val with_size_header
  :  sizeencoding:variable_count_spec
  -> encoding:(Sizability.extrinsic, 'a) Descr.t
  -> (Sizability.dynamic, 'a) Descr.t

type 'a seq_with_length = 'a Descr.seq_with_length =
  { seq : 'a Seq.t
  ; length : Sizedints.Uint62.t Lazy.t
  }

val seq_with_length_fixed
  :  Sizedints.Uint62.t
  -> ('eltsz Sizability.intrinsic, 'eltt) Descr.t
  -> 'eltt seq_with_length Descr.introspectable

val seq_with_length
  :  variable_count_spec
  -> ('eltsz Sizability.intrinsic, 'eltt) Descr.t
  -> (Sizability.dynamic, 'eltt seq_with_length) Descr.t
