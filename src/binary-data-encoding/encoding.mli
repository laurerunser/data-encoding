module Hlist = Commons.Hlist
module Sizedints = Commons.Sizedints

(** {1 Prelude}

    This section contains basic type definitions. *)

(** [variable_count_spec] are values used to specify how a numerical header is
    represented in binary. For example, a list may be preceeded by a header
    indication how many elements it has; this count can take different width. *)
type variable_count_spec =
  [ `Uint62
  | `Uint30
  | `Uint16
  | `Uint8
  ]

(** [count_spec] are either [variable_count_spec] or fixed-counts which do not
    appear in binary because they are statically known. *)
type count_spec =
  [ `Fixed of Sizedints.Uint62.t
  | variable_count_spec
  ]

(** A [numeral] specifies one of the common integer ranges. *)
type 'a numeral = 'a Sizedints.numeral =
  | Uint8 : Sizedints.Uint8.t numeral
  | Uint16 : Sizedints.Uint16.t numeral
  | Uint30 : Sizedints.Uint30.t numeral
  | Uint62 : Sizedints.Uint62.t numeral
  | Int32 : int32 numeral
  | Int64 : int64 numeral

(** An [endianness] specifies the byte order of a numeral. *)
type endianness = Descr.endianness =
  | Big_endian
  | Little_endian

(** {1 Core type} *)

(** ['a t] is the types of encodings for values of the type ['a].  *)
type 'a t

(** [introspect t] returns the (wrapped) [Descr.t] corresponding to [t]. *)
val introspect : 'a t -> 'a Descr.introspectable

(** [detrospect d] returns the encoding corresponding to the (wrapped) [d]. *)
val detrospect : 'a Descr.introspectable -> 'a t

(** {1 Combinators} *)

(** {2 Simple ground types} *)

val unit : unit t
val bool : bool t

(** {2 Strings and bytes} *)

val string : count_spec -> string t
val bytes : count_spec -> bytes t

(** {2 Numerals} *)

module type Endianed = sig
  val int64 : int64 t
  val int32 : int32 t
  val uint30 : Sizedints.Uint30.t t
  val uint62 : Sizedints.Uint62.t t
  val uint16 : Sizedints.Uint16.t t
  val uint8 : Sizedints.Uint8.t t
end

module Big_endian : Endianed

(** Big-endian is the default *)
include module type of Big_endian

(** [default_endianness] is [Big_endian] *)
val default_endianness : endianness

module Little_endian : Endianed

val ellastic_uint30 : Sizedints.Uint30.t t

(** {2 Collections} *)

(** In [With_length] the collections are preceeded by a header indicating the
    length of the collection (the number of elements). (Or nothing if the
    [count_spec] is [`Fixed].) *)
module With_length : sig
  val array : count_spec -> 'a t -> 'a array t
  val seq : count_spec -> 'a t -> 'a Seq.t t
  val list : count_spec -> 'a t -> 'a list t
end

include module type of With_length

(** In [With_size] the collections are preceeded by a header indicating the size
    in bytes.  *)
module With_size : sig
  val seq : variable_count_spec -> 'a t -> 'a Seq.t t
  val array : variable_count_spec -> 'a t -> 'a array t
  val list : variable_count_spec -> 'a t -> 'a list t
end

(** {2 Other common type constructors} *)

val option : 'a t -> 'a option t
val either : 'l t -> 'r t -> ('l, 'r) Either.t t

(** {2 Product types} *)

type _ tuple =
  | [] : unit Hlist.t tuple
  | ( :: ) : 'a t * 'b Hlist.t tuple -> ('a * 'b) Hlist.t tuple

val tuple : 'a tuple -> 'a t

(** {2 Sum types} *)

type ('tag, 'payload, 'union) case_descr
type ('tag, 'a) anycase

val anycase : ('tag, _, 'union) case_descr -> ('tag, 'union) anycase

type ('tag, 'a) anycaseandpayload

val case_and_payload
  :  ('tag, 'payload, 'union) case_descr
  -> 'payload
  -> ('tag, 'union) anycaseandpayload

val case
  :  'tag
  -> 'payload t
  -> ('payload -> 'union)
  -> ('tag, 'payload, 'union) case_descr

val case_unit : 'tag -> (unit -> 'union) -> ('tag, unit, 'union) case_descr

val union
  :  'tag t
  -> ('tag, 'a) anycase list
  -> ('a -> ('tag, 'a) anycaseandpayload)
  -> ('tag -> (('tag, 'a) anycase, string) result)
  -> 'a t

(** {2 Multi-purpose combinator} *)
val conv
  :  serialisation:('a -> 'b)
  -> deserialisation:('b -> ('a, string) result)
  -> 'b t
  -> 'a t

(** [with_size_limit limit e] is [e] but only [limit] bytes can be read
    (resp. written) during the deserialisation (resp serialisation).

    @raise Invalid_argument if [limit<0] *)
val with_size_limit : int -> 'a t -> 'a t

(** {1 Advanced helper functions}

    You should only need this if you are trying to define some advanced encoding
    for some custom strange types. *)

module Helpers : sig
  val wrap_intrinsic
    :  'sz Sizability.intrinsic
    -> ('sz Sizability.intrinsic, 'a) Descr.t
    -> 'a Descr.anyintrinsic

  val with_header
    :  headerdescr:('a Sizability.intrinsic, 'b) Descr.t
    -> mkheader:('c -> ('b, string) result)
    -> descr_of_header:('b -> ('c Descr.anyintrinsic, string) result)
    -> equal:('c -> 'c -> bool)
    -> maximum_size:Optint.Int63.t
    -> (Sizability.dynamic, 'c) Descr.t

  type ('step, 'finish) reducer = ('step, 'finish) Descr.reducer =
    | K of 'step
    | Finish of 'finish

  val fold
    :  chunkdescr:('s Sizability.intrinsic, 'chunk) Descr.t
    -> chunkify:('a -> 'chunk Seq.t)
    -> readinit:'acc
    -> reducer:('acc -> 'chunk -> ('acc, 'a) reducer)
    -> equal:('a -> 'a -> bool)
    -> maximum_size:Optint.Int63.t
    -> (Sizability.dynamic, 'a) Descr.t

  val with_length_header
    :  length_spec:variable_count_spec
    -> length:('a -> Sizedints.Uint62.t)
    -> descr_of_header:(Sizedints.Uint62.t -> ('a Descr.anyintrinsic, string) result)
    -> equal:('a -> 'a -> bool)
    -> maximum_size:Optint.Int63.t
    -> (Sizability.dynamic, 'a) Descr.t

  val with_size_header
    :  size_spec:variable_count_spec
    -> descr:(Sizability.extrinsic, 'a) Descr.t
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
end
