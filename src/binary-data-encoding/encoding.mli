module Hlist = Commons.Hlist
module Sizedints = Commons.Sizedints

type 'a t

(* simple *)
val unit : unit t
val bool : bool t

(* numerals *)
type endianness = Descr.endianness =
  | Big_endian
  | Little_endian

val int64 : int64 t
val int32 : int32 t
val uint30 : Sizedints.Uint30.t t
val uint62 : Sizedints.Uint62.t t
val uint16 : Sizedints.Uint16.t t
val uint8 : Sizedints.Uint8.t t

module Big_endian : sig
  val int64 : int64 t
  val int32 : int32 t
  val uint30 : Sizedints.Uint30.t t
  val uint62 : Sizedints.Uint62.t t
  val uint16 : Sizedints.Uint16.t t
  val uint8 : Sizedints.Uint8.t t
end

module Little_endian : sig
  val int64 : int64 t
  val int32 : int32 t
  val uint30 : Sizedints.Uint30.t t
  val uint62 : Sizedints.Uint62.t t
  val uint16 : Sizedints.Uint16.t t
  val uint8 : Sizedints.Uint8.t t
end

val default_endianness : endianness

(* collections *)
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

val array : count_spec -> 'a t -> 'a array t
val seq : variable_count_spec -> 'a t -> 'a Seq.t t
val list : variable_count_spec -> 'a t -> 'a list t

module With_size : sig
  (* outside of this module, it is by length (a.k.a. cardinal) rather than size *)
  val seq_with_size : variable_count_spec -> 'a t -> 'a Seq.t t
  (* TODO: others with size too *)
end

(* other type constructors *)
val option : 'a t -> 'a option t
val either : 'l t -> 'r t -> ('l, 'r) Either.t t

(* strings and bytes *)
val string : count_spec -> string t
val bytes : count_spec -> bytes t

val conv
  :  serialisation:('a -> 'b)
  -> deserialisation:('b -> ('a, string) result)
  -> 'b t
  -> 'a t

val ellastic_uint30 : Sizedints.Uint30.t t

(* products *)
type _ tuple =
  | [] : unit Hlist.t tuple
  | ( :: ) : 'a t * 'b Hlist.t tuple -> ('a * 'b) Hlist.t tuple

val tuple : 'a tuple -> 'a t

(* unions *)
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

module Advanced_low_level : sig
  type 'a introspectable = E : (_ Sizability.t, 'a) Descr.t -> 'a introspectable

  val introspect : 'a t -> 'a introspectable
  val forget : 'a introspectable -> 'a t

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
    -> 'eltt seq_with_length t

  val seq_with_length
    :  variable_count_spec
    -> ('eltsz Sizability.intrinsic, 'eltt) Descr.t
    -> (Sizability.dynamic, 'eltt seq_with_length) Descr.t
end

(* may raise based on sizability  
   may raise during serialisation/deserialisation  
   slower because sizability is checked during serialisation/deserialisation  
   TODO? what?
 *)
val with_length_header
  :  lengthencoding:variable_count_spec
  -> length:('a -> Sizedints.Uint62.t)
  -> mkencoding:(Sizedints.Uint62.t -> ('a t, string) result)
  -> equal:('a -> 'a -> bool)
  -> maximum_size:Optint.Int63.t
  -> 'a t

(* may raise based on sizability *)
val with_size_header : sizeencoding:variable_count_spec -> encoding:'a t -> 'a t
