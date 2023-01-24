module Hlist = Commons.Hlist
module Sizedints = Commons.Sizedints

type ('step, 'finish) reducer = ('step, 'finish) Descr.reducer =
  | K of 'step
  | Finish of 'finish

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

type 'a seq_with_length = 'a Descr.seq_with_length =
  { seq : 'a Seq.t
  ; length : Sizedints.Uint62.t Lazy.t
  }

(** [α t] is a encoding for values of type [α]. The encoding can be used to
    de/serialise these values—see {!Backend}.

    - Product are supported via the overloading of the list constructors ([[]]
    and [(::)]). This allows compact writing of product encodings of width.

    - [Headered] and [Fold] are low-level constructors. They are meant to be
    used to create advanced encodings which are not supported by the library.
    E.g., they can be used to support lists with a length header. E.g., they can
    be used to support Zarith's [Z.t] values. Note that both of those are
    supported natively by the library; these examples just demonstrate the kind
    of expressive power that [Headered] and [Fold] give to the library user.
*)
type 'a t = 'a Descr.t =
  | Unit : unit t
  | Bool : bool t
  | Numeral :
      { numeral : 'a numeral
      ; endianness : endianness
      }
      -> 'a t
  | String : Sizedints.Uint62.t -> string t
  | Bytes : Sizedints.Uint62.t -> bytes t
  | Array :
      { length : Sizedints.Uint62.t
      ; elementencoding : 'a t
      }
      -> 'a array t
  | LSeq :
      { length : Sizedints.Uint62.t
      ; elementencoding : 'a t
      }
      -> 'a seq_with_length t
  | USeq : { elementencoding : 'a t } -> 'a Seq.t t
  | Option : 'a t -> 'a option t
  | Headered :
      { mkheader : 'a -> ('header, string) result
      ; headerencoding : 'header t
      ; mkencoding : 'header -> ('a t, string) result
      ; equal : 'a -> 'a -> bool
      ; maximum_size : Optint.Int63.t
      }
      -> 'a t
  | Fold :
      { chunkencoding : 'chunk t
      ; chunkify : 'a -> 'chunk Seq.t
      ; readinit : 'acc
      ; reducer : 'acc -> 'chunk -> ('acc, 'a) reducer
      ; equal : 'a -> 'a -> bool
      ; maximum_size : Optint.Int63.t
      }
      -> 'a t
  | Conv :
      { serialisation : 'a -> 'b
      ; deserialisation : 'b -> ('a, string) result
      ; encoding : 'b t
      }
      -> 'a t
  | Size_headered :
      { size : _ numeral
      ; encoding : 'a t
      }
      -> 'a t
  | Size_limit :
      { at_most : Sizedints.Uint62.t
      ; encoding : 'a t
      }
      -> 'a t
  | [] : unit Hlist.t t
  | ( :: ) : 'a t * 'b Hlist.t t -> ('a * 'b) Hlist.t t

val unit : unit t
val bool : bool t
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

type variable_size_spec =
  [ `UInt62
  | `UInt30
  | `UInt16
  | `UInt8
  ]

type size_spec =
  [ `Fixed of Sizedints.Uint62.t
  | variable_size_spec
  ]

val array : size_spec -> 'a t -> 'a array t
val option : 'a t -> 'a option t
val string : size_spec -> string t
val bytes : size_spec -> bytes t

val conv
  :  serialisation:('a -> 'b)
  -> deserialisation:('b -> ('a, string) result)
  -> 'b t
  -> 'a t

val with_header
  :  headerencoding:'h t
  -> mkheader:('a -> ('h, string) result)
  -> mkencoding:('h -> ('a t, string) result)
  -> equal:('a -> 'a -> bool)
  -> maximum_size:Optint.Int63.t
  -> 'a t

val with_length_header
  :  lengthencoding:size_spec
  -> length:('a -> Sizedints.Uint62.t)
  -> mkencoding:(Sizedints.Uint62.t -> ('a t, string) result)
  -> equal:('a -> 'a -> bool)
  -> maximum_size:Optint.Int63.t
  -> 'a t

val with_size_header : sizeencoding:variable_size_spec -> encoding:'a t -> 'a t
val seq_with_length : size_spec -> 'a t -> 'a seq_with_length t
val seq : size_spec -> 'a t -> 'a Seq.t t
val list : size_spec -> 'a t -> 'a list t
val seq_with_size : variable_size_spec -> 'a t -> 'a Seq.t t

val fold
  :  chunkencoding:'chunk t
  -> chunkify:('a -> 'chunk Seq.t)
  -> readinit:'acc
  -> reducer:('acc -> 'chunk -> ('acc, 'a) reducer)
  -> equal:('a -> 'a -> bool)
  -> maximum_size:Optint.Int63.t
  -> 'a t

val ellastic_uint30 : Sizedints.Uint30.t t
