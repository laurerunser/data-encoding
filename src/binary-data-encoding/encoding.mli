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

module Little_endian : sig
  val int64 : int64 t
  val int32 : int32 t
  val uint30 : Sizedints.Uint30.t t
  val uint62 : Sizedints.Uint62.t t
  val uint16 : Sizedints.Uint16.t t
  val uint8 : Sizedints.Uint8.t t
end

val option : 'a t -> 'a option t

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

val string
  :  [ `Fixed of Sizedints.Uint62.t | `UInt62 | `UInt30 | `UInt16 | `UInt8 ]
  -> string t

val bytes
  :  [ `Fixed of Sizedints.Uint62.t | `UInt62 | `UInt30 | `UInt16 | `UInt8 ]
  -> bytes t

val fold
  :  chunkencoding:'chunk t
  -> chunkify:('a -> 'chunk Seq.t)
  -> readinit:'acc
  -> reducer:('acc -> 'chunk -> ('acc, 'a) reducer)
  -> equal:('a -> 'a -> bool)
  -> maximum_size:Optint.Int63.t
  -> 'a t

val ellastic_uint30 : Sizedints.Uint30.t t
