module Hlist = Commons.Hlist
module Sizedints = Commons.Sizedints

type _ t =
  | Unit : unit t
  | Bool : bool t
  | UInt8 : Sizedints.Uint8.t t
  | UInt16 : Sizedints.Uint16.t t
  | UInt30 : Sizedints.Uint30.t t
  | UInt62 : Sizedints.Uint62.t t
  | Int32 : int32 t
  | Int64 : int64 t
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
