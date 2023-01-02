module Hlist = Commons.Hlist

type _ t =
  | Unit : unit t
  | Int64 : int64 t
  | UInt64 : Stdint.Uint64.t t
  | Int32 : int32 t
  | UInt32 : Stdint.Uint32.t t
  | UInt16 : Stdint.Uint16.t t
  | UInt8 : Stdint.Uint8.t t
  | String : Stdint.Uint32.t -> string t
  | Bytes : Stdint.Uint32.t -> bytes t
  | Option : 'a t -> 'a option t
  | Headered :
      { mkheader : 'a -> ('header, string) result
      ; headerencoding : 'header t
      ; encoding : 'header -> ('a t, string) result
      ; equal : 'a -> 'a -> bool
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
val int64 : int64 t
val uint64 : Stdint.Uint64.t t
val int32 : int32 t
val uint32 : Stdint.Uint32.t t
val uint16 : Stdint.Uint16.t t
val uint8 : Stdint.Uint8.t t
val option : 'a t -> 'a option t

val conv
  :  serialisation:('a -> 'b)
  -> deserialisation:('b -> ('a, string) result)
  -> 'b t
  -> 'a t

val with_header
  :  'h t
  -> ('a -> ('h, string) result)
  -> ('h -> ('a t, string) result)
  -> ('a -> 'a -> bool)
  -> 'a t

val string : [ `Fixed of Stdint.Uint32.t | `UInt32 | `UInt16 | `UInt8 ] -> string t
val bytes : [ `Fixed of Stdint.Uint32.t | `UInt32 | `UInt16 | `UInt8 ] -> bytes t
