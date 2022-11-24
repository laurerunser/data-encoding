module Hlist = Commons.Hlist

type _ t =
  | Unit : unit t
  | Int64 : int64 t
  | UInt64 : Unsigned.UInt64.t t
  | Int32 : int32 t
  | UInt32 : Unsigned.UInt32.t t
  | UInt16 : Unsigned.UInt16.t t
  | Option : 'a t -> 'a option t
  | [] : unit Hlist.t t
  | ( :: ) : 'a t * 'b Hlist.t t -> ('a * 'b) Hlist.t t

val unit : unit t
val int64 : int64 t
val uint64 : Unsigned.UInt64.t t
val int32 : int32 t
val uint32 : Unsigned.UInt32.t t
val uint16 : Unsigned.UInt16.t t
val option : 'a t -> 'a option t
