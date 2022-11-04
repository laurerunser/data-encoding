module Hlist = Commons.Hlist

type _ t =
  | Unit : unit t
  | Int64 : int64 t
  | UInt64 : Unsigned.UInt64.t t
  | Int32 : int32 t
  | UInt32 : Unsigned.UInt32.t t
  | UInt16 : Unsigned.UInt16.t t
  | [] : unit Hlist.t t
  | ( :: ) : 'a t * 'b Hlist.t t -> ('a * 'b) Hlist.t t

let unit = Unit
let int64 = Int64
let uint64 = UInt64
let int32 = Int32
let uint32 = UInt32
let uint16 = UInt16
