module Hlist = Commons.Hlist

type _ t =
  | Unit : unit t
  | Int64 : int64 t
  | [] : unit Hlist.t t
  | ( :: ) : 'a t * 'b Hlist.t t -> ('a * 'b) Hlist.t t

let unit = Unit
let int64 = Int64
