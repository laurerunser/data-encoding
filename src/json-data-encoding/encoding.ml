module Hlist = Commons.Hlist

[@@@warning "-30"]

type _ t =
  | Unit : unit t
  | Int64 : int64 t
  | Tuple : 'a tuple -> 'a t
  | Object : 'a obj -> 'a t

and _ tuple =
  | [] : unit Hlist.t tuple
  | ( :: ) : 'a t * 'b Hlist.t tuple -> ('a * 'b) Hlist.t tuple

and _ obj =
  | [] : unit Hlist.t obj
  | ( :: ) : 'a field * 'b Hlist.t obj -> ('a * 'b) Hlist.t obj

and _ field =
  | Req :
      { encoding : 'a t
      ; name : string
      }
      -> 'a field
  | Opt :
      { encoding : 'a t
      ; name : string
      }
      -> 'a option field

[@@@warning "+30"]

let unit = Unit
let int64 = Int64
let tuple t = Tuple t
let obj t = Object t
let req name encoding = Req { encoding; name }
let opt name encoding = Opt { encoding; name }
