module Hlist = Commons.Hlist

[@@@warning "-30"]

type _ t =
  | Unit : unit t
  | Int64 : int64 t
  | String : string t
  | Tuple : 'a tuple -> 'a t
  | Object : 'a obj -> 'a t
  | Conv :
      { serialisation : 'a -> 'b
      ; deserialisation : 'b -> ('a, string) result
      ; encoding : 'b t
      }
      -> 'a t

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

val unit : unit t
val int64 : int64 t
val string : string t
val tuple : 'a tuple -> 'a t
val obj : 'a obj -> 'a t
val req : string -> 'a t -> 'a field
val opt : string -> 'a t -> 'a option field

val conv
  :  serialisation:('a -> 'b)
  -> deserialisation:('b -> ('a, string) result)
  -> 'b t
  -> 'a t

module Record : sig
  type ('a, 'r) field

  val field : string -> ('r -> 'a) -> 'a t -> ('a, 'r) field

  type ('mk, 'prod, 'r) fields =
    | [] : ('r, unit, 'r) fields
    | ( :: ) :
        ('a, 'r) field * ('mk, 'prod, 'r) fields
        -> ('a -> 'mk, 'a * 'prod, 'r) fields

  val record : 'mk -> ('mk, 'prod, 'r) fields -> 'r t
end
