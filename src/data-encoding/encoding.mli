module Hlist = Commons.Hlist

[@@@warning "-30"]

type _ t =
  | Unit : unit t
  | Int64 : int64 t
  | Tuple : 'a tuple -> 'a t
  | Object : 'a obj -> 'a t
  | Split :
      { json : 'a Json_data_encoding.Encoding.t
      ; binary : 'a Binary_data_encoding.Encoding.t
      }
      -> 'a t

and _ tuple =
  | [] : unit Hlist.t tuple
  | ( :: ) : 'a t * 'b Hlist.t tuple -> ('a * 'b) Hlist.t tuple

and _ obj =
  | [] : unit Hlist.t obj
  | ( :: ) : (string * 'a t) * 'b Hlist.t obj -> ('a * 'b) Hlist.t obj

[@@@warning "+30"]

val unit : unit t
val int64 : int64 t
val tuple : 'a tuple -> 'a t
val obj : 'a obj -> 'a t
val split : 'a Json_data_encoding.Encoding.t -> 'a Binary_data_encoding.Encoding.t -> 'a t
val to_json : 'a t -> 'a Json_data_encoding.Encoding.t
val to_binary : 'a t -> 'a Binary_data_encoding.Encoding.t
