module Hlist = Commons.Hlist

[@@@warning "-30"]

type _ t =
  | Unit : unit t
  | Int64 : int64 t
  | String : string t
  | Bytes : bytes t
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
val bytes : bytes t
val tuple : 'a tuple -> 'a t
val obj : 'a obj -> 'a t
val req : string -> 'a t -> 'a field
val opt : string -> 'a t -> 'a option field
val split : 'a Json_data_encoding.Encoding.t -> 'a Binary_data_encoding.Encoding.t -> 'a t
val to_json : 'a t -> 'a Json_data_encoding.Encoding.t
val to_binary : 'a t -> 'a Binary_data_encoding.Encoding.t
