module Hlist = Commons.Hlist

[@@@warning "-30"]

type 'a t =
  { json : 'a Json_data_encoding.Encoding.t
  ; binary : 'a Binary_data_encoding.Encoding.t
  }

type _ tuple =
  | [] : unit Hlist.t tuple
  | ( :: ) : 'a t * 'b Hlist.t tuple -> ('a * 'b) Hlist.t tuple

type _ obj =
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
val bool : bool t
val int64 : int64 t
val string : Binary_data_encoding.Encoding.count_spec -> string t
val bytes : bytes t
val list : Binary_data_encoding.Encoding.count_spec -> 'a t -> 'a list t
val option : 'a t -> 'a option t

val conv
  :  serialisation:('a -> 'b)
  -> deserialisation:('b -> ('a, string) result)
  -> 'b t
  -> 'a t

val tuple : 'a tuple -> 'a t
val obj : 'a obj -> 'a t
val req : string -> 'a t -> 'a field
val opt : string -> 'a t -> 'a option field
val split : 'a Json_data_encoding.Encoding.t -> 'a Binary_data_encoding.Encoding.t -> 'a t
val to_json : 'a t -> 'a Json_data_encoding.Encoding.t
val to_binary : 'a t -> 'a Binary_data_encoding.Encoding.t

module Union : sig
  type ('tag, 'payload, 'union) case_descr =
    { json : ('payload, 'union) Json_data_encoding.Encoding.case_descr
    ; binary : ('tag, 'payload, 'union) Binary_data_encoding.Encoding.case_descr
    }

  type ('tag, 'p, 'a) case_and_payload = ('tag, 'p, 'a) case_descr * 'p
  type ('tag, 'a) anycase = AnyC : ('tag, _, 'a) case_descr -> ('tag, 'a) anycase

  type ('tag, 'a) anycaseandpayload =
    | AnyP : ('tag, _, 'a) case_and_payload -> ('tag, 'a) anycaseandpayload

  val case
    :  'tag
    -> string
    -> 'payload t
    -> ('payload -> 'union)
    -> ('tag, 'payload, 'union) case_descr

  val case_unit : 'tag -> string -> (unit -> 'union) -> ('tag, unit, 'union) case_descr

  val union
    :  'tag Binary_data_encoding.Encoding.t
    -> ('tag, 'a) anycase list
    -> ('a -> ('tag, 'a) anycaseandpayload)
    -> ('tag -> (('tag, 'a) anycase, string) result)
    -> (string -> (('tag, 'a) anycase, string) result)
    -> 'a t

  val either : 'l t -> 'r t -> ('l, 'r) Either.t t
end
