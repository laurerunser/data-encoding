module Hlist = Commons.Hlist
module Hmap = Commons.Hmap
module FieldKeyMap : Stdlib.Map.S with type key = string

[@@@warning "-30"]

type _ t =
  | Unit : unit t
  | Null : unit t
  | Bool : bool t
  | Int64 : int64 t
  | String : string t
  | Seq : 'a t -> 'a Seq.t t
  | Tuple : 'a tuple -> 'a t
  | Object :
      { field_hlist : 'a obj (* ordered list of fields *)
      ; fieldname_key_map : anykey FieldKeyMap.t (* field-name -> key*)
      ; field_hmap : Hmap.t (* key -> field-encoding *)
      }
      -> 'a t
  | Conv :
      { serialisation : 'a -> 'b
      ; deserialisation : 'b -> ('a, string) result
      ; encoding : 'b t
      }
      -> 'a t
  | Union :
      { cases : 'a anycase list
      ; serialisation : 'a -> 'a anycaseandpayload
      ; deserialisation : string -> ('a anycase, string) result
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
      ; fkey : 'a field Hmap.k
      ; vkey : 'a Hmap.k
      }
      -> 'a field
  | Opt :
      { encoding : 'a t
      ; name : string
      ; fkey : 'a option field Hmap.k
      ; vkey : 'a Hmap.k
      }
      -> 'a option field

and anykey = Anykey : _ field Hmap.k -> anykey

and ('payload, 'union) case_descr =
  { tag : string
  ; encoding : 'payload t
  ; inject : 'payload -> 'union
  }

and ('p, 'a) case_and_payload = ('p, 'a) case_descr * 'p
and 'a anycaseandpayload = AnyP : (_, 'a) case_and_payload -> 'a anycaseandpayload
and 'a anycase = AnyC : (_, 'a) case_descr -> 'a anycase

[@@@warning "+30"]

val to_string : 'a t -> string
val value_to_string : 'a t -> 'a -> string
val unit : unit t
val null : unit t
val bool : bool t
val int64 : int64 t
val string : string t
val seq : 'a t -> 'a Seq.t t
val list : 'a t -> 'a list t
val array : 'a t -> 'a array t
val tuple : 'a tuple -> 'a t
val obj : 'a obj -> 'a t
val req : string -> 'a t -> 'a field
val opt : string -> 'a t -> 'a option field

val conv
  :  serialisation:('a -> 'b)
  -> deserialisation:('b -> ('a, string) result)
  -> 'b t
  -> 'a t

val uint8 : Commons.Sizedints.Uint8.t t
val uint30 : Commons.Sizedints.Uint30.t t

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

module Union : sig
  val case_unit : string -> (unit -> 'union) -> (unit, 'union) case_descr
  val case : string -> 'payload t -> ('payload -> 'union) -> ('payload, 'union) case_descr

  val union
    :  'a anycase list
    -> ('a -> 'a anycaseandpayload)
    -> (string -> ('a anycase, string) result)
    -> 'a t

  val either : 'a t -> 'b t -> ('a, 'b) Either.t t
  val option : 'a t -> 'a option t
end
