module Hlist = Commons.Hlist

[@@@warning "-30"]

type _ t =
  | Unit : unit t
  | Bool : bool t
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

let unit = Unit
let bool = Bool
let int64 = Int64
let string = String
let tuple t = Tuple t
let obj t = Object t
let req name encoding = Req { encoding; name }
let opt name encoding = Opt { encoding; name }

let conv ~serialisation ~deserialisation encoding =
  Conv { serialisation; deserialisation; encoding }
;;

module Record = struct
  type ('a, 'r) field =
    { name : string
    ; read : 'r -> 'a
    ; encoding : 'a t
    }

  let field name read encoding = { name; read; encoding }

  type ('mk, 'prod, 'r) fields =
    | [] : ('r, unit, 'r) fields
    | ( :: ) :
        ('a, 'r) field * ('mk, 'prod, 'r) fields
        -> ('a -> 'mk, 'a * 'prod, 'r) fields

  let record : type mk prod r. mk -> (mk, prod, r) fields -> r t =
   fun mk fields ->
    let serialisation r =
      let rec map : type mk prod. (mk, prod, r) fields -> prod Hlist.t =
       fun fields ->
        match fields with
        | [] -> []
        | { read; _ } :: fields -> read r :: map fields
      in
      map fields
    in
    let deserialisation h =
      let rec map : type mk prod. (mk, prod, r) fields -> prod Hlist.t -> mk -> r =
       fun fields prod mk ->
        match fields with
        | [] ->
          let [] = prod in
          mk
        | _ :: fields ->
          let (v :: prod) = prod in
          map fields prod (mk v)
      in
      Ok (map fields h mk)
    in
    let encoding =
      let rec map : type mk prod. (mk, prod, r) fields -> prod Hlist.t obj = function
        | [] -> []
        | { name; encoding; _ } :: fields -> req name encoding :: map fields
      in
      obj (map fields)
    in
    conv ~serialisation ~deserialisation encoding
 ;;
end
