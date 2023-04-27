module Hlist = Commons.Hlist

[@@@warning "-30"]

type _ t =
  | Unit : unit t
  | Null : unit t
  | Bool : bool t
  | Int64 : int64 t
  | String : string t
  | Seq : 'a t -> 'a Seq.t t
  | Tuple : 'a tuple -> 'a t
  | Object : 'a obj -> 'a t
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
      }
      -> 'a field
  | Opt :
      { encoding : 'a t
      ; name : string
      }
      -> 'a option field

and ('payload, 'union) case_descr =
  { tag : string
  ; encoding : 'payload t
  ; inject : 'payload -> 'union
  }

and ('p, 'a) case_and_payload = ('p, 'a) case_descr * 'p
and 'a anycaseandpayload = AnyP : (_, 'a) case_and_payload -> 'a anycaseandpayload
and 'a anycase = AnyC : (_, 'a) case_descr -> 'a anycase

[@@@warning "+30"]

let unit = Unit
let null = Null
let bool = Bool
let int64 = Int64
let string = String
let seq t = Seq t
let tuple t = Tuple t

module FieldSet = Stdlib.Set.Make (String)

let rec obj_has_duplicate_field_name : type o. FieldSet.t -> o obj -> bool =
 fun seen o ->
  match o with
  | [] -> false
  | Req { name; encoding = _ } :: fields ->
    FieldSet.mem name seen || obj_has_duplicate_field_name (FieldSet.add name seen) fields
  | Opt { name; encoding = _ } :: fields ->
    FieldSet.mem name seen || obj_has_duplicate_field_name (FieldSet.add name seen) fields
;;

let obj t =
  if obj_has_duplicate_field_name FieldSet.empty t
  then raise (Invalid_argument "Object has duplicate field name");
  Object t
;;

let req name encoding = Req { encoding; name }
let opt name encoding = Opt { encoding; name }

let conv ~serialisation ~deserialisation encoding =
  Conv { serialisation; deserialisation; encoding }
;;

let list t =
  conv
    ~serialisation:List.to_seq
    ~deserialisation:(fun s -> Result.ok (List.of_seq s))
    (seq t)
;;

let array t =
  conv
    ~serialisation:Array.to_seq
    ~deserialisation:(fun s -> Result.ok (Array.of_seq s))
    (seq t)
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

let rec union_cases_has_duplicate_tag seen : _ anycase list -> bool = function
  | [] -> false
  | AnyC { tag; encoding = _; inject = _ } :: cases ->
    FieldSet.mem tag seen || union_cases_has_duplicate_tag (FieldSet.add tag seen) cases
;;

module Union = struct
  let case tag encoding inject = { tag; encoding; inject }
  let case_unit tag inject = { tag; encoding = Unit; inject }

  let union cases serialisation deserialisation =
    if union_cases_has_duplicate_tag FieldSet.empty cases
    then raise (Invalid_argument "Union case contains duplicate tag");
    Union { cases; serialisation; deserialisation }
  ;;

  let either l r =
    let lc = case "Left" l Either.left in
    let anyclc = AnyC lc in
    let rc = case "Right" r Either.right in
    let anycrc = AnyC rc in
    union
      [ anyclc; anycrc ]
      (function
       | Either.Left x -> AnyP (lc, x)
       | Either.Right y -> AnyP (rc, y))
      (function
       | "Left" -> Ok anyclc
       | "Right" -> Ok anycrc
       | _ -> Error "Unexpected tag in union")
  ;;

  let option t =
    let nc = case "None" unit (fun () -> None) in
    let anycnc = AnyC nc in
    let sc = case "Some" t Option.some in
    let anycsc = AnyC sc in
    union
      [ anycnc; anycsc ]
      (function
       | None -> AnyP (nc, ())
       | Some v -> AnyP (sc, v))
      (function
       | "None" -> Ok anycnc
       | "Some" -> Ok anycsc
       | _ -> Error "Unexpected tag in union")
  ;;
end
