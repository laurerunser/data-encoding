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

let rec encoding_to_buffer : type a. Buffer.t -> a t -> unit =
 fun buffer encoding ->
  match encoding with
  | Unit -> Buffer.add_string buffer "unit"
  | Null -> Buffer.add_string buffer "null"
  | Bool -> Buffer.add_string buffer "bool"
  | Int64 -> Buffer.add_string buffer "int64"
  | String -> Buffer.add_string buffer "string"
  | Seq t ->
    Buffer.add_char buffer '[';
    encoding_to_buffer buffer t;
    Buffer.add_string buffer " seq] "
  | Tuple t ->
    Buffer.add_char buffer '[';
    encoding_to_buffer_tuple buffer t;
    Buffer.add_char buffer ']'
  | Object t ->
    Buffer.add_char buffer '{';
    encoding_to_buffer_obj buffer t;
    Buffer.add_char buffer '}'
  | Conv { serialisation = _; deserialisation = _; encoding } ->
    Buffer.add_string buffer "conv[";
    encoding_to_buffer buffer encoding;
    Buffer.add_char buffer ']'
  | Union { cases; serialisation = _; deserialisation = _ } ->
    Buffer.add_string buffer "union(";
    encoding_to_buffer_union buffer cases;
    Buffer.add_char buffer ')'

and encoding_to_buffer_tuple : type a. Buffer.t -> a tuple -> unit =
 fun buffer tuple ->
  match tuple with
  | [] -> ()
  | [ head ] -> encoding_to_buffer buffer head
  | head :: tail ->
    encoding_to_buffer buffer head;
    Buffer.add_string buffer " ; ";
    encoding_to_buffer_tuple buffer tail

and encoding_to_buffer_obj : type a. Buffer.t -> a obj -> unit =
 fun buffer obj ->
  match obj with
  | [] -> ()
  | [ Req { encoding; name } ] ->
    Buffer.add_string buffer name;
    Buffer.add_char buffer '=';
    encoding_to_buffer buffer encoding
  | [ Opt { encoding; name } ] ->
    Buffer.add_string buffer name;
    Buffer.add_string buffer "?=";
    encoding_to_buffer buffer encoding
  | Req { encoding; name } :: tail ->
    Buffer.add_string buffer name;
    Buffer.add_char buffer '=';
    encoding_to_buffer buffer encoding;
    Buffer.add_string buffer " ; ";
    encoding_to_buffer_obj buffer tail
  | Opt { encoding; name } :: tail ->
    Buffer.add_string buffer name;
    Buffer.add_string buffer "?=";
    encoding_to_buffer buffer encoding;
    Buffer.add_string buffer " ; ";
    encoding_to_buffer_obj buffer tail

and encoding_to_buffer_union : type a. Buffer.t -> a anycase list -> unit =
 fun buffer cases ->
  match cases with
  | [] -> ()
  | [ AnyC { tag = _; encoding; inject = _ } ] -> encoding_to_buffer buffer encoding
  | AnyC { tag = _; encoding; inject = _ } :: ts ->
    encoding_to_buffer buffer encoding;
    Buffer.add_string buffer " , ";
    encoding_to_buffer_union buffer ts
;;

let to_string t =
  let b = Buffer.create 512 in
  encoding_to_buffer b t;
  Buffer.contents b
;;

let%expect_test _ =
  let dummy _ = failwith "don't need real serialisation/deserialisation functions" in
  let module M = struct
    type abc =
      | A of bool
      | B of int64
      | C of unit

    type de =
      | D of string
      | E of int64 Seq.t
  end
  in
  let open M in
  let w encoding = print_string (to_string encoding) in
  w Unit;
  [%expect "unit"];
  w Null;
  [%expect "null"];
  w Bool;
  [%expect "bool"];
  w Int64;
  [%expect "int64"];
  w String;
  [%expect "string"];
  w (Seq Int64);
  [%expect "[int64 seq]"];
  w (Seq Unit);
  [%expect "[unit seq]"];
  w (Tuple [ Int64; Bool; String; Null; Unit ]);
  [%expect "[int64 ; bool ; string ; null ; unit]"];
  w (Tuple [ Null; Null ]);
  [%expect "[null ; null]"];
  w (Tuple [ tuple [ String ]; tuple [ Int64; Bool ]; Unit ]);
  [%expect "[[string] ; [int64 ; bool] ; unit]"];
  w (obj []);
  [%expect "{}"];
  w (obj [ req "foo" Unit; opt "bar" Int64 ]);
  [%expect "{foo=unit ; bar?=int64}"];
  w (obj [ opt "one" String; req "two" Bool; req "three" (seq Bool) ]);
  [%expect "{one?=string ; two=bool ; three=[bool seq] }"];
  w (obj [ req "nested_obj" (obj [ req "foo" Bool; req "bar" Null ]) ]);
  [%expect "{nested_obj={foo=bool ; bar=null}}"];
  w (conv ~serialisation:dummy ~deserialisation:dummy Unit);
  [%expect "conv[unit]"];
  w (conv ~serialisation:dummy ~deserialisation:dummy (tuple [ Int64; Bool ]));
  [%expect "conv[[int64 ; bool]]"];
  w
    Union.(
      union
        [ AnyC (case "0" Bool (fun a -> A a))
        ; AnyC (case "1" Int64 (fun b -> B b))
        ; AnyC (case "2" Unit (fun _ -> C ()))
        ]
        dummy
        dummy);
  [%expect "union(bool , int64 , unit)"];
  w
    Union.(
      union
        [ AnyC (case "0" String (fun d -> D d))
        ; AnyC (case "1" (Seq Int64) (fun e -> E e))
        ]
        dummy
        dummy);
  [%expect "union(string , [int64 seq] )"]
;;
