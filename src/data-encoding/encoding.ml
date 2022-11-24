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

let unit = Unit
let int64 = Int64
let tuple p = Tuple p
let obj p = Object p
let split json binary = Split { json; binary }

let rec to_json : type a. a t -> a Json_data_encoding.Encoding.t = function
  | Unit -> Json_data_encoding.Encoding.unit
  | Int64 -> Json_data_encoding.Encoding.int64
  | Tuple p -> Json_data_encoding.Encoding.tuple (to_json_tuple p)
  | Object p -> Json_data_encoding.Encoding.obj (to_json_obj p)
  | Split { json; binary = _ } -> json

and to_json_tuple : type a. a tuple -> a Json_data_encoding.Encoding.tuple = function
  | [] -> Json_data_encoding.Encoding.[]
  | t :: ts ->
    let t = to_json t in
    let ts = to_json_tuple ts in
    Json_data_encoding.Encoding.( :: ) (t, ts)

and to_json_obj : type a. a obj -> a Json_data_encoding.Encoding.obj = function
  | [] -> Json_data_encoding.Encoding.[]
  | (n, t) :: ts ->
    let t = to_json t in
    let ts = to_json_obj ts in
    Json_data_encoding.Encoding.( :: ) ((n, t), ts)
;;

let rec to_binary : type a. a t -> a Binary_data_encoding.Encoding.t = function
  | Unit -> Binary_data_encoding.Encoding.unit
  | Int64 -> Binary_data_encoding.Encoding.int64
  | Tuple p -> to_binary_tuple p
  | Object p -> to_binary_obj p
  | Split { json = _; binary } -> binary

and to_binary_tuple : type a. a tuple -> a Binary_data_encoding.Encoding.t = function
  | [] -> Binary_data_encoding.Encoding.[]
  | t :: ts ->
    let t = to_binary t in
    let ts = to_binary_tuple ts in
    Binary_data_encoding.Encoding.( :: ) (t, ts)

and to_binary_obj : type a. a obj -> a Binary_data_encoding.Encoding.t = function
  | [] -> Binary_data_encoding.Encoding.[]
  | (_, t) :: ts ->
    let t = to_binary t in
    let ts = to_binary_obj ts in
    Binary_data_encoding.Encoding.( :: ) (t, ts)
;;
