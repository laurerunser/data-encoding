module Hlist = Commons.Hlist

type _ t =
  | Unit : unit t
  | Int64 : int64 t
  | [] : unit Hlist.t t
  | ( :: ) : 'a t * 'b Hlist.t t -> ('a * 'b) Hlist.t t
  | Split :
      { json : 'a Json_data_encoding.Encoding.t
      ; binary : 'a Binary_data_encoding.Encoding.t
      }
      -> 'a t

let unit = Unit
let int64 = Int64
let split json binary = Split { json; binary }

let rec to_json : type a. a t -> a Json_data_encoding.Encoding.t = function
  | Unit -> Json_data_encoding.Encoding.unit
  | Int64 -> Json_data_encoding.Encoding.int64
  | [] -> Json_data_encoding.Encoding.[]
  | t :: ts ->
    let t = to_json t in
    let ts = to_json ts in
    Json_data_encoding.Encoding.( :: ) (t, ts)
  | Split { json; binary = _ } -> json
;;

let rec to_binary : type a. a t -> a Binary_data_encoding.Encoding.t = function
  | Unit -> Binary_data_encoding.Encoding.unit
  | Int64 -> Binary_data_encoding.Encoding.int64
  | [] -> Binary_data_encoding.Encoding.[]
  | t :: ts ->
    let t = to_binary t in
    let ts = to_binary ts in
    Binary_data_encoding.Encoding.( :: ) (t, ts)
  | Split { json = _; binary } -> binary
;;
