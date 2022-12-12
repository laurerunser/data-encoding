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

let to_json { json; binary = _ } = json
let to_binary { json = _; binary } = binary

let unit =
  { json = Json_data_encoding.Encoding.unit; binary = Binary_data_encoding.Encoding.unit }
;;

let int64 =
  { json = Json_data_encoding.Encoding.int64
  ; binary = Binary_data_encoding.Encoding.int64
  }
;;

let string =
  { json = Json_data_encoding.Encoding.string
  ; binary = Binary_data_encoding.Encoding.string
  }
;;

let bytes =
  { json =
      Json_data_encoding.Encoding.(
        conv
          ~serialisation:Bytes.to_string
          ~deserialisation:(fun s -> Ok (Bytes.of_string s))
          string)
  ; binary = Binary_data_encoding.Encoding.bytes
  }
;;

let conv ~serialisation ~deserialisation encoding =
  { json =
      Json_data_encoding.Encoding.conv ~serialisation ~deserialisation (to_json encoding)
  ; binary =
      Binary_data_encoding.Encoding.conv
        ~serialisation
        ~deserialisation
        (to_binary encoding)
  }
;;

let rec to_json_tuple : type a. a tuple -> a Json_data_encoding.Encoding.tuple = function
  | [] -> Json_data_encoding.Encoding.[]
  | t :: ts ->
    let t = to_json t in
    let ts = to_json_tuple ts in
    Json_data_encoding.Encoding.( :: ) (t, ts)
;;

let rec to_binary_tuple : type a. a tuple -> a Binary_data_encoding.Encoding.t = function
  | [] -> Binary_data_encoding.Encoding.[]
  | t :: ts ->
    let t = to_binary t in
    let ts = to_binary_tuple ts in
    Binary_data_encoding.Encoding.( :: ) (t, ts)
;;

let tuple p =
  { json = Json_data_encoding.Encoding.tuple (to_json_tuple p)
  ; binary = to_binary_tuple p
  }
;;

let rec to_json_obj : type a. a obj -> a Json_data_encoding.Encoding.obj = function
  | [] -> Json_data_encoding.Encoding.[]
  | Req { encoding = t; name } :: ts ->
    let t = to_json t in
    let ts = to_json_obj ts in
    Json_data_encoding.Encoding.( :: ) (Req { encoding = t; name }, ts)
  | Opt { encoding = t; name } :: ts ->
    let t = to_json t in
    let ts = to_json_obj ts in
    Json_data_encoding.Encoding.( :: ) (Opt { encoding = t; name }, ts)
;;

let rec to_binary_obj : type a. a obj -> a Binary_data_encoding.Encoding.t = function
  | [] -> Binary_data_encoding.Encoding.[]
  | Req { encoding = t; name = _ } :: ts ->
    let t = to_binary t in
    let ts = to_binary_obj ts in
    Binary_data_encoding.Encoding.( :: ) (t, ts)
  | Opt { encoding = t; name = _ } :: ts ->
    let t = to_binary t in
    let ts = to_binary_obj ts in
    Binary_data_encoding.Encoding.( :: ) (Binary_data_encoding.Encoding.option t, ts)
;;

let obj p =
  { json = Json_data_encoding.Encoding.obj (to_json_obj p); binary = to_binary_obj p }
;;

let req name encoding = Req { encoding; name }
let opt name encoding = Opt { encoding; name }
let split json binary = { json; binary }
