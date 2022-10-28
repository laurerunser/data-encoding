type _ t =
  | Unit : unit t
  | Int64 : int64 t
  | Split :
      { json : 'a Json_data_encoding.Encoding.t
      ; binary : 'a Binary_data_encoding.Encoding.t
      }
      -> 'a t

let unit = Unit
let int64 = Int64
let split json binary = Split { json; binary }

let to_json : type a. a t -> a Json_data_encoding.Encoding.t = function
  | Unit -> Json_data_encoding.Encoding.unit
  | Int64 -> Json_data_encoding.Encoding.int64
  | Split { json; binary = _ } -> json
;;

let to_binary : type a. a t -> a Binary_data_encoding.Encoding.t = function
  | Unit -> Binary_data_encoding.Encoding.unit
  | Int64 -> Binary_data_encoding.Encoding.int64
  | Split { json = _; binary } -> binary
;;
