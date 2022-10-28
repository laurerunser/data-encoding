type _ t =
  | Unit : unit t
  | Int64 : int64 t
  | Split :
      { json : 'a Json_data_encoding.Encoding.t
      ; binary : 'a Binary_data_encoding.Encoding.t
      }
      -> 'a t

val unit : unit t
val int64 : int64 t
val split : 'a Json_data_encoding.Encoding.t -> 'a Binary_data_encoding.Encoding.t -> 'a t
val to_json : 'a t -> 'a Json_data_encoding.Encoding.t
val to_binary : 'a t -> 'a Binary_data_encoding.Encoding.t
