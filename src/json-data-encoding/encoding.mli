type _ t =
  | Unit : unit t
  | Int64 : int64 t

val unit : unit t
val int64 : int64 t
