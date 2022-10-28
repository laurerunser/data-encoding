type _ t =
  | Unit : unit t
  | Int64 : int64 t

let unit = Unit
let int64 = Int64
