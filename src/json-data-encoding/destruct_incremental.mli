type 'a result =
  | Ok of 'a
  | Error of string
  | Await of (Buffy.Src.t -> 'a result)

val destruct_incremental : 'a Encoding.t -> Buffy.Src.t -> 'a result
