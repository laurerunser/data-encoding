type filter =
  | Field of string
  | Index of int
  | Slice of int * int
  | Iter
  | Comma of filter list
  | Pipe of filter list

val filter : filter -> JSON.t -> JSON.t Seq.t
