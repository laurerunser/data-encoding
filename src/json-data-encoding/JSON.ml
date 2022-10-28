type t =
  [ `O of (string * t) list
  | `A of t list
  | `Bool of bool
  | `Float of float
  | `String of string
  | `Null
  ]
