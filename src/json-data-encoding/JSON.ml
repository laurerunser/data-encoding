type t =
  [ `O of (string * t) Seq.t
  | `A of t Seq.t
  | `Bool of bool
  | `Float of float
  | `String of string
  | `Null
  ]
