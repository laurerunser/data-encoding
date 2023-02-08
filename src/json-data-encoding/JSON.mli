type t =
  [ `O of (string * t) Seq.t
  | `A of t Seq.t
  | `Bool of bool
  | `Float of float
  | `String of string
  | `Null
  ]

type lexeme =
  [ `Null
  | `Bool of bool
  | `String of string
  | `Float of float
  | `Name of string
  | `As
  | `Ae
  | `Os
  | `Oe
  ]

val lexemify : t -> lexeme Seq.t
val parse : lexeme Seq.t -> (t, string) result
