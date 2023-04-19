(** {1 JSON representations}

    This module provides multiple OCaml type definitions, each to represent JSON
    values. Along with these different representations, the module provides
    means of converting from one to the other. *)

type literal =
  [ `Bool of bool
  | `Float of float
  | `String of string
  | `Null
  ]

(** [compat] is a high-compatibility JSON representation. It aims to be
    compatible with major JSON libraries of the OCaml ecosystem. It is included
    here to help users interact with other libraries. *)
type compat =
  [ `O of (string * compat) list
  | `A of compat list
  | literal
  ]

(** [lexeme] is a representation of JSON lexemes rather than JSON values. It is
    used below to define [lexemes] which represent JSON values. *)
type lexeme =
  [ literal
  | `Name of string
  | `As
  | `Ae
  | `Os
  | `Oe
  ]

(** [lexemes] is a representation of JSON as a lazy sequence of lexemes. *)
type lexemes = lexeme Seq.t

(** [FieldMap] is a module for maps associating field-names to objects in some
    of the module's JSON reprensentations. *)
module FieldMap : Map.S with type key = string

(** [flex] is a flexible representation. JSON values represented as [flex] do
    not have a canonical form: two identical JSON values could have two distinct
    [flex] representation.

    [flex] is flexible. By allowing multiple representation, it makes it
    possible to avoid some copying and conversions. *)
type flex =
  [ `O of (string * flex) list
  | `Oseq of (string * flex) Seq.t
  | `Omap of flex FieldMap.t
  | `A of flex list
  | `Aarray of flex array
  | `Aseq of flex Seq.t
  | literal
  ]

(** By default we use the flexible representation. *)
type t = flex

val lexemify : t -> lexemes
val parse : lexemes -> (t, string) result
val parse_partial : lexemes -> (t * lexemes, string) result
val compatify : t -> compat

(** Note that [t] is a more general representation than [compat] and so this
    conversion is free. *)
val flexify : compat -> t
