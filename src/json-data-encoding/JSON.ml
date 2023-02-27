type literal =
  [ `Bool of bool
  | `Float of float
  | `String of string
  | `Null
  ]

type compat =
  [ `O of (string * compat) list
  | `A of compat list
  | literal
  ]

type lexeme =
  [ literal
  | `Name of string
  | `As
  | `Ae
  | `Os
  | `Oe
  ]

type lexemes = lexeme Seq.t

module FieldMap = Map.Make (String)

type flex =
  [ `O of (string * flex) list
  | `Oseq of (string * flex) Seq.t
  | `Omap of flex FieldMap.t
  | `A of flex list
  | `Aarray of flex array
  | `Aseq of flex Seq.t
  | literal
  ]

type t = flex

open Commons.Sequtils

let rec lexemify : t -> lexeme Seq.t = function
  | `Oseq fs -> bracket `Os (lexemify_object fs) `Oe
  | `O fs -> lexemify (`Oseq (List.to_seq fs))
  | `Omap fs -> lexemify (`Oseq (FieldMap.to_seq fs))
  | `Aseq ts -> bracket `As (lexemify_array ts) `Ae
  | `A ts -> lexemify (`Aseq (List.to_seq ts))
  | `Aarray ts -> lexemify (`Aseq (Array.to_seq ts))
  | (`Bool _ | `Float _ | `String _ | `Null) as t -> Seq.return t

and lexemify_object : (string * t) Seq.t -> lexeme Seq.t =
 fun fs -> Seq.flat_map (fun (name, t) -> Seq.cons (`Name name) (lexemify t)) fs

and lexemify_array : t Seq.t -> lexeme Seq.t =
 fun fs -> Seq.flat_map (fun t -> lexemify t) fs
;;

let ( let* ) = Result.bind

let rec parse : lexeme Seq.t -> (t * lexeme Seq.t, string) result =
 fun s ->
  match s () with
  | Seq.Nil -> Error "Unexpected end of lexeme"
  | Seq.Cons (c, s) ->
    (match c with
     | `Os ->
       let* o, s = parse_obj [] s in
       Ok (`O o, s)
     | `As ->
       let* a, s = parse_array [] s in
       Ok (`A a, s)
     | (`Bool _ | `Float _ | `String _ | `Null) as t -> Ok (t, s)
     | `Oe -> Error "Unexpected closing curly brace"
     | `Ae -> Error "Unexpected closing square braket"
     | `Name _ -> Error "Unexpected field name")

and parse_obj
  : (string * t) list -> lexeme Seq.t -> ((string * t) list * lexeme Seq.t, string) result
  =
 fun acc s ->
  match s () with
  | Seq.Nil -> Error "Unexpected end of lexeme"
  | Seq.Cons (`Oe, s) -> Ok (List.rev acc, s)
  | Seq.Cons (`Name name, s) ->
    let* t, s = parse s in
    parse_obj ((name, t) :: acc) s
  | Seq.Cons ((`Bool _ | `Os | `As | `Null | `Ae | `Float _ | `String _), _) ->
    Error "Unexpected lexeme, expected field name"

and parse_array : t list -> lexeme Seq.t -> (t list * lexeme Seq.t, string) result =
 fun acc s ->
  match s () with
  | Seq.Nil -> Error "Unexpected end of lexeme"
  | Seq.Cons (`Ae, s) -> Ok (List.rev acc, s)
  | Seq.Cons (lxm, s) ->
    let* t, s = parse (Seq.cons lxm s) in
    parse_array (t :: acc) s
;;

let parse_partial s = parse s

let parse s =
  let* t, s = parse s in
  let* () =
    match s () with
    | Seq.Nil -> Ok ()
    | Seq.Cons (_, _) -> Error "Extraneous lexemes after end of parsing"
  in
  Ok t
;;

(* TODO: optimise these conversions *)
let rec compatify : t -> compat = function
  | `O fs -> `O (List.map (fun (n, t) -> n, compatify t) fs)
  | `Oseq fs -> `O (List.of_seq (Seq.map (fun (n, t) -> n, compatify t) fs))
  | `Omap fs ->
    `O (List.of_seq (Seq.map (fun (n, t) -> n, compatify t) (FieldMap.to_seq fs)))
  | `A fs -> `A (List.map compatify fs)
  | `Aarray fs -> `A (List.map compatify (Array.to_list fs))
  | `Aseq fs -> `A (List.of_seq (Seq.map compatify fs))
  | (`Bool _ | `Float _ | `String _ | `Null) as t -> t
;;

let flexify t = (t : compat :> flex)
