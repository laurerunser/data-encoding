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

let rec append1 seq stop () =
  match seq () with
  | Seq.Nil -> Seq.Cons (stop, Seq.empty)
  | Seq.Cons (x, seq) -> Seq.Cons (x, append1 seq stop)
;;

let bracket start seq stop () = Seq.Cons (start, append1 seq stop)

let rec lexemify : t -> lexeme Seq.t = function
  | `O fs -> bracket `Os (lexemify_object fs) `Oe
  | `A ts -> bracket `As (lexemify_array ts) `Ae
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
    :  (string * t) list -> lexeme Seq.t
    -> ((string * t) Seq.t * lexeme Seq.t, string) result
  =
 fun acc s ->
  match s () with
  | Seq.Nil -> Error "Unexpected end of lexeme"
  | Seq.Cons (`Oe, s) -> Ok (List.to_seq (List.rev acc), s)
  | Seq.Cons (`Name name, s) ->
    let* t, s = parse s in
    parse_obj ((name, t) :: acc) s
  | Seq.Cons ((`Bool _ | `Os | `As | `Null | `Ae | `Float _ | `String _), _) ->
    Error "Unexpected lexeme, expected field name"

and parse_array : t list -> lexeme Seq.t -> (t Seq.t * lexeme Seq.t, string) result =
 fun acc s ->
  match s () with
  | Seq.Nil -> Error "Unexpected end of lexeme"
  | Seq.Cons (`Ae, s) -> Ok (List.to_seq (List.rev acc), s)
  | Seq.Cons (lxm, s) ->
    let* t, s = parse (Seq.cons lxm s) in
    parse_array (t :: acc) s
;;

let parse s =
  let* t, s = parse s in
  let* () =
    match s () with
    | Seq.Nil -> Ok ()
    | Seq.Cons (_, _) -> Error "Extraneous lexemes after end of parsing"
  in
  Ok t
;;
