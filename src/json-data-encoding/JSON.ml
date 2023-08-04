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

let rec value_to_buffer buffer (lexemes : t) =
  match lexemes with
  | `Bool true -> Buffer.add_string buffer "true"
  | `Bool false -> Buffer.add_string buffer "false"
  | `Float f ->
    (* TODO: %f vs %g? roundtripping floats? *)
    Buffer.add_string buffer (Format.sprintf "%f" f)
  | `String s ->
    (* TODO: escaping *)
    (* TODO: check utf8 *)
    Buffer.add_string buffer (Format.sprintf "\"%s\"" s)
  | `Null -> Buffer.add_string buffer "null"
  | `O l ->
    Buffer.add_char buffer '{';
    obj_as_list_to_buffer buffer l
  | `Oseq seq ->
    Buffer.add_char buffer '{';
    obj_as_seq_to_buffer buffer seq
  | `Omap map ->
    Buffer.add_char buffer '{';
    obj_as_seq_to_buffer buffer (FieldMap.to_seq map)
  | `A l ->
    Buffer.add_char buffer '[';
    array_as_list_to_buffer buffer l
  | `Aarray a ->
    Buffer.add_char buffer '[';
    array_as_seq_to_buffer buffer (Array.to_seq a)
  | `Aseq flex ->
    Buffer.add_char buffer '[';
    array_as_seq_to_buffer buffer flex

and one_field_to_buffer buffer name value =
  Buffer.add_char buffer '"';
  Buffer.add_string buffer name;
  Buffer.add_char buffer '"';
  Buffer.add_char buffer ':';
  value_to_buffer buffer value

and obj_as_list_to_buffer buffer fields =
  match fields with
  | [] -> Buffer.add_char buffer '}'
  | [ (name, value) ] ->
    one_field_to_buffer buffer name value;
    Buffer.add_char buffer '}'
  | (name, value) :: xs ->
    one_field_to_buffer buffer name value;
    Buffer.add_char buffer ',';
    obj_as_list_to_buffer buffer xs

and obj_as_seq_to_buffer buffer fields =
  match Seq.uncons fields with
  | None -> Buffer.add_char buffer '}'
  | Some ((str, lexemes), xs) ->
    one_field_to_buffer buffer str lexemes;
    if Seq.is_empty xs
    then Buffer.add_char buffer '}'
    else (
      Buffer.add_char buffer ',';
      obj_as_seq_to_buffer buffer xs)

and array_as_list_to_buffer buffer values =
  match values with
  | [] -> Buffer.add_char buffer ']'
  | [ x ] ->
    value_to_buffer buffer x;
    Buffer.add_char buffer ']'
  | x :: xs ->
    value_to_buffer buffer x;
    Buffer.add_char buffer ',';
    array_as_list_to_buffer buffer xs

and array_as_seq_to_buffer buffer values =
  match Seq.uncons values with
  | None -> Buffer.add_char buffer ']'
  | Some (v, xs) ->
    value_to_buffer buffer v;
    if Seq.is_empty xs
    then Buffer.add_char buffer ']'
    else (
      Buffer.add_char buffer ',';
      array_as_seq_to_buffer buffer xs)
;;

let to_string lexemes =
  let b = Buffer.create 512 in
  value_to_buffer b lexemes;
  Buffer.contents b
;;

let%expect_test _ =
  let w l = print_string (to_string l) in
  w `Null;
  [%expect "null"];
  w (`Float 0.234);
  [%expect "0.234000"];
  w (`Bool true);
  [%expect "true"];
  w (`String "test");
  [%expect "\"test\""];
  w (`O [ "name1", `Null; "name2", `Bool false ]);
  [%expect "{\"name1\":null,\"name2\":false}"];
  w (`Oseq Seq.(cons ("name1", `Null) (cons ("name2", `String "test") empty)));
  [%expect "{\"name1\":null,\"name2\":\"test\"}"];
  let map = FieldMap.(add "name2" (`String "test") (add "name1" `Null empty)) in
  w (`Omap map);
  [%expect "{\"name1\":null,\"name2\":\"test\"}"];
  w
    (`A
      [ `Null; `String "test"; `Bool false; `O [ "name1", `Null; "name2", `Bool false ] ]);
  [%expect "[null,\"test\",false,{\"name1\":null,\"name2\":false}]"];
  let array = Array.make 2 (`Bool true) in
  w (`Aarray array);
  [%expect "[true,true]"];
  let array = Array.make 3 `Null in
  Array.set array 1 (`String "test");
  Array.set array 2 (`Float 3.21);
  w (`Aarray array);
  [%expect "[null,\"test\",3.210000]"];
  w (`Aseq Seq.(cons `Null (cons (`Bool true) empty)));
  [%expect "[null,true]"];
  w (`String {|"test" \u0068 \n \\|});
  [%expect {| ""test" \u0068 \n \\" |}];
  w (`String {| test \n test|});
  [%expect {| " test \n test" |}]
;;
