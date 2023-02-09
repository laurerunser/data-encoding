(* TODO: string escaping *)

let shape fmt json =
  match json with
  | `O seq ->
    if Seq.is_empty seq
    then Format.pp_print_string fmt "{}"
    else Format.pp_print_string fmt "{…}"
  | `A seq ->
    if Seq.is_empty seq
    then Format.pp_print_string fmt "[]"
    else Format.pp_print_string fmt "[…]"
  | `Bool true -> Format.pp_print_string fmt "true"
  | `Bool false -> Format.pp_print_string fmt "false"
  | `Float f ->
    if Float.is_integer f
    then (
      let f = Int64.of_float f in
      Format.fprintf fmt "%Ld" f)
    else Format.pp_print_string fmt "<float>"
  | `String "" -> Format.pp_print_string fmt "\"\""
  | `String s ->
    if String.length s < 5
    then Format.fprintf fmt "%S" s
    else Format.pp_print_string fmt "<string>"
  | `Null -> Format.pp_print_string fmt "null"
;;

let%expect_test _ =
  Format.printf "%a\n" shape (`O Seq.empty);
  [%expect {| {} |}];
  Format.printf "%a\n" shape (`O (List.to_seq [ "this", `Null ]));
  [%expect {| {…} |}];
  Format.printf "%a\n" shape (`A Seq.empty);
  [%expect {| [] |}];
  Format.printf "%a\n" shape (`A (List.to_seq [ `Null; `A [] ]));
  [%expect {| […] |}];
  Format.printf "%a\n" shape (`Bool true);
  [%expect {| true |}];
  Format.printf "%a\n" shape (`Bool false);
  [%expect {| false |}];
  Format.printf "%a\n" shape (`Float 0.);
  [%expect {| 0 |}];
  Format.printf "%a\n" shape (`Float 0.1);
  [%expect {| <float> |}];
  Format.printf "%a\n" shape (`Float 1.);
  [%expect {| 1 |}];
  Format.printf "%a\n" shape (`Float Float.max_float);
  [%expect {| -9223372036854775808 |}];
  Format.printf "%a\n" shape (`Float Float.nan);
  [%expect {| <float> |}];
  Format.printf "%a\n" shape (`String "");
  [%expect {| "" |}];
  Format.printf "%a\n" shape (`String "this");
  [%expect {| "this" |}];
  Format.printf "%a\n" shape (`String "this-and-that-and-much-tooO0ooo-long");
  [%expect {| <string> |}];
  Format.printf "%a\n" shape `Null;
  [%expect {| null |}];
  ()
;;

let rec mini fmt json =
  match json with
  | `O fields ->
    Format.fprintf
      fmt
      "{%a}"
      (Format.pp_print_seq
         ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ',')
         mini_field)
      fields
  | `A elems ->
    Format.fprintf
      fmt
      "[%a]"
      (Format.pp_print_seq ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ',') mini)
      elems
  | `Bool true -> Format.pp_print_string fmt "true"
  | `Bool false -> Format.pp_print_string fmt "false"
  | `Float f -> Format.pp_print_float fmt f
  | `String s -> Format.fprintf fmt "%S" s
  | `Null -> Format.pp_print_string fmt "null"

and mini_field fmt (name, json) =
  Format.fprintf fmt "\"%s\":" name;
  Format.fprintf fmt "%a" mini json
;;

let%expect_test _ =
  Format.printf "%a\n" mini (`O Seq.empty);
  [%expect {| {} |}];
  Format.printf "%a\n" mini (`O (List.to_seq [ "this", `Null ]));
  [%expect {| {"this":null} |}];
  Format.printf "%a\n" mini (`A Seq.empty);
  [%expect {| [] |}];
  Format.printf "%a\n" mini (`A (List.to_seq [ `Null; `A Seq.empty ]));
  [%expect {| [null,[]] |}];
  Format.printf "%a\n" mini (`Bool true);
  [%expect {| true |}];
  Format.printf "%a\n" mini (`Bool false);
  [%expect {| false |}];
  Format.printf "%a\n" mini (`Float 0.);
  [%expect {| 0. |}];
  Format.printf "%a\n" mini (`Float 0.1);
  [%expect {| 0.1 |}];
  Format.printf "%a\n" mini (`Float 1.);
  [%expect {| 1. |}];
  Format.printf "%a\n" mini (`Float Float.max_float);
  [%expect {| 1.79769313486e+308 |}];
  Format.printf "%a\n" mini (`Float Float.nan);
  [%expect {| nan |}];
  Format.printf "%a\n" mini (`String "");
  [%expect {| "" |}];
  Format.printf "%a\n" mini (`String "this");
  [%expect {| "this" |}];
  Format.printf "%a\n" mini (`String "this-and-that-and-much-tooO0ooo-long");
  [%expect {| "this-and-that-and-much-tooO0ooo-long" |}];
  Format.printf "%a\n" mini `Null;
  [%expect {| null |}];
  ()
;;

let rec mini_lexemes depth first fmt (lxms : JSON.lexeme Seq.t) =
  match lxms () with
  | Seq.Nil ->
    if depth <> 0 then raise (Invalid_argument "unterminated lexeme sequence") else ()
  | Seq.Cons (lxm, lxms) ->
    (match lxm with
    | `Bool true ->
      if depth > 0 && not first then Format.pp_print_char fmt ',';
      Format.pp_print_string fmt "true";
      mini_lexemes depth false fmt lxms
    | `Bool false ->
      if depth > 0 && not first then Format.pp_print_char fmt ',';
      Format.pp_print_string fmt "false";
      mini_lexemes depth false fmt lxms
    | `Float f ->
      if depth > 0 && not first then Format.pp_print_char fmt ',';
      Format.pp_print_float fmt f;
      mini_lexemes depth false fmt lxms
    | `String s ->
      if depth > 0 && not first then Format.pp_print_char fmt ',';
      Format.fprintf fmt "%S" s;
      mini_lexemes depth false fmt lxms
    | `Null ->
      if depth > 0 && not first then Format.pp_print_char fmt ',';
      Format.pp_print_string fmt "null";
      mini_lexemes depth false fmt lxms
    | `As ->
      if depth > 0 && not first then Format.pp_print_char fmt ',';
      Format.pp_print_string fmt "[";
      mini_lexemes (depth + 1) true fmt lxms
    | `Ae ->
      Format.pp_print_string fmt "]";
      mini_lexemes (depth - 1) false fmt lxms
    | `Os ->
      if depth > 0 && not first then Format.pp_print_char fmt ',';
      Format.pp_print_string fmt "{";
      mini_lexemes (depth + 1) true fmt lxms
    | `Oe ->
      Format.pp_print_string fmt "}";
      mini_lexemes (depth - 1) false fmt lxms
    | `Name s ->
      if depth > 0 && not first then Format.pp_print_char fmt ',';
      Format.fprintf fmt "\"%s\":" s;
      mini_lexemes depth true fmt lxms)
;;

let mini_lexemes fmt s = mini_lexemes 0 false fmt s

let%expect_test _ =
  Format.printf "%a\n" mini_lexemes (JSON.lexemify @@ `O Seq.empty);
  [%expect {| {} |}];
  Format.printf "%a\n" mini_lexemes (JSON.lexemify @@ `O (List.to_seq [ "this", `Null ]));
  [%expect {| {"this":null} |}];
  Format.printf
    "%a\n"
    mini_lexemes
    (JSON.lexemify @@ `O (List.to_seq [ "this", `Null; "that", `Null ]));
  [%expect {| {"this":null,"that":null} |}];
  Format.printf "%a\n" mini_lexemes (JSON.lexemify @@ `A Seq.empty);
  [%expect {| [] |}];
  Format.printf
    "%a\n"
    mini_lexemes
    (JSON.lexemify @@ `A (List.to_seq [ `Null; `A Seq.empty ]));
  [%expect {| [null,[]] |}];
  Format.printf "%a\n" mini_lexemes (JSON.lexemify @@ `Bool true);
  [%expect {| true |}];
  Format.printf "%a\n" mini_lexemes (JSON.lexemify @@ `Bool false);
  [%expect {| false |}];
  Format.printf "%a\n" mini_lexemes (JSON.lexemify @@ `Float 0.);
  [%expect {| 0. |}];
  Format.printf "%a\n" mini_lexemes (JSON.lexemify @@ `Float 0.1);
  [%expect {| 0.1 |}];
  Format.printf "%a\n" mini_lexemes (JSON.lexemify @@ `Float 1.);
  [%expect {| 1. |}];
  Format.printf "%a\n" mini_lexemes (JSON.lexemify @@ `Float Float.max_float);
  [%expect {| 1.79769313486e+308 |}];
  Format.printf "%a\n" mini_lexemes (JSON.lexemify @@ `Float Float.nan);
  [%expect {| nan |}];
  Format.printf "%a\n" mini_lexemes (JSON.lexemify @@ `String "");
  [%expect {| "" |}];
  Format.printf "%a\n" mini_lexemes (JSON.lexemify @@ `String "this");
  [%expect {| "this" |}];
  Format.printf
    "%a\n"
    mini_lexemes
    (JSON.lexemify @@ `String "this-and-that-and-much-tooO0ooo-long");
  [%expect {| "this-and-that-and-much-tooO0ooo-long" |}];
  Format.printf "%a\n" mini_lexemes (JSON.lexemify @@ `Null);
  [%expect {| null |}];
  ()
;;
