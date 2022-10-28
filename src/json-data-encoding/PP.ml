let shape fmt json =
  match json with
  | `O [] -> Format.pp_print_string fmt "{}"
  | `O (_ :: _) -> Format.pp_print_string fmt "{…}"
  | `A [] -> Format.pp_print_string fmt "[]"
  | `A (_ :: _) -> Format.pp_print_string fmt "[…]"
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
  Format.printf "%a\n" shape (`O []);
  [%expect {| {} |}];
  Format.printf "%a\n" shape (`O [ "this", `Null ]);
  [%expect {| {…} |}];
  Format.printf "%a\n" shape (`A []);
  [%expect {| [] |}];
  Format.printf "%a\n" shape (`A [ `Null; `A [] ]);
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
