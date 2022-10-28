let construct : type a. a Encoding.t -> a -> (JSON.t, string) result =
 fun encoding v ->
  match encoding with
  | Unit ->
    assert (v = ());
    Ok (`O [])
  | Int64 -> Ok (`String (Int64.to_string v))
;;

let destruct : type a. a Encoding.t -> JSON.t -> (a, string) result =
 fun encoding json ->
  match encoding with
  | Unit ->
    (match json with
    | `O [] -> Ok ()
    | other -> Error (Format.asprintf "Expected {}, got %a" PP.shape other))
  | Int64 ->
    (match json with
    | `String s ->
      (match Int64.of_string_opt s with
      | Some i64 -> Ok i64
      | None -> Error (Format.asprintf "Expected int64 numeral, got %S" s))
    | other -> Error (Format.asprintf "Expected {}, got %a" PP.shape other))
;;
