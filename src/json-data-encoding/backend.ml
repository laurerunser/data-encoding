let rec construct : type a. a Encoding.t -> a -> (JSON.t, string) result =
 fun encoding v ->
  match encoding with
  | Unit ->
    assert (v = ());
    Ok (`O [])
  | Int64 -> Ok (`String (Int64.to_string v))
  | [] -> Ok (`A [])
  | t :: ts ->
    let (v :: vs) = v in
    (match construct t v with
    | Error _ as err -> err
    | Ok json ->
      (match construct ts vs with
      | Error _ as err -> err
      | Ok (`A jsons) -> Ok (`A (json :: jsons))
      | Ok _ -> assert false))
;;

let rec destruct : type a. a Encoding.t -> JSON.t -> (a, string) result =
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
  | [] ->
    (match json with
    | `A [] -> Ok []
    | other -> Error (Format.asprintf "Expected [], got %a" PP.shape other))
  | t :: ts ->
    (match json with
    | `A (json :: jsons) ->
      (match destruct t json with
      | Error _ as err -> err
      | Ok v ->
        (match destruct ts (`A jsons) with
        | Error _ as err -> err
        | Ok vs -> Ok (v :: vs)))
    | other -> Error (Format.asprintf "Expected [..], got %a" PP.shape other))
;;
