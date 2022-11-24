let rec construct : type a. a Encoding.t -> a -> (JSON.t, string) result =
 fun encoding v ->
  match encoding with
  | Unit ->
    assert (v = ());
    Ok (`O [])
  | Int64 -> Ok (`String (Int64.to_string v))
  | Tuple t -> construct_tuple t v
  | Object o -> construct_obj o v

and construct_tuple : type a. a Encoding.tuple -> a -> (JSON.t, string) result =
 fun encoding v ->
  match encoding with
  | [] -> Ok (`A [])
  | t :: ts ->
    let (v :: vs) = v in
    (match construct t v with
    | Error _ as err -> err
    | Ok json ->
      (match construct_tuple ts vs with
      | Error _ as err -> err
      | Ok (`A jsons) -> Ok (`A (json :: jsons))
      | Ok _ -> assert false))

and construct_obj : type a. a Encoding.obj -> a -> (JSON.t, string) result =
 fun encoding v ->
  match encoding with
  | [] -> Ok (`O [])
  | (name, t) :: ts ->
    let (v :: vs) = v in
    (match construct t v with
    | Error _ as err -> err
    | Ok json ->
      (match construct_obj ts vs with
      | Error _ as err -> err
      | Ok (`O jsons) -> Ok (`O ((name, json) :: jsons))
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
  | Tuple t -> destruct_tuple t json
  | Object t -> destruct_obj t json

and destruct_tuple : type a. a Encoding.tuple -> JSON.t -> (a, string) result =
 fun encoding json ->
  match encoding with
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
        (match destruct_tuple ts (`A jsons) with
        | Error _ as err -> err
        | Ok vs -> Ok (v :: vs)))
    | other -> Error (Format.asprintf "Expected [..], got %a" PP.shape other))

and destruct_obj : type a. a Encoding.obj -> JSON.t -> (a, string) result =
 fun encoding json ->
  match encoding with
  | [] ->
    (match json with
    | `O [] -> Ok []
    | other ->
      (* TODO: control what to do with left-over fields *)
      Error (Format.asprintf "Expected {}, got %a" PP.shape other))
  | (name, t) :: ts ->
    (match json with
    | `O ((namename, json) :: jsons) ->
      if not (String.equal name namename)
      then
        (* TODO: support out-of-order fields *)
        Error (Format.asprintf "Expected field {%S:…}, got {%S:…}" name namename)
      else (
        match destruct t json with
        | Error _ as err -> err
        | Ok v ->
          (match destruct_obj ts (`O jsons) with
          | Error _ as err -> err
          | Ok vs -> Ok (v :: vs)))
    | other -> Error (Format.asprintf "Expected {..}, got %a" PP.shape other))
;;
