let ( let* ) = Result.bind

let rec map_e f seq =
  match seq () with
  | Seq.Nil -> Ok Seq.empty
  | Seq.Cons (x, next) ->
    let* y = f x in
    let* next = map_e f next in
    Ok (Seq.cons y next)
;;

let error_unexpected_shape : type a. expected:string -> JSON.t -> (a, string) result =
 fun ~expected json ->
  Error (Format.asprintf "Expected %s, got %a" expected PP.shape json)
;;

let rec destruct : type a. a Encoding.t -> JSON.t -> (a, string) result =
 fun encoding json ->
  match encoding with
  | Unit ->
    (match json with
    | `O [] -> Ok ()
    | `Omap m when JSON.FieldMap.is_empty m -> Ok ()
    | `Oseq seq when Seq.is_empty seq -> Ok ()
    | other -> Error (Format.asprintf "Expected {}, got %a" PP.shape other))
  | Bool ->
    (match json with
    | `Bool b -> Ok b
    | other -> Error (Format.asprintf "Expected bool, got %a" PP.shape other))
  | Int64 ->
    (match json with
    | `String s ->
      (match Int64.of_string_opt s with
      | Some i64 -> Ok i64
      | None -> Error (Format.asprintf "Expected int64 numeral, got %S" s))
    | other -> Error (Format.asprintf "Expected {}, got %a" PP.shape other))
  | String ->
    (match json with
    | `String s -> Ok s
    | other -> Error (Format.asprintf "Expected \"…\", got %a" PP.shape other))
  | Seq t -> destruct_seq t json
  | Tuple t -> destruct_tuple t json
  | Object t -> destruct_obj t json
  | Conv { serialisation = _; deserialisation; encoding } ->
    (match destruct encoding json with
    | Error _ as err -> err
    | Ok w -> deserialisation w (* TODO: wrap error message *))

and destruct_seq : type a. a Encoding.t -> JSON.t -> (a Seq.t, string) result =
 fun t json ->
  match json with
  | `A seq -> map_e (destruct t) (List.to_seq seq)
  | `Aseq seq -> map_e (destruct t) seq
  | `Aarray seq -> map_e (destruct t) (Array.to_seq seq)
  | _ -> error_unexpected_shape ~expected:"[.]" json

and destruct_tuple : type a. a Encoding.tuple -> JSON.t -> (a, string) result =
 fun encoding json ->
  match json with
  | `A a -> destruct_tuple_seq encoding (List.to_seq a)
  | `Aseq a -> destruct_tuple_seq encoding a
  | `Aarray a -> destruct_tuple_seq encoding (Array.to_seq a)
  | other -> Error (Format.asprintf "Expected [..], got %a" PP.shape other)

and destruct_tuple_seq : type a. a Encoding.tuple -> JSON.t Seq.t -> (a, string) result =
 fun encoding jsons ->
  match encoding with
  | [] ->
    if Seq.is_empty jsons
    then Ok []
    else Error (Format.asprintf "Expected end-of-array, got additional entries")
  | t :: ts ->
    (match jsons () with
    | Seq.Cons (json, jsons) ->
      (match destruct t json with
      | Error _ as err -> err
      | Ok v ->
        (match destruct_tuple_seq ts jsons with
        | Error _ as err -> err
        | Ok vs -> Ok (v :: vs)))
    | Seq.Nil -> Error (Format.asprintf "Expected elements in array, got end of array"))

and destruct_obj : type a. a Encoding.obj -> JSON.t -> (a, string) result =
 fun encoding json ->
  match json with
  | `O o -> destruct_obj_map encoding (JSON.FieldMap.of_seq (List.to_seq o))
  | `Oseq o -> destruct_obj_map encoding (JSON.FieldMap.of_seq o)
  | `Omap o -> destruct_obj_map encoding o
  | other -> Error (Format.asprintf "Expected {..}, got %a" PP.shape other)

and destruct_obj_map
    : type a. a Encoding.obj -> JSON.t JSON.FieldMap.t -> (a, string) result
  =
 fun encoding fields ->
  match encoding with
  | [] ->
    if JSON.FieldMap.is_empty fields
    then Ok []
    else
      (* TODO: control what to do with left-over fields *)
      Error (Format.asprintf "Expected end-of-object, got additional fields")
  | Req { encoding = t; name } :: ts ->
    (match JSON.FieldMap.find_opt name fields with
    | None -> Error (Format.asprintf "Can't find expected field %S" name)
    | Some json ->
      let fields = JSON.FieldMap.remove name fields in
      (match destruct t json with
      | Error _ as err -> err
      | Ok v ->
        (match destruct_obj_map ts fields with
        | Error _ as err -> err
        | Ok vs -> Ok (v :: vs))))
  | Opt { encoding = t; name } :: ts ->
    (match JSON.FieldMap.find_opt name fields with
    | None ->
      (match destruct_obj_map ts fields with
      | Error _ as err -> err
      | Ok vs -> Ok (None :: vs))
    | Some json ->
      let fields = JSON.FieldMap.remove name fields in
      (match destruct t json with
      | Error _ as err -> err
      | Ok v ->
        (match destruct_obj_map ts fields with
        | Error _ as err -> err
        | Ok vs -> Ok (Some v :: vs))))
;;
