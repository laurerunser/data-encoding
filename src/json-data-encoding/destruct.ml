let ( let* ) = Result.bind

let rec map_e f seq =
  match seq () with
  | Seq.Nil -> Ok Seq.empty
  | Seq.Cons (x, next) ->
    let* y = f x in
    let* next = map_e f next in
    Ok (Seq.cons y next)
;;

let%expect_test _ =
  let w f s =
    match map_e f (List.to_seq s) with
    | Ok s ->
      Format.printf
        "Ok %a\n"
        (Format.pp_print_seq ~pp_sep:(fun _ () -> ()) Format.pp_print_int)
        s
    | Error s -> Format.printf "Error %s\n" s
  in
  w (fun i -> Ok i) [];
  [%expect {| Ok |}];
  w (fun i -> Ok i) [ 0 ];
  [%expect {| Ok 0 |}];
  w (fun i -> Ok i) [ 0; 1; 2 ];
  [%expect {| Ok 012 |}];
  w (fun i -> Error (string_of_int i)) [];
  [%expect {| Ok |}];
  w (fun i -> Error (string_of_int i)) [ 0 ];
  [%expect {| Error 0 |}];
  w (fun i -> Error (string_of_int i)) [ 0; 1; 2 ];
  [%expect {| Error 0 |}];
  w (fun i -> if i < 3 then Ok i else Error (string_of_int i)) [];
  [%expect {| Ok |}];
  w (fun i -> if i < 3 then Ok i else Error (string_of_int i)) [ 0 ];
  [%expect {| Ok 0 |}];
  w (fun i -> if i < 3 then Ok i else Error (string_of_int i)) [ 0; 1 ];
  [%expect {| Ok 01 |}];
  w (fun i -> if i < 3 then Ok i else Error (string_of_int i)) [ 0; 1; 2; 3; 4 ];
  [%expect {| Error 3 |}];
  ()
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

let%expect_test _ =
  let w e j pp =
    match destruct e j with
    | Ok v -> Format.printf "Ok %a\n" pp v
    | Error s -> Format.printf "Error %s\n" s
  in
  w Unit (`O []) (fun fmt () -> Format.fprintf fmt "()");
  [%expect {| Ok () |}];
  w Unit (`A []) (fun fmt () -> Format.fprintf fmt "()");
  [%expect {| Error Expected {}, got [] |}];
  w Unit `Null (fun fmt () -> Format.fprintf fmt "()");
  [%expect {| Error Expected {}, got null |}];
  w Unit (`O [ "foo", `Null ]) (fun fmt () -> Format.fprintf fmt "()");
  [%expect {| Error Expected {}, got {…} |}];
  w Bool (`Bool true) (fun fmt b -> Format.fprintf fmt "%b" b);
  [%expect {| Ok true |}];
  w Bool (`Bool false) (fun fmt b -> Format.fprintf fmt "%b" b);
  [%expect {| Ok false |}];
  w Bool (`String "") (fun fmt b -> Format.fprintf fmt "%b" b);
  [%expect {| Error Expected bool, got "" |}];
  w Bool (`String "crash") (fun fmt b -> Format.fprintf fmt "%b" b);
  [%expect {| Error Expected bool, got <string> |}];
  w Bool `Null (fun fmt b -> Format.fprintf fmt "%b" b);
  [%expect {| Error Expected bool, got null |}];
  w Int64 (`String "0") (fun fmt n -> Format.fprintf fmt "%Ld" n);
  [%expect {| Ok 0 |}];
  w Int64 (`String "1234567890") (fun fmt n -> Format.fprintf fmt "%Ld" n);
  [%expect {| Ok 1234567890 |}];
  w Int64 (`String "this") (fun fmt n -> Format.fprintf fmt "%Ld" n);
  [%expect {| Error Expected int64 numeral, got "this" |}];
  w Int64 `Null (fun fmt n -> Format.fprintf fmt "%Ld" n);
  [%expect {| Error Expected {}, got null |}];
  w String (`String "haha") (fun fmt s -> Format.fprintf fmt "%s" s);
  [%expect {| Ok haha |}];
  w String `Null (fun fmt s -> Format.fprintf fmt "%s" s);
  [%expect {| Error Expected "…", got null |}];
  let print_int_seq fmt s =
    Format.fprintf
      fmt
      "%a"
      (Format.pp_print_seq
         ~pp_sep:(fun _ () -> ())
         (fun fmt n -> Format.fprintf fmt "%Ld" n))
      s
  in
  w (Seq Int64) (`A []) print_int_seq;
  [%expect {| Ok |}];
  w (Seq Int64) (`A [ `String "0" ]) print_int_seq;
  [%expect {| Ok 0 |}];
  w (Seq Int64) (`A [ `String "0"; `String "1" ]) print_int_seq;
  [%expect {| Ok 01 |}];
  w (Seq Int64) (`A [ `Null ]) print_int_seq;
  [%expect {| Error Expected {}, got null |}];
  w (Seq Int64) (`A [ `String "2"; `Null ]) print_int_seq;
  [%expect {| Error Expected {}, got null |}];
  ()
;;

let consume_1 lxms lxm =
  match lxms () with
  | Seq.Nil -> Error "Unepxected end of lexeme sequence"
  | Seq.Cons (l, lxms) ->
    if l = lxm then Ok lxms else Error "Unexpected lexeme in sequence"
;;

let consume_q lxms f =
  match lxms () with
  | Seq.Nil -> Error "Unepxected end of lexeme sequence"
  | Seq.Cons (lxm, lxms) ->
    (match f lxm with
    | Some v -> Ok (v, lxms)
    | None -> Error "Unexpected lexeme in sequence")
;;

let rec destruct_lexemes
    : type a. a Encoding.t -> JSON.lexemes -> (a * JSON.lexemes, string) result
  =
 fun encoding lxms ->
  match encoding with
  | Unit ->
    let* lxms = consume_1 lxms `Os in
    let* lxms = consume_1 lxms `Oe in
    Ok ((), lxms)
  | Bool ->
    consume_q lxms (function
        | `Bool b -> Some b
        | `Float _ | `String _ | `Null | `Name _ | `As | `Ae | `Os | `Oe -> None)
  | Int64 ->
    consume_q lxms (function
        | `String s -> Int64.of_string_opt s
        | `Bool _ | `Float _ | `Null | `Name _ | `As | `Ae | `Os | `Oe -> None)
  | String ->
    consume_q lxms (function
        | `String s -> Some s
        | `Bool _ | `Float _ | `Null | `Name _ | `As | `Ae | `Os | `Oe -> None)
  | Seq e ->
    let* lxms = consume_1 lxms `As in
    destruct_lexemes_seq e lxms []
  | Tuple t ->
    let* lxms = consume_1 lxms `As in
    destruct_lexemes_tuple t lxms
  | Object o ->
    let* lxms = consume_1 lxms `Os in
    destruct_lexemes_obj o lxms
  | Conv { serialisation = _; deserialisation; encoding } ->
    let* w, lxms = destruct_lexemes encoding lxms in
    let* w = deserialisation w in
    Ok (w, lxms)

and destruct_lexemes_seq
    : type a.
      a Encoding.t -> JSON.lexemes -> a list -> (a Seq.t * JSON.lexemes, string) result
  =
 fun encoding lxms acc ->
  match lxms () with
  | Seq.Nil -> Error "Unexpected end of lexeme sequence"
  | Seq.Cons (`Ae, lxms) -> Ok (List.to_seq (List.rev acc), lxms)
  | Seq.Cons (lxm, lxms) ->
    let* elt, lxms = destruct_lexemes encoding (Seq.cons lxm lxms) in
    destruct_lexemes_seq encoding lxms (elt :: acc)

and destruct_lexemes_tuple
    : type a. a Encoding.tuple -> JSON.lexemes -> (a * JSON.lexemes, string) result
  =
 fun encoding lxms ->
  match encoding with
  | [] ->
    let* lxms = consume_1 lxms `Ae in
    Ok (Commons.Hlist.[], lxms)
  | t :: ts ->
    let* v, lxms = destruct_lexemes t lxms in
    let* vs, lxms = destruct_lexemes_tuple ts lxms in
    Ok (Commons.Hlist.( :: ) (v, vs), lxms)

and destruct_lexemes_obj_reached_the_end
    : type a. a Encoding.obj -> JSON.t JSON.FieldMap.t -> (a, string) result
  =
 fun encoding fields ->
  match encoding with
  | [] ->
    (* TODO: allow control over dealing with extra fields *)
    if JSON.FieldMap.is_empty fields
    then Ok Commons.Hlist.[]
    else Error "Extraneous fields in object"
  | Req { encoding = t; name } :: ts ->
    (match JSON.FieldMap.find_opt name fields with
    | None -> Error "Cannot find field"
    | Some json ->
      let* v = destruct t json in
      let fields = JSON.FieldMap.remove name fields in
      let* vs = destruct_lexemes_obj_reached_the_end ts fields in
      Ok (Commons.Hlist.( :: ) (v, vs)))
  | Opt { encoding = t; name } :: ts ->
    (match JSON.FieldMap.find_opt name fields with
    | None ->
      let* vs = destruct_lexemes_obj_reached_the_end ts fields in
      Ok (Commons.Hlist.( :: ) (None, vs))
    | Some json ->
      let* v = destruct t json in
      let fields = JSON.FieldMap.remove name fields in
      let* vs = destruct_lexemes_obj_reached_the_end ts fields in
      Ok (Commons.Hlist.( :: ) (Some v, vs)))

and destruct_lexemes_fields fields lxms =
  match lxms () with
  | Seq.Nil -> Error "Unexpected end of lexeme sequence"
  | Seq.Cons (`Name name, lxms) ->
    if JSON.FieldMap.find_opt name fields = None
    then
      let* field, lxms = JSON.parse_partial lxms in
      let fields = JSON.FieldMap.add name field fields in
      destruct_lexemes_fields fields lxms
    else Error "Duplicate field name in object"
  | Seq.Cons (`Oe, lxms) -> Ok (fields, lxms)
  | Seq.Cons (_, _) -> Error "Unexpected lexeme in object"

(* TODO: study the perf of different obj destruction strategies and pick the
   best:
     - lazily traverse obj lexemes building the field map progressively but
     consuming it progressively too OR (current impl) traverse the whole obj in
     one pass
     - (current impl) destruct each field to JSON.t value OR destruct each field
     as a sequence of lexemes
*)
and destruct_lexemes_obj
    : type a. a Encoding.obj -> JSON.lexemes -> (a * JSON.lexemes, string) result
  =
 fun encoding lxms ->
  let* fields, lxms = destruct_lexemes_fields JSON.FieldMap.empty lxms in
  let* o = destruct_lexemes_obj_reached_the_end encoding fields in
  Ok (o, lxms)
;;

let destruct_lexemes encoding lxms =
  let* v, lxms = destruct_lexemes encoding lxms in
  if Seq.is_empty lxms then Ok v else Error "Too many lexemes"
;;

let%expect_test _ =
  let w e j pp =
    match destruct_lexemes e (List.to_seq j) with
    | Ok v -> Format.printf "Ok %a\n" pp v
    | Error s -> Format.printf "Error %s\n" s
  in
  w Unit [ `Os; `Oe ] (fun fmt () -> Format.fprintf fmt "()");
  [%expect {| Ok () |}];
  w Unit [] (fun fmt () -> Format.fprintf fmt "()");
  [%expect {| Error Unepxected end of lexeme sequence |}];
  w Unit [ `Oe; `Os ] (fun fmt () -> Format.fprintf fmt "()");
  [%expect {| Error Unexpected lexeme in sequence |}];
  w Unit [ `Os; `Oe; `Os; `Oe ] (fun fmt () -> Format.fprintf fmt "()");
  [%expect {| Error Too many lexemes |}];
  w Unit [ `Os ] (fun fmt () -> Format.fprintf fmt "()");
  [%expect {| Error Unepxected end of lexeme sequence |}];
  w Unit [ `As; `Ae ] (fun fmt () -> Format.fprintf fmt "()");
  [%expect {| Error Unexpected lexeme in sequence |}];
  w Unit [ `Ae; `As ] (fun fmt () -> Format.fprintf fmt "()");
  [%expect {| Error Unexpected lexeme in sequence |}];
  w Unit [ `Null ] (fun fmt () -> Format.fprintf fmt "()");
  [%expect {| Error Unexpected lexeme in sequence |}];
  w Unit [ `Os; `Name "foo"; `Null; `Oe ] (fun fmt () -> Format.fprintf fmt "()");
  [%expect {| Error Unexpected lexeme in sequence |}];
  w Bool [ `Bool true ] (fun fmt b -> Format.fprintf fmt "%b" b);
  [%expect {| Ok true |}];
  w Bool [ `Bool false ] (fun fmt b -> Format.fprintf fmt "%b" b);
  [%expect {| Ok false |}];
  w Bool [ `String "" ] (fun fmt b -> Format.fprintf fmt "%b" b);
  [%expect {| Error Unexpected lexeme in sequence |}];
  w Bool [ `String "crash" ] (fun fmt b -> Format.fprintf fmt "%b" b);
  [%expect {| Error Unexpected lexeme in sequence |}];
  w Bool [ `Null ] (fun fmt b -> Format.fprintf fmt "%b" b);
  [%expect {| Error Unexpected lexeme in sequence |}];
  w Int64 [ `String "0" ] (fun fmt n -> Format.fprintf fmt "%Ld" n);
  [%expect {| Ok 0 |}];
  w Int64 [ `String "1234567890" ] (fun fmt n -> Format.fprintf fmt "%Ld" n);
  [%expect {| Ok 1234567890 |}];
  w Int64 [ `String "this" ] (fun fmt n -> Format.fprintf fmt "%Ld" n);
  [%expect {| Error Unexpected lexeme in sequence |}];
  w Int64 [ `Null ] (fun fmt n -> Format.fprintf fmt "%Ld" n);
  [%expect {| Error Unexpected lexeme in sequence |}];
  w String [ `String "haha" ] (fun fmt s -> Format.fprintf fmt "%s" s);
  [%expect {| Ok haha |}];
  w String [ `Null ] (fun fmt s -> Format.fprintf fmt "%s" s);
  [%expect {| Error Unexpected lexeme in sequence |}];
  let print_int_seq fmt s =
    Format.fprintf
      fmt
      "%a"
      (Format.pp_print_seq
         ~pp_sep:(fun _ () -> ())
         (fun fmt n -> Format.fprintf fmt "%Ld" n))
      s
  in
  w (Seq Int64) [ `As; `Ae ] print_int_seq;
  [%expect {| Ok |}];
  w (Seq Int64) [ `As; `String "0"; `Ae ] print_int_seq;
  [%expect {| Ok 0 |}];
  w (Seq Int64) [ `As; `String "0"; `String "1"; `Ae ] print_int_seq;
  [%expect {| Ok 01 |}];
  w (Seq Int64) [ `As; `Null; `Ae ] print_int_seq;
  [%expect {| Error Unexpected lexeme in sequence |}];
  w (Seq Int64) [ `As; `String "2"; `Null; `Ae ] print_int_seq;
  [%expect {| Error Unexpected lexeme in sequence |}];
  ()
;;
