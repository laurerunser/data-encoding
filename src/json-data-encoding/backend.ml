let rec construct : type a. a Encoding.t -> a -> (JSON.t, string) result =
 fun encoding v ->
  match encoding with
  | Unit ->
    assert (v = ());
    Ok (`O [])
  | Int64 -> Ok (`String (Int64.to_string v))
  | String -> Ok (`String v) (* TODO check utf8 *)
  | Tuple t -> construct_tuple t v
  | Object o -> construct_obj o v
  | Conv { serialisation; deserialisation = _; encoding } ->
    construct encoding (serialisation v)
(* TODO exception handling?? *)

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
  | Req { encoding = t; name } :: ts ->
    let (v :: vs) = v in
    (match construct t v with
    | Error _ as err -> err
    | Ok json ->
      (match construct_obj ts vs with
      | Error _ as err -> err
      | Ok (`O jsons) -> Ok (`O ((name, json) :: jsons))
      | Ok _ -> assert false))
  | Opt { encoding = t; name } :: ts ->
    let (v :: vs) = v in
    (match v with
    | None -> construct_obj ts vs
    | Some v ->
      (match construct t v with
      | Error _ as err -> err
      | Ok json ->
        (match construct_obj ts vs with
        | Error _ as err -> err
        | Ok (`O jsons) -> Ok (`O ((name, json) :: jsons))
        | Ok _ -> assert false)))
;;

let%expect_test _ =
  let w : type a. a Encoding.t -> a -> unit =
   fun e v ->
    match construct e v with
    | Ok j -> Format.printf "%a\n" PP.mini j
    | Error s -> Format.printf "Error: %s\n" s
  in
  w Encoding.unit ();
  [%expect {| {} |}];
  w Encoding.int64 0x4c_6f_6f_6f_6f_6f_6f_4cL;
  [%expect {| "5507743393699032908" |}];
  w Encoding.int64 0xff_ff_ff_ff_ff_ff_ff_ffL;
  [%expect {| "-1" |}];
  w
    Encoding.(tuple [ unit; unit; int64; unit; int64 ])
    [ (); (); 0x4c_6f_6f_4cL; (); 0x4c_6f_6f_4cL ];
  [%expect {| [{},{},"1282371404",{},"1282371404"] |}];
  w Encoding.(tuple [ string; unit ]) [ "FOO"; () ];
  [%expect {| ["FOO",{}] |}];
  w Encoding.(obj [ req "foo" string; req "bar" unit ]) [ "FOO"; () ];
  [%expect {| {"foo":"FOO","bar":{}} |}];
  w Encoding.(obj [ req "foo" string; opt "bar" unit ]) [ "FOO"; None ];
  [%expect {| {"foo":"FOO"} |}];
  let module R = struct
    type t =
      { foo : string
      ; bar : int64
      }
  end
  in
  w
    Encoding.(
      Record.(
        record
          (fun foo bar -> R.{ foo; bar })
          [ field "foo" (fun r -> r.R.foo) string; field "bar" (fun r -> r.R.bar) int64 ]))
    R.{ foo = "FOO"; bar = 0xff_ff_ff_ff_ff_ff_ff_ffL };
  [%expect {| {"foo":"FOO","bar":"-1"} |}];
  ()
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
  | String ->
    (match json with
    | `String s -> Ok s
    | other -> Error (Format.asprintf "Expected \"…\", got %a" PP.shape other))
  | Tuple t -> destruct_tuple t json
  | Object t -> destruct_obj t json
  | Conv { serialisation = _; deserialisation; encoding } ->
    (match destruct encoding json with
    | Error _ as err -> err
    | Ok w -> deserialisation w (* TODO: wrap error message *))

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
  | Req { encoding = t; name } :: ts ->
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
    | `O [] -> Error (Format.asprintf "Expected field {%S:…}, got {}" name)
    | other -> Error (Format.asprintf "Expected {..}, got %a" PP.shape other))
  | Opt { encoding = t; name } :: ts ->
    (match json with
    | `O [] ->
      (match destruct_obj ts (`O []) with
      | Error _ as err -> err
      | Ok vs -> Ok (None :: vs))
    | `O ((namename, json) :: jsons) ->
      if not (String.equal name namename)
      then (
        (* TODO: support out-of-order fields *)
        match destruct_obj ts (`O jsons) with
        | Error _ as err -> err
        | Ok vs -> Ok (None :: vs))
      else (
        match destruct t json with
        | Error _ as err -> err
        | Ok v ->
          (match destruct_obj ts (`O jsons) with
          | Error _ as err -> err
          | Ok vs -> Ok (Some v :: vs)))
    | other -> Error (Format.asprintf "Expected {..}, got %a" PP.shape other))
;;
