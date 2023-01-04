let ( let* ) = Result.bind

let rec map_e f seq =
  match Seq.uncons seq with
  | None -> Ok Seq.empty
  | Some (x, next) ->
    let* y = f x in
    let* next = map_e f next in
    Ok (Seq.cons y next)
;;

let rec construct : type a. a Encoding.t -> a -> (JSON.t, string) result =
 fun encoding v ->
  match encoding with
  | Unit ->
    assert (v = ());
    Ok (`O Seq.empty)
  | Bool -> Ok (`Bool v)
  | Int64 -> Ok (`String (Int64.to_string v))
  | String -> Ok (`String v) (* TODO check utf8 *)
  | Seq t ->
    let* seq = map_e (construct t) v in
    Ok (`A seq)
  | Tuple t -> construct_tuple t v
  | Object o -> construct_obj o v
  | Conv { serialisation; deserialisation = _; encoding } ->
    construct encoding (serialisation v)
(* TODO exception handling?? *)

and construct_tuple : type a. a Encoding.tuple -> a -> (JSON.t, string) result =
 fun encoding v ->
  match encoding with
  | [] -> Ok (`A Seq.empty)
  | t :: ts ->
    let (v :: vs) = v in
    (match construct t v with
    | Error _ as err -> err
    | Ok json ->
      (match construct_tuple ts vs with
      | Error _ as err -> err
      | Ok (`A jsons) -> Ok (`A (Seq.cons json jsons))
      | Ok _ -> assert false))

and construct_obj : type a. a Encoding.obj -> a -> (JSON.t, string) result =
 fun encoding v ->
  match encoding with
  | [] -> Ok (`O Seq.empty)
  | Req { encoding = t; name } :: ts ->
    let (v :: vs) = v in
    (match construct t v with
    | Error _ as err -> err
    | Ok json ->
      (match construct_obj ts vs with
      | Error _ as err -> err
      | Ok (`O jsons) -> Ok (`O (Seq.cons (name, json) jsons))
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
        | Ok (`O jsons) -> Ok (`O (Seq.cons (name, json) jsons))
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
  w Encoding.(seq string) (List.to_seq [ "a"; "bc"; "Foo"; "BAR" ]);
  [%expect {| ["a","bc","Foo","BAR"] |}];
  w Encoding.(list int64) [ 0x4c_6f_6f_4cL; 0xff_ff_ff_ff_ff_ff_ff_ffL; 0x4c_6f_6f_4cL ];
  [%expect {| ["1282371404","-1","1282371404"] |}];
  w Encoding.(array int64) [||];
  [%expect {| [] |}];
  w Encoding.(array unit) [| () |];
  [%expect {| [{}] |}];
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

let error_unexpected_shape : type a. expected:string -> JSON.t -> (a, string) result =
 fun ~expected json ->
  Error (Format.asprintf "Expected %s, got %a" expected PP.shape json)
;;

let rec destruct : type a. a Encoding.t -> JSON.t -> (a, string) result =
 fun encoding json ->
  match encoding with
  | Unit ->
    (match json with
    | `O seq when Seq.is_empty seq -> Ok ()
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
  | `A seq -> map_e (destruct t) seq
  | _ -> error_unexpected_shape ~expected:"[.]" json

and destruct_tuple : type a. a Encoding.tuple -> JSON.t -> (a, string) result =
 fun encoding json ->
  match encoding with
  | [] ->
    (match json with
    | `A seq when Seq.is_empty seq -> Ok []
    | other -> Error (Format.asprintf "Expected [], got %a" PP.shape other))
  | t :: ts ->
    (match json with
    | `A seq ->
      (match Seq.uncons seq with
      | Some (json, jsons) ->
        (match destruct t json with
        | Error _ as err -> err
        | Ok v ->
          (match destruct_tuple ts (`A jsons) with
          | Error _ as err -> err
          | Ok vs -> Ok (v :: vs)))
      | None -> Error (Format.asprintf "Expected [..], got %a" PP.shape json))
    | other -> Error (Format.asprintf "Expected [..], got %a" PP.shape other))

and destruct_obj : type a. a Encoding.obj -> JSON.t -> (a, string) result =
 fun encoding json ->
  match encoding with
  | [] ->
    (match json with
    | `O seq when Seq.is_empty seq -> Ok []
    | other ->
      (* TODO: control what to do with left-over fields *)
      Error (Format.asprintf "Expected {}, got %a" PP.shape other))
  | Req { encoding = t; name } :: ts ->
    (match json with
    | `O seq ->
      (match Seq.uncons seq with
      | Some ((namename, json), jsons) ->
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
      | None -> Error (Format.asprintf "Expected field {%S:…}, got {}" name))
    | other -> Error (Format.asprintf "Expected {..}, got %a" PP.shape other))
  | Opt { encoding = t; name } :: ts ->
    (match json with
    | `O seq ->
      (match Seq.uncons seq with
      | None ->
        (match destruct_obj ts (`O Seq.empty) with
        | Error _ as err -> err
        | Ok vs -> Ok (None :: vs))
      | Some ((namename, json), jsons) ->
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
            | Ok vs -> Ok (Some v :: vs))))
    | other -> Error (Format.asprintf "Expected {..}, got %a" PP.shape other))
;;
