[@@@landmark "auto"]

let ( let* ) = Result.bind

let rec map_e f seq =
  match seq () with
  | Seq.Nil -> Ok Seq.empty
  | Seq.Cons (x, next) ->
    let* y = f x in
    let* next = map_e f next in
    Ok (Seq.cons y next)
;;

let rec construct : type a. a Encoding.t -> a -> (JSON.t, string) result =
 fun encoding v ->
  match encoding with
  | Unit -> Ok (`O [])
  | Null -> Ok `Null
  | Bool -> Ok (`Bool v)
  | Int64 -> Ok (`String (Int64.to_string v))
  | String -> Ok (`String v) (* TODO check utf8 *)
  | Seq t ->
    let* seq = map_e (construct t) v in
    Ok (`Aseq seq)
  | Tuple t -> construct_tuple t v
  | Object { field_hlist; fieldname_key_map = _; field_hmap = _ } ->
    construct_obj field_hlist v
  | Conv { serialisation; deserialisation = _; encoding } ->
    (* TODO exception handling?? *)
    construct encoding (serialisation v)
  | Union { cases = _; serialisation; deserialisation = _ } ->
    let (AnyP ({ tag; encoding; inject = _ }, p)) = serialisation v in
    (match encoding with
     | Unit ->
       (* special case: cases without a payload are represented as strings *)
       Ok (`String tag)
     | _ ->
       let* jsonp = construct encoding p in
       Ok (`O [ tag, jsonp ]))

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
        | Ok (`Aseq jsons) -> Ok (`Aseq (Seq.cons json jsons))
        | Ok (`A []) -> Ok (`Aseq (Seq.return json))
        | Ok _ -> assert false))

and construct_obj : type a. a Encoding.obj -> a -> (JSON.t, string) result =
 fun encoding v ->
  match encoding with
  | [] -> Ok (`O [])
  | Req { encoding = t; name; vkey = _; fkey = _ } :: ts ->
    let (v :: vs) = v in
    (match construct t v with
     | Error _ as err -> err
     | Ok json ->
       (match construct_obj ts vs with
        | Error _ as err -> err
        | Ok (`Oseq jsons) -> Ok (`Oseq (Seq.cons (name, json) jsons))
        | Ok (`O []) -> Ok (`Oseq (Seq.return (name, json)))
        | Ok _ -> assert false))
  | Opt { encoding = t; name; vkey = _; fkey = _ } :: ts ->
    let (v :: vs) = v in
    (match v with
     | None -> construct_obj ts vs
     | Some v ->
       (match construct t v with
        | Error _ as err -> err
        | Ok json ->
          (match construct_obj ts vs with
           | Error _ as err -> err
           | Ok (`Oseq jsons) -> Ok (`Oseq (Seq.cons (name, json) jsons))
           | Ok (`O []) -> Ok (`Oseq (Seq.return (name, json)))
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

let empty_obj : JSON.lexeme Seq.node = Seq.Cons (`Os, fun () -> Seq.Cons (`Oe, Seq.empty))
let null : JSON.lexeme Seq.node = Seq.Cons (`Null, Seq.empty)

let rec construct_lexemes : type a. a Encoding.t -> a -> JSON.lexeme Seq.t =
 fun encoding v () ->
  match encoding with
  | Unit -> empty_obj
  | Null -> null
  | Bool -> Seq.Cons (`Bool v, Seq.empty)
  | Int64 -> Seq.Cons (`String (Int64.to_string v), Seq.empty)
  | String -> Seq.Cons (`String v (* TODO check utf8 *), Seq.empty)
  | Seq t -> Commons.Sequtils.bracket `As (Seq.flat_map (construct_lexemes t) v) `Ae ()
  | Tuple t -> Commons.Sequtils.bracket `As (construct_tuple_lexemes t v) `Ae ()
  | Object { field_hlist; fieldname_key_map = _; field_hmap = _ } ->
    Commons.Sequtils.bracket `Os (construct_obj_lexemes field_hlist v) `Oe ()
  | Conv { serialisation; deserialisation = _; encoding } ->
    (* TODO exception handling?? *)
    construct_lexemes encoding (serialisation v) ()
  | Union { cases = _; serialisation; deserialisation = _ } ->
    let (AnyP ({ tag; encoding; inject = _ }, p)) = serialisation v in
    (match encoding with
     | Unit ->
       (* special case: cases without a payload are represented as strings *)
       Seq.Cons (`String tag, Seq.empty)
     | _ ->
       Commons.Sequtils.bracket
         `Os
         (Seq.cons (`Name tag) (construct_lexemes encoding p))
         `Oe
         ())

and construct_tuple_lexemes : type a. a Encoding.tuple -> a -> JSON.lexeme Seq.t =
 fun encoding v () ->
  match encoding with
  | [] -> Seq.Nil
  | t :: ts ->
    let (v :: vs) = v in
    Seq.append (construct_lexemes t v) (construct_tuple_lexemes ts vs) ()

and construct_obj_lexemes : type a. a Encoding.obj -> a -> JSON.lexeme Seq.t =
 fun encoding v () ->
  match encoding with
  | [] -> Seq.Nil
  | Req { encoding = t; name; vkey = _; fkey = _ } :: ts ->
    let (v :: vs) = v in
    Seq.append
      (Seq.cons (`Name name) (construct_lexemes t v))
      (construct_obj_lexemes ts vs)
      ()
  | Opt { encoding = t; name; vkey = _; fkey = _ } :: ts ->
    let (v :: vs) = v in
    (match v with
     | None -> construct_obj_lexemes ts vs ()
     | Some v ->
       Seq.append
         (Seq.cons (`Name name) (construct_lexemes t v))
         (construct_obj_lexemes ts vs)
         ())
;;

let%expect_test _ =
  let w : type a. a Encoding.t -> a -> unit =
   fun e v ->
    match construct_lexemes e v with
    | j -> Format.printf "%a\n" PP.mini_lexemes j
    | exception exc -> Format.printf "Error: %s\n" (Printexc.to_string exc)
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

let ( let* ) = Buffy.W.( let* )

(* TODO: pbt tests *)
let rec write_lexemes depth first state (lxms : JSON.lexeme Seq.t) =
  match lxms () with
  | exception exc ->
    let error = "Error whilst serialising: " ^ Printexc.to_string exc in
    Buffy.W.Failed { state; error }
  | Seq.Nil -> Buffy.W.Written { state }
  | Seq.Cons (lxm, lxms) ->
    (match lxm with
     | `Bool true ->
       let* state =
         if depth > 0 && not first
         then Buffy.W.write_string state ",true"
         else Buffy.W.write_string state "true"
       in
       write_lexemes depth false state lxms
     | `Bool false ->
       let* state =
         if depth > 0 && not first
         then Buffy.W.write_string state ",false"
         else Buffy.W.write_string state "false"
       in
       write_lexemes depth false state lxms
     | `Float f ->
       (* TODO: more tractable representation of float *)
       let literal = Float.to_string f in
       let literal = if depth > 0 && not first then "," ^ literal else literal in
       let* state = Buffy.W.write_string state literal in
       write_lexemes depth false state lxms
     | `String s ->
       (* TODO: check for UTF8 validity *)
       let* state =
         if depth > 0 && not first
         then Buffy.W.write_string state ",\""
         else Buffy.W.write_char state '"'
       in
       let* state = Buffy.W.write_string state s in
       let* state = Buffy.W.write_char state '"' in
       write_lexemes depth false state lxms
     | `Null ->
       let* state =
         if depth > 0 && not first
         then Buffy.W.write_string state ",null"
         else Buffy.W.write_string state "null"
       in
       write_lexemes depth false state lxms
     | `As ->
       let* state =
         if depth > 0 && not first
         then Buffy.W.write_string state ",["
         else Buffy.W.write_char state '['
       in
       write_lexemes (depth + 1) true state lxms
     | `Ae ->
       let* state = Buffy.W.write_char state ']' in
       write_lexemes (depth - 1) false state lxms
     | `Os ->
       let* state =
         if depth > 0 && not first
         then Buffy.W.write_string state ",{"
         else Buffy.W.write_char state '{'
       in
       write_lexemes (depth + 1) true state lxms
     | `Oe ->
       let* state = Buffy.W.write_char state '}' in
       write_lexemes (depth - 1) false state lxms
     | `Name s ->
       let* state =
         if depth > 0 && not first
         then Buffy.W.write_string state ",\""
         else Buffy.W.write_char state '"'
       in
       let* state = Buffy.W.write_string state s in
       let* state = Buffy.W.write_string state "\":" in
       write_lexemes (depth + 1) true state lxms)
;;

let write_lexemes state lxms = write_lexemes 0 true state lxms

let%expect_test _ =
  let scratch = String.make 20 ' ' in
  let w : JSON.lexeme Seq.t -> unit =
   fun lxms ->
    let scratch = Bytes.of_string scratch in
    let destination = Buffy.Dst.of_bytes scratch ~offset:1 ~length:18 in
    let state = Buffy.W.mk_state destination in
    match write_lexemes state lxms with
    | Buffy.W.Written _ -> Format.printf "Ok: %s\n" (Bytes.unsafe_to_string scratch)
    | Buffy.W.Failed { state = _; error } ->
      Format.printf "Error: %s (%S)" error (Bytes.unsafe_to_string scratch)
    | Buffy.W.Suspended _ -> assert false
   (* not possible in these small tests *)
  in
  w Seq.empty;
  [%expect {| Ok: |}];
  w (JSON.lexemify (`Oseq Seq.empty));
  [%expect {| Ok:  {} |}];
  w (JSON.lexemify (`Oseq (List.to_seq [ "x", `Null ])));
  [%expect {| Ok:  {"x":null} |}];
  w (JSON.lexemify (`O [ "x", `Null ]));
  [%expect {| Ok:  {"x":null} |}];
  w (JSON.lexemify (`Oseq (List.to_seq [ "x", `String "x"; "y", `String "y" ])));
  [%expect {| Ok:  {"x":"x","y":"y"} |}];
  w (JSON.lexemify (`O [ "x", `String "x"; "y", `String "y" ]));
  [%expect {| Ok:  {"x":"x","y":"y"} |}];
  w (JSON.lexemify (`Aseq Seq.empty));
  [%expect {| Ok:  [] |}];
  w (JSON.lexemify (`Aseq (List.to_seq [ `Null ])));
  [%expect {| Ok:  [null] |}];
  w (JSON.lexemify (`A [ `Null ]));
  [%expect {| Ok:  [null] |}];
  w (JSON.lexemify (`A [ `Aseq Seq.empty ]));
  [%expect {| Ok:  [[]] |}];
  w (JSON.lexemify (`A [ `O [ "x", `A [] ] ]));
  [%expect {| Ok:  [{"x":[]}] |}];
  w (JSON.lexemify (`A [ `O []; `Aseq Seq.empty; `Oseq Seq.empty ]));
  [%expect {| Ok:  [{},[],{}] |}];
  w (JSON.lexemify (`Aarray [| `O []; `A []; `Oseq Seq.empty |]));
  [%expect {| Ok:  [{},[],{}] |}];
  ()
;;

(* TODO: pbt tests *)
let rec write
  : type a. int -> bool -> Buffy.W.state -> a Encoding.t -> a -> Buffy.W.written
  =
 fun depth first state encoding v ->
  match encoding with
  | Unit ->
    if depth > 0 && not first
    then Buffy.W.write_string state ",{}"
    else Buffy.W.write_string state "{}"
  | Null ->
    if depth > 0 && not first
    then Buffy.W.write_string state ",null"
    else Buffy.W.write_string state "null"
  | Bool ->
    (match v with
     | true ->
       if depth > 0 && not first
       then Buffy.W.write_string state ",true"
       else Buffy.W.write_string state "true"
     | false ->
       if depth > 0 && not first
       then Buffy.W.write_string state ",false"
       else Buffy.W.write_string state "false")
  | Int64 ->
    let* state =
      if depth > 0 && not first
      then Buffy.W.write_char state ','
      else Buffy.W.Written { state }
    in
    let s = Int64.to_string v in
    Buffy.W.write_string state s
  | String ->
    let* state =
      if depth > 0 && not first
      then Buffy.W.write_string state ",\""
      else Buffy.W.write_char state '"'
    in
    let* state = Buffy.W.write_string state v in
    Buffy.W.write_char state '"'
  | Seq t ->
    let* state =
      if depth > 0 && not first
      then Buffy.W.write_string state ",["
      else Buffy.W.write_char state '['
    in
    let rec go first state s =
      match s () with
      | Seq.Nil -> Buffy.W.Written { state }
      | Seq.Cons (v, s) ->
        let* state = write (depth + 1) first state t v in
        go false state s
    in
    let* state = go true state v in
    Buffy.W.write_char state ']'
  | Tuple t ->
    let* state =
      if depth > 0 && not first
      then Buffy.W.write_string state ",["
      else Buffy.W.write_char state '['
    in
    let* state = write_tuple (depth + 1) true state t v in
    Buffy.W.write_char state ']'
  | Object { field_hlist; fieldname_key_map = _; field_hmap = _ } ->
    let* state =
      if depth > 0 && not first
      then Buffy.W.write_string state ",{"
      else Buffy.W.write_char state '{'
    in
    let* state = write_object (depth + 1) true state field_hlist v in
    Buffy.W.write_char state '}'
  | Conv { serialisation; deserialisation = _; encoding } ->
    (* TODO: exn management in serialisation function *)
    write depth first state encoding (serialisation v)
  | Union { cases = _; serialisation; deserialisation = _ } ->
    let (AnyP ({ tag; encoding; inject = _ }, p)) = serialisation v in
    (match encoding with
     | Unit ->
       (* special case: cases without a payload are represented as strings *)
       let* state =
         if depth > 0 && not first
         then Buffy.W.write_string state ",\""
         else Buffy.W.write_char state '"'
       in
       let* state = Buffy.W.write_string state tag in
       Buffy.W.write_char state '"'
     | _ ->
       let* state =
         if depth > 0 && not first
         then Buffy.W.write_string state ",{\""
         else Buffy.W.write_string state "{\""
       in
       let* state = Buffy.W.write_string state tag in
       let* state = Buffy.W.write_string state "\":" in
       let* state = write (depth + 1) true state encoding p in
       Buffy.W.write_char state '}')

and write_tuple
  : type a. int -> bool -> Buffy.W.state -> a Encoding.tuple -> a -> Buffy.W.written
  =
 fun depth first state encoding v ->
  match encoding with
  | [] -> Buffy.W.Written { state }
  | t :: ts ->
    let (v :: vs) = v in
    let* state = write depth first state t v in
    write_tuple depth false state ts vs

and write_object
  : type a. int -> bool -> Buffy.W.state -> a Encoding.obj -> a -> Buffy.W.written
  =
 fun depth first state encoding v ->
  match encoding with
  | [] -> Buffy.W.Written { state }
  | Req { encoding; name; vkey = _; fkey = _ } :: ts ->
    let (v :: vs) = v in
    let* state =
      if depth > 0 && not first
      then Buffy.W.write_string state ",\""
      else Buffy.W.write_char state '"'
    in
    let* state = Buffy.W.write_string state name in
    let* state = Buffy.W.write_string state "\":" in
    let* state = write depth true state encoding v in
    write_object depth false state ts vs
  | Opt { encoding; name; vkey = _; fkey = _ } :: ts ->
    (match v with
     | None :: vs -> write_object depth first state ts vs
     | Some v :: vs ->
       let* state =
         if depth > 0 && not first
         then Buffy.W.write_string state ",\""
         else Buffy.W.write_char state '"'
       in
       let* state = Buffy.W.write_string state name in
       let* state = Buffy.W.write_string state "\":" in
       let* state = write depth true state encoding v in
       write_object depth false state ts vs)
;;

let write state encoding v = write 0 true state encoding v

let%expect_test _ =
  let scratch = String.make 20 ' ' in
  let w : type a. a Encoding.t -> a -> unit =
   fun enc v ->
    let scratch = Bytes.of_string scratch in
    let destination = Buffy.Dst.of_bytes scratch ~offset:1 ~length:18 in
    let state = Buffy.W.mk_state destination in
    match write state enc v with
    | Buffy.W.Written _ -> Format.printf "Ok: %s\n" (Bytes.unsafe_to_string scratch)
    | Buffy.W.Failed { state = _; error } ->
      Format.printf "Error: %s (%S)" error (Bytes.unsafe_to_string scratch)
    | Buffy.W.Suspended _ -> assert false
   (* not possible in these small tests *)
  in
  w Encoding.Unit ();
  [%expect {| Ok:  {} |}];
  w Encoding.int64 0L;
  [%expect {| Ok:  0 |}];
  w Encoding.int64 1L;
  [%expect {| Ok:  1 |}];
  w Encoding.(tuple [ unit; bool; bool ]) Commons.Hlist.[ (); true; false ];
  [%expect {| Ok:  [{},true,false] |}];
  ()
;;
