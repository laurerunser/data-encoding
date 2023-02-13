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
  | Unit ->
    assert (v = ());
    Ok (`O [])
  | Bool -> Ok (`Bool v)
  | Int64 -> Ok (`String (Int64.to_string v))
  | String -> Ok (`String v) (* TODO check utf8 *)
  | Seq t ->
    let* seq = map_e (construct t) v in
    Ok (`Aseq seq)
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
      | Ok (`Aseq jsons) -> Ok (`Aseq (Seq.cons json jsons))
      | Ok (`A []) -> Ok (`Aseq (Seq.return json))
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
      | Ok (`Oseq jsons) -> Ok (`Oseq (Seq.cons (name, json) jsons))
      | Ok (`O []) -> Ok (`Oseq (Seq.return (name, json)))
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

let rec construct_lexemes : type a. a Encoding.t -> a -> JSON.lexeme Seq.t =
 fun encoding v () ->
  match encoding with
  | Unit ->
    assert (v = ());
    empty_obj
  | Bool -> Seq.Cons (`Bool v, Seq.empty)
  | Int64 -> Seq.Cons (`String (Int64.to_string v), Seq.empty)
  | String -> Seq.Cons (`String v (* TODO check utf8 *), Seq.empty)
  | Seq t -> Commons.Sequtils.bracket `As (Seq.flat_map (construct_lexemes t) v) `Ae ()
  | Tuple t -> Commons.Sequtils.bracket `As (construct_tuple_lexemes t v) `Ae ()
  | Object o -> Commons.Sequtils.bracket `Os (construct_obj_lexemes o v) `Oe ()
  | Conv { serialisation; deserialisation = _; encoding } ->
    construct_lexemes encoding (serialisation v) ()
(* TODO exception handling?? *)

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
  | Req { encoding = t; name } :: ts ->
    let (v :: vs) = v in
    Seq.append
      (Seq.cons (`Name name) (construct_lexemes t v))
      (construct_obj_lexemes ts vs)
      ()
  | Opt { encoding = t; name } :: ts ->
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
let rec write_lexemes depth first destination (lxms : JSON.lexeme Seq.t) =
  match lxms () with
  | exception exc ->
    let error = "Error whilst serialising: " ^ Printexc.to_string exc in
    Buffy.W.Failed { destination; error }
  | Seq.Nil -> Buffy.W.Written { destination }
  | Seq.Cons (lxm, lxms) ->
    (match lxm with
    | `Bool true ->
      let* destination =
        if depth > 0 && not first
        then Buffy.W.write_small_string destination ",true"
        else Buffy.W.write_small_string destination "true"
      in
      write_lexemes depth false destination lxms
    | `Bool false ->
      let* destination =
        if depth > 0 && not first
        then Buffy.W.write_small_string destination ",false"
        else Buffy.W.write_small_string destination "false"
      in
      write_lexemes depth false destination lxms
    | `Float f ->
      (* TODO: more tractable representation of float *)
      let literal = Float.to_string f in
      let literal = if depth > 0 && not first then "," ^ literal else literal in
      let* destination = Buffy.W.write_small_string destination literal in
      write_lexemes depth false destination lxms
    | `String s ->
      (* TODO: check for UTF8 validity *)
      let* destination =
        if depth > 0 && not first
        then Buffy.W.write_small_string destination ",\""
        else Buffy.W.write_char destination '"'
      in
      let* destination = Buffy.W.write_large_string destination s in
      let* destination = Buffy.W.write_char destination '"' in
      write_lexemes depth false destination lxms
    | `Null ->
      let* destination =
        if depth > 0 && not first
        then Buffy.W.write_small_string destination ",null"
        else Buffy.W.write_small_string destination "null"
      in
      write_lexemes depth false destination lxms
    | `As ->
      let* destination =
        if depth > 0 && not first
        then Buffy.W.write_small_string destination ",["
        else Buffy.W.write_char destination '['
      in
      write_lexemes (depth + 1) true destination lxms
    | `Ae ->
      let* destination = Buffy.W.write_char destination ']' in
      write_lexemes (depth - 1) false destination lxms
    | `Os ->
      let* destination =
        if depth > 0 && not first
        then Buffy.W.write_small_string destination ",{"
        else Buffy.W.write_char destination '{'
      in
      write_lexemes (depth + 1) true destination lxms
    | `Oe ->
      let* destination = Buffy.W.write_char destination '}' in
      write_lexemes (depth - 1) false destination lxms
    | `Name s ->
      let* destination =
        if depth > 0 && not first
        then Buffy.W.write_small_string destination ",\""
        else Buffy.W.write_char destination '"'
      in
      let* destination = Buffy.W.write_large_string destination s in
      let* destination = Buffy.W.write_small_string destination "\":" in
      write_lexemes (depth + 1) true destination lxms)
;;

let write_lexemes destination lxms = write_lexemes 0 true destination lxms

let%expect_test _ =
  let scratch = String.make 20 ' ' in
  let w : JSON.lexeme Seq.t -> unit =
   fun lxms ->
    let destination = Buffy.W.mk_destination (Bytes.of_string scratch) 1 18 in
    match write_lexemes destination lxms with
    | Buffy.W.Written { destination } ->
      Format.printf "Ok: %s\n" (Bytes.unsafe_to_string destination.buffer)
    | Buffy.W.Failed { destination; error } ->
      Format.printf "Error: %s (%S)" error (Bytes.unsafe_to_string destination.buffer)
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
    : type a. int -> bool -> Buffy.W.destination -> a Encoding.t -> a -> Buffy.W.written
  =
 fun depth first destination encoding v ->
  match encoding with
  | Unit ->
    assert (v = ());
    if depth > 0 && not first
    then Buffy.W.write_small_string destination ",{}"
    else Buffy.W.write_small_string destination "{}"
  | Bool ->
    (match v with
    | true ->
      if depth > 0 && not first
      then Buffy.W.write_small_string destination ",true"
      else Buffy.W.write_small_string destination "true"
    | false ->
      if depth > 0 && not first
      then Buffy.W.write_small_string destination ",false"
      else Buffy.W.write_small_string destination "false")
  | Int64 ->
    let* destination =
      if depth > 0 && not first
      then Buffy.W.write_char destination ','
      else Buffy.W.Written { destination }
    in
    let s = Int64.to_string v in
    Buffy.W.write_small_string destination s
  | String ->
    let* destination =
      if depth > 0 && not first
      then Buffy.W.write_small_string destination ",\""
      else Buffy.W.write_char destination '"'
    in
    let* destination = Buffy.W.write_large_string destination v in
    Buffy.W.write_char destination '"'
  | Seq t ->
    let* destination =
      if depth > 0 && not first
      then Buffy.W.write_small_string destination ",["
      else Buffy.W.write_char destination '['
    in
    let rec go first destination s =
      match s () with
      | Seq.Nil -> Buffy.W.Written { destination }
      | Seq.Cons (v, s) ->
        let* destination = write (depth + 1) first destination t v in
        go false destination s
    in
    let* destination = go true destination v in
    Buffy.W.write_char destination ']'
  | Tuple t ->
    let* destination =
      if depth > 0 && not first
      then Buffy.W.write_small_string destination ",["
      else Buffy.W.write_char destination '['
    in
    let* destination = write_tuple (depth + 1) true destination t v in
    Buffy.W.write_char destination ']'
  | Object o ->
    let* destination =
      if depth > 0 && not first
      then Buffy.W.write_small_string destination ",{"
      else Buffy.W.write_char destination '{'
    in
    let* destination = write_object (depth + 1) true destination o v in
    Buffy.W.write_char destination '}'
  | Conv { serialisation; deserialisation = _; encoding } ->
    (* TODO: exn management in serialisation function *)
    write depth first destination encoding (serialisation v)

and write_tuple
    : type a.
      int -> bool -> Buffy.W.destination -> a Encoding.tuple -> a -> Buffy.W.written
  =
 fun depth first destination encoding v ->
  match encoding with
  | [] -> Buffy.W.Written { destination }
  | t :: ts ->
    let (v :: vs) = v in
    let* destination = write depth first destination t v in
    write_tuple depth false destination ts vs

and write_object
    : type a. int -> bool -> Buffy.W.destination -> a Encoding.obj -> a -> Buffy.W.written
  =
 fun depth first destination encoding v ->
  match encoding with
  | [] -> Buffy.W.Written { destination }
  | Req { encoding; name } :: ts ->
    let (v :: vs) = v in
    let* destination =
      if depth > 0 && not first
      then Buffy.W.write_small_string destination ",\""
      else Buffy.W.write_char destination '"'
    in
    let* destination = Buffy.W.write_large_string destination name in
    let* destination = Buffy.W.write_small_string destination "\":" in
    let* destination = write depth true destination encoding v in
    write_object depth false destination ts vs
  | Opt { encoding; name } :: ts ->
    (match v with
    | None :: vs -> write_object depth first destination ts vs
    | Some v :: vs ->
      let* destination =
        if depth > 0 && not first
        then Buffy.W.write_small_string destination ",\""
        else Buffy.W.write_char destination '"'
      in
      let* destination = Buffy.W.write_large_string destination name in
      let* destination = Buffy.W.write_small_string destination "\":" in
      let* destination = write depth true destination encoding v in
      write_object depth false destination ts vs)
;;

let write destination encoding v = write 0 true destination encoding v

let%expect_test _ =
  let scratch = String.make 20 ' ' in
  let w : type a. a Encoding.t -> a -> unit =
   fun enc v ->
    let destination = Buffy.W.mk_destination (Bytes.of_string scratch) 1 18 in
    match write destination enc v with
    | Buffy.W.Written { destination } ->
      Format.printf "Ok: %s\n" (Bytes.unsafe_to_string destination.buffer)
    | Buffy.W.Failed { destination; error } ->
      Format.printf "Error: %s (%S)" error (Bytes.unsafe_to_string destination.buffer)
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
