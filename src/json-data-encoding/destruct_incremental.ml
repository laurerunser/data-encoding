[@@@landmark "auto"]

type 'a result =
  | Ok of 'a
  | Error of string
  | Await of (Buffy.Src.t -> 'a result)

type buffy_state =
  { decoder : Jsonm.decoder
  ; buffy : Buffy.Src.t
  ; transfer_buffer : Bytes.t
  }

type state =
  | UseBuffy of buffy_state
  | UseTokens of Jsonm.lexeme list ref

let use_tokens tokens = UseTokens (ref tokens)
let change_buffy_src old_state src = { old_state with buffy = src }
let is_empty tokens = !tokens = []
let buffer_size = 4000 (* size of the Buffer to transfer bytes from Buffy to Jsonm *)

let read_from_buffy state =
  let { decoder; buffy; transfer_buffer } = state in
  (* let state = Buffy.R.mk_state src in *)
  let available = min (Buffy.Src.available buffy) buffer_size in
  if available <> 0
  then (
    (* read available bytes and feed them to Jsonm
       Reuse the same buffer to avoid allocations.
       Use `get_blits_onto_bytes` to avoid copying the bytes. *)
    Buffy.Src.get_blit_onto_bytes buffy transfer_buffer 0 available;
    Jsonm.Manual.src decoder transfer_buffer 0 available)
;;

let rec read_lexeme state =
  match state with
  | UseBuffy ({ decoder; buffy; transfer_buffer = _ } as s) ->
    (match Jsonm.decode decoder with
     | `Lexeme v -> Ok v
     | `End -> assert false (* see Jsonm.Manual docs *)
     | `Error e -> Error (Format.asprintf "Jsonm: %a" Jsonm.pp_error e)
     | `Await ->
       if Buffy.Src.available buffy > 0
       then (
         read_from_buffy s;
         read_lexeme (UseBuffy s))
       else
         Await
           (fun src ->
             read_from_buffy (change_buffy_src s src);
             read_lexeme (UseBuffy (change_buffy_src s src))))
  | UseTokens tokens ->
    if is_empty tokens
    then Error "Reading from memorized tokens: Unexpected end of input"
    else (
      let t = List.hd !tokens in
      tokens := List.tl !tokens;
      Ok t)
;;

let rec ( let* ) res (cont : 'a -> 'b result) : 'b result =
  match res with
  | Ok res -> cont res
  | Error e -> Error e
  | Await cont1 ->
    Await
      (fun v ->
        let* a = cont1 v in
        cont a)
;;

type expected_token =
  | OpenO
  | CloseO
  | Null
  | OpenA
  | CloseA

let expected_token_to_string expected =
  match expected with
  | Null -> "`Null"
  | OpenO -> "`Os"
  | CloseO -> "`Oe"
  | OpenA -> "`As"
  | CloseA -> "`Ae"
;;

let check_lexeme_is state expected =
  let* v = read_lexeme state in
  match v, expected with
  | `Null, Null -> Ok ()
  | `As, OpenA -> Ok ()
  | `Ae, CloseA -> Ok ()
  | `Os, OpenO -> Ok ()
  | `Oe, CloseO -> Ok ()
  | res, expected ->
    Error
      (Format.asprintf
         "Unexpected lexeme in sequence: received %a, expected %s"
         Jsonm.pp_lexeme
         res
         (expected_token_to_string expected))
;;

let consume_q state f_expected expected_msg =
  let* v = read_lexeme state in
  match f_expected v with
  | Some v -> Ok v
  | None ->
    Error
      (Format.asprintf
         "Unexpected lexeme in sequence: received %a, expected %s"
         Jsonm.pp_lexeme
         v
         expected_msg)
;;

let get_value_as_tokens : state -> Jsonm.lexeme list result =
 fun state ->
  let rec loop acc nb_bracket_open nb_curly_open =
    let* t = read_lexeme state in
    match t with
    | `Os -> loop (`Os :: acc) nb_bracket_open (nb_curly_open + 1)
    | `Oe ->
      if nb_curly_open - 1 = 0 && nb_bracket_open = 0
      then Ok (List.rev (`Oe :: acc))
      else loop (`Oe :: acc) nb_bracket_open (nb_curly_open - 1)
    | `As -> loop (`As :: acc) (nb_bracket_open + 1) nb_curly_open
    | `Ae ->
      if nb_bracket_open - 1 = 0 && nb_curly_open = 0
      then Ok (List.rev (`Ae :: acc))
      else loop (`Ae :: acc) (nb_bracket_open - 1) nb_curly_open
    | token ->
      if nb_curly_open = 0
      then Ok (List.rev (token :: acc))
      else loop (token :: acc) nb_bracket_open nb_curly_open
  in
  loop [] 0 0
;;

module StringMap = Map.Make (String)

let rec parse_fields_until_name_match field_name state map =
  let* v = read_lexeme state in
  match v with
  | `Name res when res = field_name ->
    let* tokens = get_value_as_tokens state in
    Ok (Some tokens, map)
  | `Name other_field_name ->
    let* tokens = get_value_as_tokens state in
    parse_fields_until_name_match
      field_name
      state
      (StringMap.add other_field_name tokens map)
  | `Oe -> Ok (None, map)
  | l -> Error (Format.asprintf "Unexpected lexeme in field: %a" Jsonm.pp_lexeme l)
;;

let rec destruct_value : type a. a Encoding.t -> state -> a result =
 fun encoding state ->
  match encoding with
  | Unit ->
    let* () = check_lexeme_is state OpenO in
    let* () = check_lexeme_is state CloseO in
    Ok ()
  | Null ->
    let* () = check_lexeme_is state Null in
    Ok ()
  | Bool ->
    consume_q
      state
      (function
       | `Bool b -> Some b
       | `Float _ | `String _ | `Null | `Name _ | `As | `Ae | `Os | `Oe -> None)
      "`Bool"
  | Int64 ->
    consume_q
      state
      (function
       | `String s -> Int64.of_string_opt s
       | `Bool _ | `Float _ | `Null | `Name _ | `As | `Ae | `Os | `Oe -> None)
      "`String (convertable to int64)"
  | String ->
    consume_q
      state
      (function
       | `String s -> Some s
       | `Bool _ | `Float _ | `Null | `Name _ | `As | `Ae | `Os | `Oe -> None)
      "`String"
  | Seq e ->
    let* () = check_lexeme_is state OpenA in
    destruct_seq e state []
  | Tuple e ->
    let* () = check_lexeme_is state OpenA in
    destruct_tuple e state
  | Object o ->
    let* () = check_lexeme_is state OpenO in
    destruct_obj o state
  | Conv { serialisation = _; deserialisation; encoding } ->
    let* v = destruct_value encoding state in
    (match deserialisation v with
     | Ok res -> Ok res
     | Error e -> Error e)
  | Union u ->
    let* t = read_lexeme state in
    destruct_union (Union u) state t

and destruct_seq : type a. a Encoding.t -> state -> a list -> a Seq.t result =
 (* Seq is a sequence of elements which all have the same type *)
 fun encoding state contents_so_far ->
  let* v = read_lexeme state in
  match v, encoding with
  | `Ae, _ -> Ok (List.to_seq (List.rev contents_so_far))
  | `Oe, _ -> Error "Unexpected lexeme in Seq: `Oe"
  | `Name n, _ ->
    Error (Format.asprintf "Unexpected lexeme in Seq: %a" Jsonm.pp_lexeme (`Name n))
  | `Os, Unit ->
    let* () = check_lexeme_is state CloseO in
    destruct_seq encoding state (() :: contents_so_far)
  | `Null, Null -> destruct_seq encoding state (() :: contents_so_far)
  | `Bool b, Bool -> destruct_seq encoding state (b :: contents_so_far)
  | `String s, String -> destruct_seq encoding state (s :: contents_so_far)
  | `String s, Int64 ->
    (match Int64.of_string_opt s with
     | Some i -> destruct_seq encoding state (i :: contents_so_far)
     | None -> failwith "Expected int64 in Seq")
  | `As, Seq new_e ->
    let* v = destruct_seq new_e state [] in
    destruct_seq encoding state (v :: contents_so_far)
  | `As, Tuple e ->
    let* tuple = destruct_tuple e state in
    destruct_seq encoding state (tuple :: contents_so_far)
  | `Os, Object o ->
    let* obj = destruct_obj o state in
    destruct_seq encoding state (obj :: contents_so_far)
  | _, Conv _ -> failwith "Can't have Conv inside a Seq"
  | t, Union u ->
    let* v = destruct_union (Union u) state t in
    destruct_seq encoding state (v :: contents_so_far)
  | l, _ -> Error (Format.asprintf "Unexpected lexeme in Seq: %a" Jsonm.pp_lexeme l)

and destruct_tuple : type a. a Encoding.tuple -> state -> a result =
 fun encoding state ->
  match encoding with
  | [] ->
    let* () = check_lexeme_is state CloseA in
    Ok Commons.Hlist.[]
  | t :: ts ->
    let* v = destruct_value t state in
    let* t = destruct_tuple ts state in
    Ok (Commons.Hlist.( :: ) (v, t))

and destruct_obj_reached_the_end
  : type a. a Encoding.obj -> state -> Jsonm.lexeme list StringMap.t -> a result
  =
 fun encoding state fields_and_tokens_map ->
  match encoding with
  | [] ->
    (* make sure there are no extra things in the fields map *)
    if StringMap.is_empty fields_and_tokens_map
    then Ok Commons.Hlist.[]
    else Error "Extraneous fields in object"
  | Req { encoding = e; name = field_name } :: ts ->
    (match StringMap.find_opt field_name fields_and_tokens_map with
     | None -> Error (Format.sprintf "Can't find field name %s" field_name)
     | Some tokens ->
       (* remove from the map *)
       let fields = StringMap.remove field_name fields_and_tokens_map in
       (* parse the value of the field *)
       let state = use_tokens tokens in
       let* v = destruct_value e state in
       let* vs = destruct_obj_reached_the_end ts state fields in
       (match state with
        | UseBuffy _ -> Ok (Commons.Hlist.( :: ) (v, vs))
        | UseTokens tokens ->
          if is_empty tokens
          then Ok (Commons.Hlist.( :: ) (v, vs))
          else Error "Too many lexemes in sequence"))
  | Opt { encoding = e; name = field_name } :: ts ->
    (match StringMap.find_opt field_name fields_and_tokens_map with
     | None ->
       (* optional field: don't fail, just parse the rest of the fields *)
       let* vs = destruct_obj_reached_the_end ts state fields_and_tokens_map in
       Ok (Commons.Hlist.( :: ) (None, vs))
     | Some tokens ->
       (* remove from the map *)
       let fields = StringMap.remove field_name fields_and_tokens_map in
       (* parse the value of the field *)
       let state = use_tokens tokens in
       let* v = destruct_value e state in
       let* vs = destruct_obj_reached_the_end ts state fields in
       (match state with
        | UseBuffy _ -> Ok (Commons.Hlist.( :: ) (Some v, vs))
        | UseTokens tokens ->
          if is_empty tokens
          then Ok (Commons.Hlist.( :: ) (Some v, vs))
          else Error "Too many lexemes in sequence"))

and destruct_obj_fields
  : type a. a Encoding.obj -> state -> Jsonm.lexeme list StringMap.t -> a result
  =
 fun encoding state fields ->
  match encoding with
  | [] ->
    let* () = check_lexeme_is state CloseO in
    Ok Commons.Hlist.[]
  | Req { encoding = e; name } :: ts ->
    (match StringMap.find_opt name fields with
     | Some tokens ->
       (* field is already parsed in the fields map *)
       (* remove from the map *)
       let fields = StringMap.remove name fields in
       (* parse the value of the field *)
       let new_state = use_tokens tokens in
       let* v = destruct_value e new_state in
       let* vs = destruct_obj_fields ts state fields in
       (match state with
        | UseBuffy _ -> Ok (Commons.Hlist.( :: ) (v, vs))
        | UseTokens tokens ->
          if is_empty tokens
          then Ok (Commons.Hlist.( :: ) (v, vs))
          else Error "Too many lexemes in sequence")
     | None ->
       let* tokens, map = parse_fields_until_name_match name state fields in
       (match tokens with
        | None -> Error "Missing field"
        | Some tokens ->
          let* a = destruct_value e (use_tokens tokens) in
          let* rest_of_values = destruct_obj_fields ts state map in
          Ok (Commons.Hlist.( :: ) (a, rest_of_values))))
  | Opt { encoding = e; name } :: ts ->
    (match StringMap.find_opt name fields with
     | Some tokens ->
       (* field is already parsed in the fields map *)
       (* remove from the map *)
       let fields = StringMap.remove name fields in
       (* parse the value of the field *)
       let new_state = use_tokens tokens in
       let* v = destruct_value e new_state in
       let* vs = destruct_obj_fields ts state fields in
       (match state with
        | UseBuffy _ -> Ok (Commons.Hlist.( :: ) (Some v, vs))
        | UseTokens tokens ->
          if is_empty tokens
          then Ok (Commons.Hlist.( :: ) (Some v, vs))
          else Error "Too many lexemes in sequence")
     | None ->
       let* tokens, map = parse_fields_until_name_match name state fields in
       (match tokens with
        | None ->
          let* rest_of_values = destruct_obj_reached_the_end ts state map in
          (* make the result with None for this element *)
          Ok (Commons.Hlist.( :: ) (None, rest_of_values))
        | Some tokens ->
          let* a = destruct_value e (use_tokens tokens) in
          let* rest_of_values = destruct_obj_fields ts state map in
          Ok (Commons.Hlist.( :: ) (Some a, rest_of_values))))

and destruct_obj : type a. a Encoding.obj -> state -> a result =
 fun encoding state -> destruct_obj_fields encoding state StringMap.empty

and destruct_union : type a. a Encoding.t -> state -> Jsonm.lexeme -> a result =
 fun union state first_token ->
  match union with
  | Union { cases = _; serialisation = _; deserialisation } ->
    let* n = consume_name state first_token in
    (match n with
     | None -> Error "Unexpected lexeme in union"
     | Some (Either.Left tag) ->
       (* tag inside an object *)
       (match deserialisation tag with
        | Error e -> Error e
        | Ok (AnyC { tag = _; encoding; inject }) ->
          let* contents = destruct_value encoding state in
          let* () = check_lexeme_is state CloseO in
          Ok (inject contents))
     | Some (Either.Right tag) ->
       (match deserialisation tag with
        | Error e -> Error e
        | Ok (AnyC { tag = _; encoding; inject }) ->
          (match encoding with
           | Unit -> Ok (inject ())
           | _ -> Error "Found payload-less case with a payload-full case encoding")))
  | _ -> assert false

and consume_name state token =
  match token with
  | `Os ->
    let* t = read_lexeme state in
    (match t with
     | `Name name -> Ok (Some (Either.Left name))
     | _ -> Ok None)
  | `String name -> Ok (Some (Either.Right name))
  | _ -> Ok None
;;

let destruct_incremental : type a. a Encoding.t -> Buffy.Src.t -> a result =
 fun encoding buffy ->
  let decoder = Jsonm.decoder ~encoding:`UTF_8 `Manual in
  (* add 2 spaces at the start of the input to avoid the Uutf bug
     (it uses the first 3 chars to determine its encoding, so 
     if there are less than 3 chars, the decoder returns Await) *)
  Jsonm.Manual.src decoder (Bytes.of_string "  ") 0 2;
  let transfer_buffer = Bytes.make buffer_size ' ' in
  let state = UseBuffy { decoder; buffy; transfer_buffer } in
  let res = destruct_value encoding state in
  match res, state with
  | Ok _, UseTokens tokens -> if is_empty tokens then res else Error "Too many lexemes"
  | Ok _, UseBuffy _ -> res (* TODO: check if the end of Jsonm *)
  | res, _ -> res
;;

let%expect_test _ =
  let open Encoding in
  let w e str =
    let buffy = Buffy.Src.of_string (str ^ " ") in
    match destruct_incremental e buffy with
    | Ok v -> Format.printf "Ok %s\n" (value_to_string e v)
    | Error s -> Format.printf "Error %s\n" s
    | Await _ -> Format.printf "Waiting\n"
  in
  w Unit "{}";
  [%expect {| Ok unit |}];
  w Unit "";
  [%expect {| Waiting |}];
  w Unit "} ";
  [%expect {| Error Jsonm: expected JSON text (JSON value) |}];
  w Unit "}{ ";
  [%expect {| Error Jsonm: expected JSON text (JSON value) |}];
  w Unit "{";
  [%expect {| Waiting |}];
  w Unit "[]";
  [%expect {| Error Unexpected lexeme in sequence: received `As, expected `Os |}];
  w Unit "][ ";
  [%expect {| Error Jsonm: expected JSON text (JSON value) |}];
  w Unit "null ";
  [%expect {| Error Unexpected lexeme in sequence: received `Null, expected `Os |}];
  w Unit {| {"foo": null} |};
  [%expect {| Error Unexpected lexeme in sequence: received `Name "foo", expected `Oe |}];
  w Bool "true ";
  [%expect {| Ok true |}];
  w Bool "false ";
  [%expect {| Ok false |}];
  w Bool {| "" |};
  [%expect {| Error Unexpected lexeme in sequence: received `String "", expected `Bool |}];
  w Bool {| "crash" |};
  [%expect
    {| Error Unexpected lexeme in sequence: received `String "crash", expected `Bool |}];
  w Bool "null ";
  [%expect {| Error Unexpected lexeme in sequence: received `Null, expected `Bool |}];
  w Int64 {| "0" |};
  [%expect {| Ok 0 |}];
  w Int64 {| "1234567890" |};
  [%expect {| Ok 1234567890 |}];
  w Int64 {| "this" |};
  [%expect
    {| Error Unexpected lexeme in sequence: received `String "this", expected `String (convertable to int64) |}];
  w Int64 {| null |};
  [%expect
    {| Error Unexpected lexeme in sequence: received `Null, expected `String (convertable to int64) |}];
  w String {| "haha" |};
  [%expect {| Ok haha |}];
  w String {| null |};
  [%expect {| Error Unexpected lexeme in sequence: received `Null, expected `String |}];
  w String {| "\u0068\u0123" |};
  [%expect {| Ok hģ |}];
  w String {| "\128" |};
  [%expect {| Error Jsonm: illegal escape, '1' (U+0031) not an escaped character |}];
  w String {| "\ntest\u03BB"|};
  [%expect {|
    Ok
    testλ |}];
  w (Seq Int64) "[]";
  [%expect {| Ok [] |}];
  w (Seq Int64) {| ["0"] |};
  [%expect {| Ok [0] |}];
  w (Seq Int64) {| ["0", "1"] |};
  [%expect {| Ok [0; 1] |}];
  w (Seq Int64) {| [ null ] |};
  [%expect {| Error Unexpected lexeme in Seq: `Null |}];
  w (Seq Int64) {| ["2", null] |};
  [%expect {| Error Unexpected lexeme in Seq: `Null |}];
  w (Tuple []) {| [] |};
  [%expect {| Ok [] |}];
  w (Tuple [ String; Bool ]) {| [ "0", true]|};
  [%expect {| Ok [0; true] |}];
  w (Tuple [ Int64; Int64; Int64 ]) {| [ "0", "3", "1" ]|};
  [%expect {| Ok [0; 3; 1] |}];
  w (* missing "" around the int64 *)
    (Object [ Req { encoding = Int64; name = "foo" } ])
    {| {"foo" : 32} |};
  [%expect
    {| Error Unexpected lexeme in sequence: received `Float 32., expected `String (convertable to int64) |}];
  w (Object [ Req { encoding = Int64; name = "foo" } ]) {| {"foo" : "32"} |};
  [%expect {| Ok {foo = 32} |}];
  w
    (Object
       [ Req { encoding = Int64; name = "foo" }; Req { encoding = Int64; name = "bar" } ])
    {| {"foo":"32", "bar":"43"} |};
  [%expect {| Ok {foo = 32; bar = 43} |}];
  let e = obj [ req "this" Unit ] in
  w e {| {"this" : {} } |};
  [%expect {| Ok {this = unit} |}];
  let e = obj [ req "this" Int64 ] in
  w e {| {"this" : [] } |};
  [%expect
    {| Error Unexpected lexeme in sequence: received `As, expected `String (convertable to int64) |}];
  let e = obj [ opt "foo" Int64; req "bar" Int64 ] in
  w e {| {"foo":"32", "bar":"43"} |};
  [%expect {| Ok {foo ?= 32; bar = 43} |}];
  w
    (Object
       [ Opt { encoding = Int64; name = "foo" }; Req { encoding = Int64; name = "bar" } ])
    {| {"bar":"43"} |};
  [%expect {| Ok {foo ?= None; bar = 43} |}];
  w
    (Object
       [ Opt { encoding = Int64; name = "foo" }; Opt { encoding = Int64; name = "bar" } ])
    {| {} |};
  [%expect {| Ok {foo ?= None; bar ?= None}  |}];
  w
    (Object
       [ Opt { encoding = Int64; name = "foo" }; Opt { encoding = Int64; name = "bar" } ])
    {| {"foo":"43311"} |};
  [%expect {| Ok {foo ?= 43311; bar ?= None} |}];
  w
    (Object
       [ Req { encoding = Int64; name = "foo" }; Req { encoding = Int64; name = "bar" } ])
    {| {"bar":"43311", "foo":"43311"} |};
  [%expect {| Ok {foo = 43311; bar = 43311} |}]
;;

type abc =
  | A of bool
  | B of int64
  | C of unit

let%expect_test _ =
  let open Encoding in
  let w e str =
    let buffy = Buffy.Src.of_string (str ^ " ") in
    match destruct_incremental e buffy with
    | Ok v -> Format.printf "Ok %s\n" (value_to_string e v)
    | Error s -> Format.printf "Error %s\n" s
    | Await _ -> Format.printf "Waiting\n"
  in
  let union_encoding =
    Encoding.Union.(
      union
        [ AnyC (case "0" Bool (fun a -> A a))
        ; AnyC (case "1" Int64 (fun b -> B b))
        ; AnyC (case "2" Unit (fun _ -> C ()))
        ]
        (fun _ -> failwith "")
        (fun str ->
          match str with
          | "0" -> Ok (AnyC (case "0" Bool (fun a -> A a)))
          | "1" -> Ok (AnyC (case "1" Int64 (fun b -> B b)))
          | "2" -> Ok (AnyC (case "2" Unit (fun _ -> C ())))
          | _ -> failwith ""))
  in
  w union_encoding {| "2" |};
  [%expect {| Ok union(bool | int64 | unit) |}];
  w union_encoding {| "0" : true |};
  [%expect {| Error Found payload-less case with a payload-full case encoding |}];
  w union_encoding {| {"0" : false} |};
  [%expect {| Ok union(bool | int64 | unit) |}];
  w union_encoding {| {"1" : true} |};
  [%expect
    {| Error Unexpected lexeme in sequence: received `Bool true, expected `String (convertable to int64) |}];
  w union_encoding {| {"2" : false} |};
  [%expect {| Error Unexpected lexeme in sequence: received `Bool false, expected `Os |}];
  w (Tuple [ Int64; Unit; String ]) {| ["0",{},"128"]|};
  [%expect {| Ok [0; unit; 128] |}]
;;

(* should fail: 2 can't have a payload*)
(* w union_encoding "\"2\" : \"test\" "; *)

(* [%expect {||}];
  w Unit "{}{" (fun fmt () -> Format.fprintf fmt "()");
  [%expect {| |}]; *)

(* leftover chars problem
  w Unit "{}{}";
  [%expect {| Ok unit |}];
  *)

let%expect_test _ =
  (* incremental tests with input cut into 2 parts *)
  let open Encoding in
  let w e str1 str2 =
    let buffy = Buffy.Src.of_string str1 in
    let buffy2 = Buffy.Src.of_string (str2 ^ " ") in
    match destruct_incremental e buffy with
    | Await f ->
      (match f buffy2 with
       | Ok v -> Format.printf "Ok %s\n" (value_to_string e v)
       | Error s -> Format.printf "Error %s\n" s
       | Await _ -> Format.printf "Await")
    | _ -> failwith "incomplete input; should have failed"
  in
  w Unit "{" "}";
  [%expect {| Ok unit |}];
  w Unit "}" " ";
  [%expect {| Error Jsonm: expected JSON text (JSON value) |}];
  w Unit "}{" " ";
  [%expect {| Error Jsonm: expected JSON text (JSON value) |}];
  w Unit "nu" "ll ";
  [%expect {| Error Unexpected lexeme in sequence: received `Null, expected `Os |}];
  w Bool "f" "alse ";
  [%expect {| Ok false |}];
  w Int64 "\"12345" "67890\"";
  [%expect {| Ok 1234567890 |}];
  w (Seq Int64) "[\"0\"" ", \"1\"]";
  [%expect {| Ok [0; 1] |}];
  w (Seq Int64) "[nu" "ll ]";
  [%expect {| Error Unexpected lexeme in Seq: `Null |}];
  w (Seq Int64) "[\"2\", n" "ull]";
  [%expect {| Error Unexpected lexeme in Seq: `Null |}];
  w (Tuple []) "[" "]";
  [%expect {| Ok [] |}];
  w String {| "\ntest\u03BB |} {| test" |};
  [%expect "\n    Ok\n    test\206\187  test"]
;;

(* doesn't like to be cut in the middle of a unicode sequence *)
(* w String {| "\ntest\u03 |} {| BBtest" |};
  [%expect "\n      Ok\n      test\206\187  test"] *)
