[@@@landmark "auto"]

type 'a result =
  | Ok of 'a
  | Error of string
  | Await of (Buffy.Src.t -> 'a result)

type state =
  { decoder : Jsonm.decoder
  ; buffy : Buffy.Src.t
  ; transfer_buffer : Bytes.t
  ; token_peeked : Jsonm.lexeme option ref
  }

let buffer_size = 4000 (* size of the Buffer to transfer bytes from Buffy to Jsonm *)
let change_buffy old_state new_buffy = { old_state with buffy = new_buffy }

let read_from_buffy decoder buffy transfer_buffer =
  let available = min (Buffy.Src.available buffy) buffer_size in
  if available <> 0
  then (
    (* read available bytes and  them to Jsonm
       Reuse the same buffer to avoid allocations.
       Use `get_blits_onto_bytes` to avoid copying the bytes. *)
    Buffy.Src.get_blit_onto_bytes buffy transfer_buffer 0 available;
    Jsonm.Manual.src decoder transfer_buffer 0 available)
  else Jsonm.Manual.src decoder transfer_buffer 0 0
;;

let rec read_lexeme state =
  let { decoder; buffy; transfer_buffer; token_peeked } = state in
  (* if the token has been peeked, remove it and return it*)
  if Option.is_some !token_peeked
  then (
    let tmp = Option.get !token_peeked in
    token_peeked := None;
    Ok tmp)
  else (
    match Jsonm.decode decoder with
    | `Lexeme v -> Ok v
    | `End -> assert false (* see Jsonm.Manual docs *)
    | `Error e ->
      let (a, b), (c, d) = Jsonm.decoded_range decoder in
      Error
        (Format.asprintf
           "Jsonm: (line:%d, col:%d) (line:%d, col:%d) %a"
           a
           b
           c
           d
           Jsonm.pp_error
           e)
    | `Await ->
      if Buffy.Src.available buffy > 0
      then (
        read_from_buffy decoder buffy transfer_buffer;
        read_lexeme state)
      else
        Await
          (fun src ->
            read_from_buffy decoder src transfer_buffer;
            read_lexeme (change_buffy state src)))
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

let peek state =
  let { decoder = _; buffy = _; transfer_buffer = _; token_peeked } = state in
  if Option.is_some !token_peeked
  then Ok (Option.get !token_peeked)
  else
    let* t = read_lexeme state in
    state.token_peeked := Some t;
    Ok t
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

module StringMap = Map.Make (String)

let rec hlist_of_vmap : type a. a Encoding.obj -> Encoding.Hmap.t -> a result =
 fun o vmap ->
  match o with
  | [] -> Ok Commons.Hlist.[]
  | Req { encoding = _; fkey = _; name = _; vkey } :: o ->
    (match Encoding.Hmap.find vmap vkey with
     | None -> failwith "missing field" (* TODO: better message *)
     | Some v ->
       let* vs = hlist_of_vmap o vmap in
       Ok (Commons.Hlist.( :: ) (v, vs)))
  | Opt { encoding = _; fkey = _; name = _; vkey } :: o ->
    (match Encoding.Hmap.find vmap vkey with
     | None ->
       let* vs = hlist_of_vmap o vmap in
       Ok (Commons.Hlist.( :: ) (None, vs))
     | Some v ->
       let* vs = hlist_of_vmap o vmap in
       Ok (Commons.Hlist.( :: ) (Some v, vs)))
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
  | Object { field_hlist; fieldname_key_map; field_hmap } ->
    let* () = check_lexeme_is state OpenO in
    destruct_obj field_hlist fieldname_key_map field_hmap state
  | Conv { serialisation = _; deserialisation; encoding } ->
    let* v = destruct_value encoding state in
    (match deserialisation v with
     | Ok res -> Ok res
     | Error e -> Error e)
  | Union u -> destruct_union (Union u) state

and destruct_seq : type a. a Encoding.t -> state -> a list -> a Seq.t result =
 (* Seq is a sequence of elements which all have the same type *)
 fun encoding state contents_so_far ->
  let* v = peek state in
  match v, encoding with
  | `Ae, _ ->
    let* _ = read_lexeme state in
    Ok (List.to_seq (List.rev contents_so_far))
  | `Oe, _ -> Error "Unexpected lexeme in Seq: `Oe"
  | `Name n, _ ->
    Error (Format.asprintf "Unexpected lexeme in Seq: %a" Jsonm.pp_lexeme (`Name n))
  | `Os, Unit ->
    let* _ = read_lexeme state in
    let* () = check_lexeme_is state CloseO in
    destruct_seq encoding state (() :: contents_so_far)
  | `Null, Null ->
    let* _ = read_lexeme state in
    destruct_seq encoding state (() :: contents_so_far)
  | `Bool b, Bool ->
    let* _ = read_lexeme state in
    destruct_seq encoding state (b :: contents_so_far)
  | `String s, String ->
    let* _ = read_lexeme state in
    destruct_seq encoding state (s :: contents_so_far)
  | `String s, Int64 ->
    let* _ = read_lexeme state in
    (match Int64.of_string_opt s with
     | Some i -> destruct_seq encoding state (i :: contents_so_far)
     | None -> Error "Expected int64 in Seq")
  | `As, Seq new_e ->
    let* _ = read_lexeme state in
    let* v = destruct_seq new_e state [] in
    destruct_seq encoding state (v :: contents_so_far)
  | `As, Tuple e ->
    let* _ = read_lexeme state in
    let* tuple = destruct_tuple e state in
    destruct_seq encoding state (tuple :: contents_so_far)
  | `Os, Object { field_hlist; fieldname_key_map; field_hmap } ->
    let* _ = read_lexeme state in
    let* obj = destruct_obj field_hlist fieldname_key_map field_hmap state in
    destruct_seq encoding state (obj :: contents_so_far)
  | _, Union u ->
    let* v = destruct_union (Union u) state in
    destruct_seq encoding state (v :: contents_so_far)
  | _, Conv _ ->
    (* if the encoding is a conv, I can't consume a lexeme 
           (because I don't know what it should be at all), 
            hence why I need this outer match first *)
    let* c = destruct_value encoding state in
    destruct_seq encoding state (c :: contents_so_far)
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

and destruct_obj_fields
  :  Encoding.anykey Encoding.FieldKeyMap.t -> Encoding.Hmap.t -> state -> Commons.Hmap.t
  -> Commons.Hmap.t result
  =
 fun fkmap fmap state seenfields ->
  let open Encoding in
  let* t = read_lexeme state in
  match t with
  | `Oe -> Ok seenfields
  | `Name fname ->
    let afk = FieldKeyMap.find_opt fname fkmap in
    (match afk with
     | None -> failwith "unknown filed name"
     | Some (Anykey afk) ->
       let f = Hmap.find fmap afk in
       (match f with
        | None -> assert false (* internal issue with constructing objects *)
        | Some (Req { encoding; fkey = _; vkey; name }) ->
          assert (name = fname);
          let* v = destruct_value encoding state in
          (* TODO: check for collisions: field already seen *)
          let seenfields = Hmap.add seenfields vkey v in
          destruct_obj_fields fkmap fmap state seenfields
        | Some (Opt { encoding; fkey = _; vkey; name }) ->
          assert (name = fname);
          let* v = destruct_value encoding state in
          (* TODO: check for collisions: field already seen *)
          let seenfields = Hmap.add seenfields vkey v in
          destruct_obj_fields fkmap fmap state seenfields))
  | _ -> failwith "TODO nice error message"

and destruct_obj
  : type a.
    a Encoding.obj
    -> Encoding.anykey Encoding.FieldKeyMap.t
    -> Encoding.Hmap.t
    -> state
    -> a result
  =
 fun fieldlist fieldname_key_map field_hmap state ->
  let* seen =
    destruct_obj_fields fieldname_key_map field_hmap state Encoding.Hmap.empty
  in
  hlist_of_vmap fieldlist seen

and destruct_union : type a. a Encoding.t -> state -> a result =
 fun union state ->
  match union with
  | Union { cases = _; serialisation = _; deserialisation } ->
    let* n = consume_name state in
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

and consume_name state =
  let* token = read_lexeme state in
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
  let state = { decoder; buffy; transfer_buffer; token_peeked = ref None } in
  let res = destruct_value encoding state in
  match res with
  | Ok _ ->
    if Option.is_some !(state.token_peeked)
    then Error "Too many lexemes"
    else res (* TODO: check if the end of Jsonm *)
  | res -> res
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
  [%expect
    {| Error Jsonm: (line:1, col:3) (line:1, col:3) expected JSON text (JSON value) |}];
  w Unit "}{ ";
  [%expect
    {| Error Jsonm: (line:1, col:3) (line:1, col:4) expected JSON text (JSON value) |}];
  w Unit "{";
  [%expect {| Waiting |}];
  w Unit "[]";
  [%expect {| Error Unexpected lexeme in sequence: received `As, expected `Os |}];
  w Unit "][ ";
  [%expect
    {| Error Jsonm: (line:1, col:3) (line:1, col:4) expected JSON text (JSON value) |}];
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
  [%expect
    {|
    Error Jsonm: (line:1, col:4) (line:1, col:6) illegal escape, '1' (U+0031) not an
                                           escaped character |}];
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
  w (* missing "" around the int64 *) (obj [ req "foo" Int64 ]) {| {"foo" : 32} |};
  [%expect
    {| Error Unexpected lexeme in sequence: received `Float 32., expected `String (convertable to int64) |}];
  w (obj [ req "foo" Int64 ]) {| {"foo" : "32"} |};
  [%expect {| Ok {foo = 32} |}];
  w (obj [ req "foo" int64; req "bar" Int64 ]) {| {"foo":"32", "bar":"43"} |};
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
  w (obj [ opt "foo" Int64; req "bar" Int64 ]) {| {"bar":"43"} |};
  [%expect {| Ok {foo ?= None; bar = 43} |}];
  w (obj [ opt "foo" Int64; opt "bar" Int64 ]) {| {} |};
  [%expect {| Ok {foo ?= None; bar ?= None}  |}];
  w (obj [ opt "foo" Int64; opt "bar" Int64 ]) {| {"foo":"43311"} |};
  [%expect {| Ok {foo ?= 43311; bar ?= None} |}];
  w (obj [ req "foo" Int64; req "bar" Int64 ]) {| {"bar":"43311", "foo":"43311"} |};
  [%expect {| Ok {foo = 43311; bar = 43311} |}]
;;

type abc =
  | A of bool
  | B of int64
  | C of unit

type data =
  { x : int64
  ; y : int64
  }

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
  [%expect {| Ok [0; unit; 128] |}];
  let r =
    Record.(
      record
        (fun x y -> { x; y })
        [ field "x" (fun { x; _ } -> x) int64; field "y" (fun { y; _ } -> y) int64 ])
  in
  let str = {|  {"x":"0", "y":"1"} |} in
  w r str;
  [%expect "Ok conv[{x=int64, y=int64}]"];
  w (list int64) {| [] |};
  [%expect "Ok conv[[int64 seq] ]"];
  w (array int64) {| [ "43" ] |};
  [%expect "Ok conv[[int64 seq] ]"];
  w (list (array int64)) {|[[], [] ]|};
  [%expect "Ok conv[[conv([int64 seq] ) seq] ]"];
  w (list (array int64)) {|[[ "43" ]]|};
  [%expect "Ok conv[[conv([int64 seq] ) seq] ]"];
  w (list (array r)) {|[[ {"x":"0", "y":"1"} ]]|};
  [%expect "Ok conv[[conv([conv({x=int64, y=int64}) seq] ) seq] ]"]
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
  (* let w3 e str1 str2 =
    let buffy = Buffy.Src.of_string str1 in
    let buffy2 = Buffy.Src.of_string (str2 ^ " ") in
    let buffy3 = Buffy.Src.of_string "" in
    match destruct_incremental e buffy with
    | Await f ->
      (match f buffy2 with
       | Ok v -> Format.printf "Ok %s\n" (value_to_string e v)
       | Error s -> Format.printf "Error %s\n" s
       | Await _ ->
         (match f buffy3 with
          | Ok v -> Format.printf "Ok %s\n" (value_to_string e v)
          | Error s -> Format.printf "Error %s\n" s
          | Await _ -> Format.printf "Await"))
    | _ -> failwith "incomplete input; should have failed"
  in *)
  w Unit "{" "}";
  [%expect {| Ok unit |}];
  w Unit "}" " ";
  [%expect
    {| Error Jsonm: (line:1, col:3) (line:1, col:3) expected JSON text (JSON value) |}];
  w Unit "}{" " ";
  [%expect
    {| Error Jsonm: (line:1, col:3) (line:1, col:4) expected JSON text (JSON value) |}];
  w Unit "nu" "ll ";
  [%expect {| Error Unexpected lexeme in sequence: received `Null, expected `Os |}];
  w Bool "f" "alse ";
  [%expect {| Ok false |}];
  w Int64 "\"12345" "67890\"";
  [%expect {| Ok 1234567890 |}];
  w Int64 {|"1|} {|"|};
  [%expect {| Ok 1 |}];
  w (Seq Int64) "[\"0\"" ", \"1\"]";
  [%expect {| Ok [0; 1] |}];
  w (Seq Int64) "[nu" "ll ]";
  [%expect {| Error Unexpected lexeme in Seq: `Null |}];
  w (Seq Int64) "[\"2\", n" "ull]";
  [%expect {| Error Unexpected lexeme in Seq: `Null |}];
  w (Tuple []) "[" "]";
  [%expect {| Ok [] |}];
  w String {| "\ntest\u03BB |} {| test" |};
  [%expect "\n    Ok\n    test\206\187  test"];
  w
    (list
       (array
          Record.(
            record
              (fun x y -> { x; y })
              [ field "x" (fun { x; _ } -> x) int64; field "y" (fun { y; _ } -> y) int64 ])))
    {|[[{"x":"0","y":"1|}
    {|"}]]|};
  [%expect {| Ok conv[[conv([conv({x=int64, y=int64}) seq] ) seq] ] |}];
  w
    (list
       (array
          Record.(
            record
              (fun x y -> { x; y })
              [ field "x" (fun { x; _ } -> x) int64; field "y" (fun { y; _ } -> y) int64 ])))
    {|[[{"x":"0","y"|}
    {|:"1"}]]|};
  [%expect {| Ok conv[[conv([conv({x=int64, y=int64}) seq] ) seq] ] |}];
  w
    (list
       (array
          Record.(
            record
              (fun x y -> { x; y })
              [ field "x" (fun { x; _ } -> x) int64; field "y" (fun { y; _ } -> y) int64 ])))
    {|[[{"x":"0","y":|}
    {|"1"}]]|};
  [%expect {| Ok conv[[conv([conv({x=int64, y=int64}) seq] ) seq] ] |}]
;;
(* doesn't like to be cut in the middle of a unicode sequence *)
(* w String {| "\ntest\u03 |} {| BBtest" |};
  [%expect "\n      Ok\n      test\206\187  test"] *)
