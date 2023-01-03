type filter =
  | Field of string
  | Index of int
  | Slice of int * int
  | Iter
  | Comma of filter list
  | Pipe of filter list

let rec assoc_opt x seq =
  match Seq.uncons seq with
  | None -> None
  | Some ((a, b), tail) -> if compare a x = 0 then Some b else assoc_opt x tail
;;

let nth_opt seq n =
  if n < 0
  then invalid_arg "List.nth"
  else (
    let rec nth_aux seq n =
      match Seq.uncons seq with
      | None -> None
      | Some (a, tail) -> if n = 0 then Some a else nth_aux tail (n - 1)
    in
    nth_aux seq n)
;;

let rec filter f json =
  match f, json with
  | Field s, `O fs ->
    (match assoc_opt s fs with
    | Some json -> Seq.return json
    | None -> Seq.empty)
  | Index n, `A vs ->
    if n >= 0
    then (
      match nth_opt vs n with
      | Some json -> Seq.return json
      | None -> Seq.empty)
    else (
      match List.nth_opt (List.rev (List.of_seq vs)) (abs n - 1) with
      | Some json -> Seq.return json
      | None -> Seq.empty)
  | Slice (low, high), `A vs -> Seq.take (high - low) (Seq.drop low vs)
  (* TODO: Slice, String *)
  (* TODO: Slice with negative integers *)
  (* TODO: Slice with omitted bounds *)
  | Iter, `A vs -> vs
  | Iter, `O vs -> Seq.map snd vs
  | Comma fs, json -> Seq.concat_map (fun f -> filter f json) (List.to_seq fs)
  | Pipe fs, json ->
    Seq.fold_left
      (fun jsons f -> Seq.concat_map (filter f) jsons)
      (Seq.return json)
      (List.to_seq fs)
  | _ -> Seq.empty
;;

let toA l = `A (List.to_seq l)
let toO l = `O (List.to_seq l)

let%expect_test _ =
  let w : filter -> JSON.t -> unit =
   fun f j ->
    let filtered = filter f j in
    Format.printf
      "%a\n"
      (Format.pp_print_seq ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ',') PP.mini)
      filtered
  in
  w (Pipe []) (`String "Hello, world!");
  [%expect {| "Hello, world!" |}];
  w (Field "foo") (toO [ "foo", `Float 42.; "bar", `String ".." ]);
  [%expect {| 42. |}];
  w (Field "foo") (toO [ "notfoo", `Float 42.; "bar", `String ".." ]);
  [%expect {| |}];
  w (Field "foo") (toO [ "foo", `Float 42. ]);
  [%expect {| 42. |}];
  w (Index 0) (toA [ toO [ "name", `String "JSON" ]; toO [ "name", `String "XML" ] ]);
  [%expect {| {"name":"JSON"} |}];
  w (Index 2) (toA [ toO [ "name", `String "JSON" ]; toO [ "name", `String "XML" ] ]);
  [%expect {| |}];
  w (Index (-2)) (toA [ `Float 1.; `Float 2.; `Float 3. ]);
  [%expect {| 2. |}];
  w
    (Slice (2, 4))
    (toA [ `String "a"; `String "b"; `String "c"; `String "d"; `String "e" ]);
  [%expect {| "c","d" |}];
  w Iter (toA [ `String "a"; `String "b"; `String "c"; `String "d"; `String "e" ]);
  [%expect {| "a","b","c","d","e" |}];
  w
    (Comma [ Field "foo"; Field "bar" ])
    (toO [ "foo", `Float 42.; "bar", `String ".."; "baz", `Bool true ]);
  [%expect {| 42.,".." |}];
  w
    (Pipe [ Iter; Field "name" ])
    (toA [ toO [ "name", `String "JSON" ]; toO [ "name", `String "XML" ] ]);
  [%expect {| "JSON","XML" |}];
  ()
;;
