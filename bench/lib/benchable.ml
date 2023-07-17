(* to generate the payloads: `dune exec bench/lib/helpers/make_json_reading_payload.exe` 
   it takes a few seconds *)

module type S = sig
  type data

  val encoding : data Data_encoding.Encoding.t
  val make_data : int -> data
  val make_json_string : int -> string
  val name : string
end

(* TODO: avoid the split declaration encodings by filling up the common interface *)

module Benchable0 : S = struct
  type datum =
    { x : int64
    ; y : int64
    }

  type data = datum array list

  let encoding : data Data_encoding.Encoding.t =
    let binary =
      let open Binary_data_encoding.Encoding in
      list
        `Uint30
        (array
           `Uint8 (*TODO: support sized arrays, support N*)
           (conv
              ~serialisation:(fun { x; y } -> Commons.Hlist.[ x; y ])
              ~deserialisation:(fun Commons.Hlist.[ x; y ] -> Ok { x; y })
              (tuple [ int64; int64 ])))
    in
    let json =
      let open Json_data_encoding.Encoding in
      list
        (array
           Record.(
             record
               (fun x y -> { x; y })
               [ field "x" (fun { x; _ } -> x) int64
               ; field "y" (fun { y; _ } -> y) int64
               ]))
    in
    { Data_encoding.Encoding.binary; json }
  ;;

  let make_data sz =
    let datum = { x = 0L; y = 1L } in
    let arr n = Array.make n datum in
    let rec mk count acc =
      if count <= 0
      then acc
      else if count = 1
      then arr 10 :: acc
      else (
        assert (count >= 2);
        let delta = count mod 5 in
        mk (count - 2) (arr (10 - delta) :: arr (10 + delta) :: acc))
    in
    mk sz []
  ;;

  let make_json_string size =
    let one_record = {| {"x":"0","y":"1"} |} in
    let rec list n acc = if n = 0 then acc else list (n - 1) (one_record :: acc) in
    let one_array n = "[" ^ String.concat "," (list n []) ^ "]" in
    let rec make count acc =
      if count <= 0
      then acc
      else if count = 1
      then one_array 10 :: acc
      else (
        assert (count >= 2);
        let delta = count mod 5 in
        make (count - 2) (one_array (10 - delta) :: one_array (10 + delta) :: acc))
    in
    "[[" ^ String.concat "," (make size []) ^ "]]"
  ;;

  let name = "list[ui30](array[ui8](record(i64,i64)))"
end

module Benchable1 : S = struct
  type l =
    { x : int64
    ; y : int64
    }

  type r = (Commons.Sizedints.Uint30.t * int list) option
  type datum = (l, r) Either.t
  type data = datum array

  let encoding : data Data_encoding.Encoding.t =
    let binary =
      let open Binary_data_encoding.Encoding in
      array
        `Uint30
        (either
           (conv
              ~serialisation:(fun { x; y } -> Commons.Hlist.[ x; y ])
              ~deserialisation:(fun Commons.Hlist.[ x; y ] -> Ok { x; y })
              (tuple [ int64; int64 ]))
           (option
              (conv
                 ~serialisation:(fun (x, xs) ->
                   let xs =
                     List.map (fun x -> Option.get (Commons.Sizedints.Uint8.of_int x)) xs
                   in
                   Commons.Hlist.[ x; xs ])
                 ~deserialisation:(fun Commons.Hlist.[ x; xs ] ->
                   let xs = (xs :> int list) in
                   Ok (x, xs))
                 (tuple [ uint30; list `Uint30 uint8 ]))))
    in
    let json =
      let open Json_data_encoding.Encoding in
      array
        (Union.either
           (conv
              ~serialisation:(fun { x; y } -> Commons.Hlist.[ x; y ])
              ~deserialisation:(fun Commons.Hlist.[ x; y ] -> Ok { x; y })
              (tuple [ int64; int64 ]))
           (Union.option
              (conv
                 ~serialisation:(fun (x, xs) ->
                   let xs =
                     List.map (fun x -> Option.get (Commons.Sizedints.Uint8.of_int x)) xs
                   in
                   Commons.Hlist.[ x; xs ])
                 ~deserialisation:(fun Commons.Hlist.[ x; xs ] ->
                   let xs = (xs :> int list) in
                   Ok (x, xs))
                 (tuple [ uint30; list uint8 ]))))
    in
    { Data_encoding.Encoding.binary; json }
  ;;

  let make_data sz : data =
    Array.init sz (fun i ->
      if i mod 3 = 0
      then Either.Left { x = 0L; y = Int64.of_int i }
      else if i mod 5 = 0
      then Either.Right None
      else Either.Right (Some (Option.get (Commons.Sizedints.Uint30.of_int i), [])))
  ;;

  let make_json_string size =
    let first_choice = {|{"Left":["0","1"]}|} in
    let second_choice i =
      Format.sprintf {|{"Right":{"Some":["%d",["%d","%d"]]}}|} i i i
    in
    let second_none = {|{"Right":"None"}|} in
    let rec make_str count acc =
      if count = 0
      then acc
      else if count mod 3 = 0
      then make_str (count - 1) (first_choice :: acc)
      else if count mod 5 = 0
      then make_str (count - 1) (second_choice count :: acc)
      else make_str (count - 1) (second_none :: acc)
    in
    "[" ^ String.concat "," (make_str size []) ^ "]"
  ;;

  let name = "array[ui30](either(conv(i64,i64),option(conv(ui30,list(i8)))))"
end

module Benchable2 : S = struct
  type a =
    { x : int64 option
    ; y : int64 option
    }

  type b =
    { x : int64
    ; y : int64
    }

  type c = string list option

  type datum =
    | A of a
    | B of b
    | C of c
    | D

  type data = datum list

  let encoding : data Data_encoding.Encoding.t =
    let open Data_encoding.Encoding in
    list
      `Uint30
      (let open Union in
      let case_a =
        (case 0L "A" (tuple [ option int64; option int64 ]))
          (fun Commons.Hlist.[ x; y ] -> A { x; y })
      in
      let anycase_a = AnyC case_a in
      let case_b =
        (case 1L "B" (tuple [ int64; int64 ])) (fun Commons.Hlist.[ x; y ] -> B { x; y })
      in
      let anycase_b = AnyC case_b in
      let case_c = case 2L "C" (option (list `Uint30 (string `Uint30))) (fun p -> C p) in
      let anycase_c = AnyC case_c in
      let case_d = case_unit 3L "D" (fun () -> D) in
      let anycase_d = AnyC case_d in
      union
        Binary_data_encoding.Encoding.int64
        [ anycase_a; anycase_b; anycase_c; anycase_d ]
        (function
         | A { x; y } -> AnyP (case_a, [ x; y ])
         | B { x; y } -> AnyP (case_b, [ x; y ])
         | C p -> AnyP (case_c, p)
         | D -> AnyP (case_d, ()))
        (function
         | 0L -> Ok anycase_a
         | 1L -> Ok anycase_b
         | 2L -> Ok anycase_c
         | 3L -> Ok anycase_d
         | _ -> Error "unkown tag")
        (function
         | "A" -> Ok anycase_a
         | "B" -> Ok anycase_b
         | "C" -> Ok anycase_c
         | "D" -> Ok anycase_d
         | _ -> Error "unkown tag"))
  ;;

  let make_data sz : data =
    let prng = Random.State.make [| sz |] in
    let opt prng f = if Random.State.int prng 4 = 0 then Some (f prng) else None in
    List.init sz (fun k ->
      match Random.State.int prng 4 with
      | 0 ->
        let x = opt prng (fun prng -> Random.State.int64 prng 1024L) in
        let y = opt prng (fun prng -> Random.State.int64 prng 1024L) in
        A { x; y }
      | 1 ->
        let x = Random.State.int64 prng 1024L in
        let y = Random.State.int64 prng 1024L in
        B { x; y }
      | 2 ->
        let p =
          opt prng (fun prng ->
            List.init (Random.State.int prng 5) (fun i ->
              String.make (i + (k mod 256)) '0'))
        in
        C p
      | 3 -> D
      | _ -> assert false)
  ;;

  let make_json_string size =
    (* randomness generators *)
    let prng = Random.State.make [| size |] in
    let random prng = Random.State.int prng 4 = 0 in
    let random_int64 () = Random.State.int64 prng 1024L in
    (* strings for each element *)
    let make_str_a () =
      let choice_a1 = {|{"A":["None","None"]}|} in
      let choice_a2 () =
        Format.sprintf {|{"A":[{"Some":"%Ld"},"None"]}|} (random_int64 ())
      in
      let choice_a3 () =
        Format.sprintf {|{"A":["None",{"Some":"%Ld"}]}|} (random_int64 ())
      in
      let choice_a4 () =
        Format.sprintf
          {|{"A":[{"Some":"%Ld"},{"Some":"%Ld"}]}|}
          (random_int64 ())
          (random_int64 ())
      in
      match Random.State.int prng 4 with
      | 0 -> choice_a1
      | 1 -> choice_a2 ()
      | 2 -> choice_a3 ()
      | 3 -> choice_a4 ()
      | _ -> assert false
    in
    let make_str_b () =
      Format.sprintf {|{"B":["%Ld","%Ld"]}|} (random_int64 ()) (random_int64 ())
    in
    let make_str_c k =
      if random prng
      then {| {"C":"None"} |}
      else (
        let start = {|{"C":{"Some":|} in
        let last = {|}}|} in
        let l =
          String.concat
            ","
            (List.init (Random.State.int prng 5) (fun i ->
               String.make (i + (k mod 256)) '0'))
        in
        start ^ l ^ last)
    in
    let make_str_d = {|"D"|} in
    (* making the payload *)
    let rec make size acc =
      if size = 0
      then acc
      else (
        match Random.State.int prng 4 with
        | 0 -> make (size - 1) (make_str_a () :: acc)
        | 1 -> make (size - 1) (make_str_b () :: acc)
        | 2 -> make (size - 1) (make_str_c size :: acc)
        | 3 -> make (size - 1) (make_str_d :: acc)
        | _ -> assert false)
    in
    let l = make size [] in
    "[" ^ String.concat "," l ^ "]"
  ;;

  let name = "list(union(…))"
end
