let () = Printf.printf "Testing all simple encodings\n"

let seed_gen =
  match Sys.getenv_opt "PBT_ROUNDTRIP_SEED" with
  | None ->
    let prng = Random.State.make_self_init () in
    let seed = Random.State.int prng (1 lsl 29) in
    Printf.printf "Seed picked randomly: PBT_ROUNDTRIP_SEED=%d\n" seed;
    Random.State.make [| seed |]
  | Some s ->
    (match int_of_string_opt s with
     | None ->
       Printf.eprintf "Malformed PBT_ROUNDTRIP_SEED";
       exit 1
     | Some seed -> Random.State.make [| seed |])
;;

let run tests =
  let rand = Random.State.make [| Random.State.int seed_gen (1 lsl 29) |] in
  let exitcode =
    QCheck_runner.run_tests
      ~rand
      (List.of_seq
         (Seq.map
            (fun (Binary_data_encoding_test_pbt.Testable.AnyE (name, t)) ->
              Binary_data_encoding_test_pbt.Pbtlib.to_test name t)
            tests))
  in
  if exitcode <> 0 then exit exitcode
;;

let all =
  let open Binary_data_encoding_test_pbt.Testable in
  let all = all_ground_encodings in
  let all = Seq.append all str_encodings in
  let all =
    Seq.concat
      (List.to_seq [ all; simple_combinators all; sequences all; either all all ])
  in
  all
;;

let () = run all
