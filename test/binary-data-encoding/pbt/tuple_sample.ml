let () = Printf.printf "Testing a sample of tuple encodings\n"

let seed_gen =
  match Sys.getenv_opt "PBT_ROUNDTRIP_SEED" with
  | None ->
    let prng = Random.State.make_self_init () in
    let seed = Random.State.int prng (1 lsl 29) in
    Printf.printf "Seed picked randomly: PBT_ROUNDTRIP_SEED=%d\n%!" seed;
    Random.State.make [| seed |]
  | Some s ->
    (match int_of_string_opt s with
     | None ->
       Printf.eprintf "Malformed PBT_ROUNDTRIP_SEED";
       exit 1
     | Some seed -> Random.State.make [| seed |])
;;

let () = flush_all ()

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

let rec skips rate s () =
  let s = Seq.drop rate s in
  match s () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons (v, s) -> Seq.Cons (v, skips rate s)
;;

let base =
  let open Binary_data_encoding_test_pbt.Testable in
  let all = all_ground_encodings in
  let all = Seq.concat (List.to_seq [ all; sequences all; either all all ]) in
  all
;;

let tuple_encodings =
  let open Binary_data_encoding_test_pbt.Testable in
  tuples (List.to_seq [ skips 10 base; skips 10 base; skips 10 base ])
;;

let tuple_encoding_sample =
  (* sampling the long long not-so-basic sequence because it's too long *)
  let offset = Random.State.int seed_gen 32 in
  let rate = 100 in
  skips rate (Seq.drop offset tuple_encodings)
;;

let () = run tuple_encoding_sample
