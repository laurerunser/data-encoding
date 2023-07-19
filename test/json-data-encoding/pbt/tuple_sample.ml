let () = Printf.printf "Testing a sample of product encodings (JSON)\n"

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
            (fun (Json_data_encoding_test_pbt.Testable.AnyE (name, t)) ->
              Json_data_encoding_test_pbt.Pbtlib.to_test name t)
            tests))
  in
  if exitcode <> 0 then exit exitcode
;;

let tuples =
  let open Json_data_encoding_test_pbt.Testable in
  tuples (List.to_seq [ all_ground_encodings; all_ground_encodings ])
;;

let objs =
  let open Json_data_encoding_test_pbt.Testable in
  objs (List.to_seq [ all_ground_encodings; all_ground_encodings ])
;;

let rec skips rate s () =
  let s = Seq.drop rate s in
  match s () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons (v, s) -> Seq.Cons (v, skips rate s)
;;

let sample rate s = skips rate (Seq.drop (Random.int rate) s)
let () = run (sample 100 tuples)
let () = run (sample 100 objs)