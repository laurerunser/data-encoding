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
         (Seq.map (fun (Testable.AnyE (name, t)) -> Pbtlib.to_test name t) tests))
  in
  if exitcode <> 0 then exit exitcode
;;

let () = run Testable.basics

let not_so_basic =
  (* sampling the long long not-so-basic sequence because it's too long *)
  let rec skips s () =
    let n = Random.State.int seed_gen 128 in
    let s = Seq.drop (n + 1) s in
    match s () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (v, s) -> Seq.Cons (v, skips s)
  in
  skips Testable.not_so_basic
;;

let () = run not_so_basic
