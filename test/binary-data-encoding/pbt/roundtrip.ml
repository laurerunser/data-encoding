let () =
  Seq.iter
    (fun (Testable.AnyE (name, t)) -> QCheck2.Test.check_exn (Pbtlib.to_test name t))
    Testable.basics
;;

let () =
  (* sampling the long long not-so-basic sequence because it's too long *)
  let prng = Random.State.make [| 111; 1111; 11111 |] in
  let rec skips s () =
    let n = Random.State.int prng 64 in
    let s = Seq.drop n s in
    match s () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (v, s) -> Seq.Cons (v, skips s)
  in
  let not_so_basic = skips Testable.not_so_basic in
  Seq.iter
    (fun (Testable.AnyE (name, t)) -> QCheck2.Test.check_exn (Pbtlib.to_test name t))
    not_so_basic
;;
