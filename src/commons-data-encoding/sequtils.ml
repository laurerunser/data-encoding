let rec append1 seq stop () =
  match seq () with
  | Seq.Nil -> Seq.Cons (stop, Seq.empty)
  | Seq.Cons (x, seq) -> Seq.Cons (x, append1 seq stop)
;;

let bracket start seq stop () = Seq.Cons (start, append1 seq stop)
