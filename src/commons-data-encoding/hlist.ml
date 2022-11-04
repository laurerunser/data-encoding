type _ t =
  | [] : unit t
  | ( :: ) : 'a * 'b t -> ('a * 'b) t

type (_, _) nth =
  | Hd : ('a * 'b, 'a) nth
  | Tl : ('b, 'c) nth -> ('a * 'b, 'c) nth

let rec nth : type a b. a t -> (a, b) nth -> b =
 fun t n ->
  match t, n with
  | x :: _, Hd -> x
  | _ :: xs, Tl n -> nth xs n
  | _ -> .
;;
