type _ t =
  | [] : unit t
  | ( :: ) : 'a * 'b t -> ('a * 'b) t

type (_, _) nth =
  | Hd : ('a * 'b, 'a) nth
  | Tl : ('b, 'c) nth -> ('a * 'b, 'c) nth

val nth : 'a t -> ('a, 'b) nth -> 'b
