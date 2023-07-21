type 'a t

val make : unit -> 'a t

type (_, _) eq = Eq : ('a, 'a) eq

val eq : 'a t -> 'b t -> ('a, 'b) eq option
