val size_of : 'a Descr.t -> 'a -> (Optint.Int63.t, string) result
val maximum_size_of : 'a Descr.t -> Optint.Int63.t
val equal_of : 'a Descr.t -> 'a -> 'a -> bool
val pp_of : 'a Descr.t -> Format.formatter -> 'a -> unit
