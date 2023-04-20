val binary_size_of : 'a Encoding.t -> 'a -> (Optint.Int63.t, string) result
val maximum_binary_size_of : 'a Encoding.t -> Optint.Int63.t
val equal_of : 'a Encoding.t -> 'a -> 'a -> bool
val pp_of : 'a Encoding.t -> Format.formatter -> 'a -> unit
