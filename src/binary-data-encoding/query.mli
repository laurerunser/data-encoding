val zero_of_numeral : 'a Descr.numeral -> 'a
val max_int_of : 'a Descr.numeral -> Commons.Sizedints.Uint62.t
val size_of_numeral : 'a Descr.numeral -> Optint.Int63.t
val numeral_of_int : 'a Descr.numeral -> int -> 'a
val int_of_numeral : 'a Descr.numeral -> 'a -> int
val size_of : 'a Descr.t -> 'a -> (Optint.Int63.t, string) result
val maximum_size_of : 'a Descr.t -> Optint.Int63.t
val equal_of : 'a Descr.t -> 'a -> 'a -> bool
val pp_of : 'a Descr.t -> Format.formatter -> 'a -> unit
