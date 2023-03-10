val zero_of_numeral : 'a Descr.numeral -> 'a
val max_int_of : 'a Descr.numeral -> Commons.Sizedints.Uint62.t
val size_of_numeral : 'a Descr.numeral -> Commons.Sizedints.Uint62.t
val numeral_of_int : 'a Descr.numeral -> int -> 'a
val int_of_numeral : 'a Descr.numeral -> 'a -> int
val size_of : ('s, 'a) Descr.t -> 'a -> (Optint.Int63.t, string) result
val maximum_size_of : ('s Sizability.intrinsic, 'a) Descr.t -> Optint.Int63.t
val equal_of : ('s, 'a) Descr.t -> 'a -> 'a -> bool
val pp_of : ('s, 'a) Descr.t -> Format.formatter -> 'a -> unit
val sizability : ('s, 'a) Descr.t -> 's
