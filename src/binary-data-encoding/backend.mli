val write
  :  dst:bytes
  -> offset:int
  -> length:int
  -> 'a Encoding.t
  -> 'a
  -> (int, int * string) result

val string_of : ?buffer_size:int -> 'a Encoding.t -> 'a -> (string, string) result
val read : src:string -> offset:int -> length:int -> 'a Encoding.t -> ('a, string) result
