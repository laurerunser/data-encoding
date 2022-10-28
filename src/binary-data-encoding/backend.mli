val write
  :  dst:bytes
  -> offset:int
  -> maximum_length:int
  -> 'a Encoding.t
  -> 'a
  -> (int, int * string) result

val read
  :  src:string
  -> offset:int
  -> maximum_length:int
  -> 'a Encoding.t
  -> ('a, string) result
