(* TODO: documentation of @raise *)
(* TODO: documentation of string/bytes ownership and mutation discipline *)
(* TODO: more variants of read *)

val readk : Buffy.R.state -> ('s, 'a) Descr.t -> 'a Buffy.R.readed

val read
  :  src:string
  -> offset:int
  -> length:int
  -> ('s, 'a) Descr.t
  -> ('a, string) result

val read_string : string -> ('s, 'a) Descr.t -> ('a, string) result
val read_strings : (string * int * int) Seq.t -> ('s, 'a) Descr.t -> ('a, string) result
