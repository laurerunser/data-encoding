(* TODO: documentation of @raise *)
(* TODO: documentation of string/bytes ownership and mutation discipline *)
(* TODO: more variants of read/write *)
(* TODO: separate the low-level backend functions ([*k]) from the high-level
   backend functions (all others) *)
type destination

val mk_destination : ?maximum_length:int -> bytes -> int -> int -> destination

type written =
  | Written of { destination : destination }
  | Failed of
      { destination : destination
      ; error : string
      }
  | Suspended of
      { destination : destination
      ; cont : bytes -> int -> int -> written
      }

val writek : destination -> 'a Encoding.t -> 'a -> written

val write
  :  dst:bytes
  -> offset:int
  -> length:int
  -> 'a Encoding.t
  -> 'a
  -> (int, int * string) result

val string_of : ?buffer_size:int -> 'a Encoding.t -> 'a -> (string, string) result

type source

val mk_source : ?maximum_length:int -> string -> int -> int -> source

type 'a readed =
  | Readed of
      { source : source
      ; value : 'a
      }
  | Failed of
      { source : source
      ; error : string
      }
  | Suspended of
      { source : source
      ; cont : string -> int -> int -> 'a readed
      }

val readk : source -> 'a Encoding.t -> 'a readed
val read : src:string -> offset:int -> length:int -> 'a Encoding.t -> ('a, string) result
