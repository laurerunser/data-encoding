(** {1: Sources}

    This module is for abstracting read-only blobs of bytes of different types. *)

type t

val length : t -> int
val get : t -> int -> char
val get_uint8 : t -> int -> int
val get_int8 : t -> int -> int
val get_uint16_be : t -> int -> int
val get_uint16_le : t -> int -> int
val get_int16_be : t -> int -> int
val get_int16_le : t -> int -> int
val get_int32_be : t -> int -> int32
val get_int32_le : t -> int -> int32
val get_int64_be : t -> int -> int64
val get_int64_le : t -> int -> int64
val get_string : t -> int -> int -> string
val blit_onto_bytes : t -> int -> bytes -> int -> int -> unit

(* TODO: support more than just strings *)
val of_string : ?offset:int -> ?length:int -> string -> t
val of_src : offset:int -> length:int -> t -> t

(** warning: modifying the underlying bytes will modify the source *)
val of_bytes : ?offset:int -> ?length:int -> bytes -> t

val to_string : t -> string
