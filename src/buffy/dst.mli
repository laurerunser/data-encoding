(** {1: Destination } *)

type t

val length : t -> int
val set_char : t -> int -> char -> unit
val set_uint8 : t -> int -> int -> unit
val set_int8 : t -> int -> int -> unit
val set_uint16_be : t -> int -> int -> unit
val set_uint16_le : t -> int -> int -> unit
val set_int16_be : t -> int -> int -> unit
val set_int16_le : t -> int -> int -> unit
val set_int32_be : t -> int -> int32 -> unit
val set_int32_le : t -> int -> int32 -> unit
val set_int64_be : t -> int -> int64 -> unit
val set_int64_le : t -> int -> int64 -> unit
val set_string : t -> int -> string -> unit
val set_string_slice : t -> int -> string -> int -> int -> unit
val set_bytes : t -> int -> bytes -> unit
val set_bytes_slice : t -> int -> bytes -> int -> int -> unit

(* TODO: support more than just bytes *)
val of_bytes : ?offset:int -> ?length:int -> bytes -> t

(** warning: modifying either dsts modifies both *)
val of_dst : offset:int -> length:int -> t -> t

val to_string : t -> string
val blit_onto_bytes : t -> int -> bytes -> int -> int -> unit
