(** {1: Sources}

    Sources are an abstractions for things you can get bytes (and other
    low-level byte-like values) from. *)

(** A value of the type [t] is a source. *)
type t

val available : t -> int
val readed : t -> int

(** {2: Getters}

    [get_X t o] gets an [X] from [t] at offset [o].

    @raise Invalid_argument if [o < 0] or [o >= length t]

    @raise Invalid_argument if the reading goes beyond [length t] *)

(** [get_char] is a getter for characters. *)
val get_char : t -> char

(** [get_uint8] is a getter for unsigned 8-bit integers. *)
val get_uint8 : t -> int

(** [get_int8] is a getter for signed 8-bit integers. *)
val get_int8 : t -> int

(** [get_uint16_be] is a getter for unsigned 16-bit integers which reads a
    big-endian representation. *)
val get_uint16_be : t -> int

(** [get_uint16_le] is a getter for unsigned 16-bit integers which reads a
    little-endian representation. *)
val get_uint16_le : t -> int

(** [get_int16_be] is a getter for signed 16-bit integers which reads a
    big-endian representation. *)
val get_int16_be : t -> int

(** [get_int16_le] is a getter for signed 16-bit integers which reads a
    little-endian representation. *)
val get_int16_le : t -> int

(** [get_int32_be] is a getter for signed 32-bit integers which reads a
    big-endian representation. *)
val get_int32_be : t -> int32

(** [get_int32_le] is a getter for signed 32-bit integers which reads a
    little-endian representation. *)
val get_int32_le : t -> int32

(** [get_int64_be] is a getter for signed 64-bit integers which reads a
    big-endian representation. *)
val get_int64_be : t -> int64

(** [get_int64_le] is a getter for signed 64-bit integers which reads a
    little-endian representation. *)
val get_int64_le : t -> int64

(** [get_string] is a getter for string. The length of the string is determined
    by the additional parameter.

    See [blit_onto_bytes] which may be of interest. *)
val get_string : t -> int -> string

(** [get_blit_onto_bytes t b doff len] blits [len] bytes from [t] onto [b]
    starting at offset [doff].

    @raise [Invalid_argument] if the offsets or length are out of bounds. *)
val get_blit_onto_bytes : t -> bytes -> int -> int -> unit

(** {2: Makers}

    The functions of this section are used to produce sources. *)

(** [of_string s] is a source from which the bytes of [s] can be read.

    All getter and other indices are relative to the [offset]. For example,
    [get_char (of_string ~offset:x s) y] is the character [s.[x+y]]. The
    content prior to [offset] or beyond [offset+length] cannot be accessed.

    @param [offset] (default: [0])

    @param [length] (default: [String.length s - offset]) *)
val of_string : ?offset:int -> ?length:int -> string -> t

(** [of_bytes b] is a source from which the bytes of [b] can be read.

    {b Warning:} modifying [b] will modify the source. *)
val of_bytes : ?offset:int -> ?length:int -> bytes -> t

(** {2: Contents}

    The functions in this section are used to inspect the content of sources. *)

(** [to_string t] is the content of the allowed part of the underlying blob of
    [t], as a string. *)
val to_string : t -> string
