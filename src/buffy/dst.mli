(** {1: Destinations }

    Destinations are an abstractions for things you can send bytes (and other
    low-level byte-like values) to. *)

(** A value of the type [t] is a destination. *)
type t

(** [length t] is the available space in [t]. *)
val length : t -> int

(** {2: Setters}

    [set_X t o x] sets the contents of [t] at offset [o] to be the binary representation of [x].

    @raise Invalid_argument if [o < 0]

    @raise Invalid_argument if [o + w >= length t] is the width (in bytes) of [x]. *)

(** [set_char] sets a character *)
val set_char : t -> int -> char -> unit

(** [set_uint8] sets an unsigned 8-bit integer.

    The behaviour is unspecified if the provided int is beyond the range of uint8.
    It may even raise an exception.

    @raise X May or may not raise an exception if the integer is out of bounds.
  *)
val set_uint8 : t -> int -> int -> unit

(** [set_int8] sets a signed 8-bit integer.

    The behaviour is unspecified if the provided int is beyond the range of
    int8. It may even raise an exception.

    @raise X May or may not raise an exception if the integer is out of bounds.
  *)
val set_int8 : t -> int -> int -> unit

(** [set_uint16_be] sets an unsigned 16-bit integer in big-endian representation.

    The behaviour is unspecified if the provided int is beyond the range of
    uint16. It may even raise an exception.

    @raise X May or may not raise an exception if the integer is out of bounds.
  *)
val set_uint16_be : t -> int -> int -> unit

(** [set_uint16_le] sets an unsigned 16-bit integer in little-endian representation.

    The behaviour is unspecified if the provided int is beyond the range of
    uint16. It may even raise an exception.

    @raise X May or may not raise an exception if the integer is out of bounds.
  *)
val set_uint16_le : t -> int -> int -> unit

(** [set_int16_be] sets a signed 16-bit integer in big-endian representation.

    The behaviour is unspecified if the provided int is beyond the range of
    int16. It may even raise an exception.

    @raise X May or may not raise an exception if the integer is out of bounds.
  *)
val set_int16_be : t -> int -> int -> unit

(** [set_int16_le] sets a signed 16-bit integer in little-endian representation.

    The behaviour is unspecified if the provided int is beyond the range of
    int16. It may even raise an exception.

    @raise X May or may not raise an exception if the integer is out of bounds.
  *)
val set_int16_le : t -> int -> int -> unit

(** [set_int32_be] sets a 32-bit integer in big-endian representation. *)
val set_int32_be : t -> int -> int32 -> unit

(** [set_int32_le] sets a 32-bit integer in little-endian representation. *)
val set_int32_le : t -> int -> int32 -> unit

(** [set_int64_be] sets a 64-bit integer in big-endian representation. *)
val set_int64_be : t -> int -> int64 -> unit

(** [set_int64_be] sets a 64-bit integer in little-endian representation. *)
val set_int64_le : t -> int -> int64 -> unit

(** [set_string] sets a full string. This is equivalent to setting each of the
    byte one after the other with increasing indices. *)
val set_string : t -> int -> string -> unit

(** [set_string_slice] sets a slice of a string designated by an offset and a
    length.

    The behaviour is unspecified if the provided offset and length do not
    designate a valid slice of the string. It may even raise an exception.

    @raise X May or may not raise an exception if the offset and length is not
    valid for the string. *)
val set_string_slice : t -> int -> string -> int -> int -> unit

(** [set_bytes] sets a full bytes. This is equivalent to setting each of the
    byte one after the other with increasing indices. *)
val set_bytes : t -> int -> bytes -> unit

(** [set_bytes_slice] sets a slice of a bytes designated by an offset and a
    length.

    The behaviour is unspecified if the provided offset and length do not
    designate a valid slice of the bytes. It may even raise an exception.

    @raise X May or may not raise an exception if the offset and length is not
    valid for the bytes. *)
val set_bytes_slice : t -> int -> bytes -> int -> int -> unit

(** {2: Makers}

    The functions of this section are used to produce destinations. *)

(** [of_bytes b] is a destination such that writing writes on the underlying
    bytes [b].

    All setter and other indices are relative to the [offset]. For example,
    [set_char (of_bytes ~offset:x b) y '1'] has the same effect as
    [Bytes.set b (x+y) '1']. The content prior to [offset] or beyond
    [offset+length] cannot be altered.

    @param [offset] (default: [0])

    @param [length] (default: [Bytes.length b - offset]) *)
val of_bytes : ?offset:int -> ?length:int -> bytes -> t

(** [of_dst ~offset ~length t] is a new destination with a narrower span of
    valid indices. Writing on the returned destination is equivalent to writing
    to the parameter destination, up to offset translation and length limits. *)
val of_dst : offset:int -> length:int -> t -> t

(** {2: Contents}

    The functions of this section are used to extract information from a
    destination. *)

(** [to_string t] is the content of the allowed part of the underlying buffer of
    [t] as a string. *)
val to_string : t -> string

(** [blit_onto_bytes t soff b doff len] blits [len] bytes from [t] starting at
    offset [soff] onto [b] starting at offset [doff].

    @parameter [soff] is relative to the writable part of the destination.

    @raise [Invalid_argument] if the offsets or length are out of bounds. *)
val blit_onto_bytes : t -> int -> bytes -> int -> int -> unit

(** [bytes_of_dst t] is a triplet [(b,o,l)] such that the contents of [b]
    starting at offset [o] and of length [l] is the writable part of [t].

    Modifying the content of the destination or the returned bytes may affect
    the other. *)
val bytes_of_dst : t -> bytes * int * int
