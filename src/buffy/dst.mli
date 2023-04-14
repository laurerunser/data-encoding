(** {1: Destinations }

    Destinations are an abstractions for things you can send bytes (and other
    low-level byte-like values) to. *)

(** A value of the type [t] is a destination. *)
type t

(** [available t] is the remaining available space in [t]. *)
val available : t -> int

(** [added t] is the number of bytes already added to [t]. *)
val added : t -> int

(** {2: Setters}

    [add_X t x] adds the binary representation of [x] to [t].

    @raise Invalid_argument if adding the binary representation would exceed the
    length of [t]. *)

(** [add_char] sets a character *)
val add_char : t -> char -> unit

(** [add_uint8] sets an unsigned 8-bit integer.

    The behaviour is unspecified if the provided int is beyond the range of uint8.
    It may even raise an exception.

    @raise X May or may not raise an exception if the integer is out of bounds.
  *)
val add_uint8 : t -> int -> unit

(** [add_int8] sets a signed 8-bit integer.

    The behaviour is unspecified if the provided int is beyond the range of
    int8. It may even raise an exception.

    @raise X May or may not raise an exception if the integer is out of bounds.
  *)
val add_int8 : t -> int -> unit

(** [add_uint16_be] sets an unsigned 16-bit integer in big-endian representation.

    The behaviour is unspecified if the provided int is beyond the range of
    uint16. It may even raise an exception.

    @raise X May or may not raise an exception if the integer is out of bounds.
  *)
val add_uint16_be : t -> int -> unit

(** [add_uint16_le] sets an unsigned 16-bit integer in little-endian representation.

    The behaviour is unspecified if the provided int is beyond the range of
    uint16. It may even raise an exception.

    @raise X May or may not raise an exception if the integer is out of bounds.
  *)
val add_uint16_le : t -> int -> unit

(** [add_int16_be] sets a signed 16-bit integer in big-endian representation.

    The behaviour is unspecified if the provided int is beyond the range of
    int16. It may even raise an exception.

    @raise X May or may not raise an exception if the integer is out of bounds.
  *)
val add_int16_be : t -> int -> unit

(** [add_int16_le] sets a signed 16-bit integer in little-endian representation.

    The behaviour is unspecified if the provided int is beyond the range of
    int16. It may even raise an exception.

    @raise X May or may not raise an exception if the integer is out of bounds.
  *)
val add_int16_le : t -> int -> unit

(** [add_int32_be] sets a 32-bit integer in big-endian representation. *)
val add_int32_be : t -> int32 -> unit

(** [add_int32_le] sets a 32-bit integer in little-endian representation. *)
val add_int32_le : t -> int32 -> unit

(** [add_int64_be] sets a 64-bit integer in big-endian representation. *)
val add_int64_be : t -> int64 -> unit

(** [add_int64_be] sets a 64-bit integer in little-endian representation. *)
val add_int64_le : t -> int64 -> unit

(** [add_string] sets a full string. This is equivalent to setting each of the
    byte one after the other with increasing indices. *)
val add_string : t -> string -> unit

(** [add_string_slice] sets a slice of a string designated by an offset and a
    length.

    The behaviour is unspecified if the provided offset and length do not
    designate a valid slice of the string. It may even raise an exception.

    @raise X May or may not raise an exception if the offset and length is not
    valid for the string. *)
val add_string_slice : t -> string -> int -> int -> unit

(** [add_bytes] sets a full bytes. This is equivalent to setting each of the
    byte one after the other with increasing indices. *)
val add_bytes : t -> bytes -> unit

(** [add_bytes_slice] sets a slice of a bytes designated by an offset and a
    length.

    The behaviour is unspecified if the provided offset and length do not
    designate a valid slice of the bytes. It may even raise an exception.

    @raise X May or may not raise an exception if the offset and length is not
    valid for the bytes. *)
val add_bytes_slice : t -> bytes -> int -> int -> unit

(** {2: Makers}

    The functions of this section are used to produce destinations. *)

(** [of_bytes b] is a destination such that writing writes on the underlying
    bytes [b].

    All setter and other indices are relative to the [offset]. For example,
    [add_char (of_bytes ~offset:x b) '1'] has the same effect as
    [Bytes.set b x '1']. The content prior to [offset] or beyond
    [offset+length] cannot be altered.

    @param [offset] (default: [0])

    @param [length] (default: [Bytes.length b - offset]) *)
val of_bytes : ?offset:int -> ?length:int -> bytes -> t

(** {2: Contents}

    The functions of this section are used to extract information from a
    destination. *)

(** [to_string t] is a string with the content which has been added to [t]. *)
val to_string : t -> string

(** [blit_onto_bytes t soff b doff len] blits [len] bytes from [t] starting at
    offset [soff] onto [b] starting at offset [doff].

    @parameter [soff] is relative to the writable part of the destination.

    @raise [Invalid_argument] if the offsets or length are out of bounds. *)
val blit_onto_bytes : t -> int -> bytes -> int -> int -> unit

(** [bytes_of_dst t] is a triplet [(b,o,l)] such that the contents of [b]
    starting at offset [o] and of length [l] is the written part of [t] (i.e.,
    the part which contains bytes that have been added to [t]).

    Modifying the content of the destination or the returned bytes affects the
    other. *)
val bytes_of_dst : t -> bytes * int * int
