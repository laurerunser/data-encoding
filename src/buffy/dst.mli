(** {1 Destinations }

    Destinations are an abstractions for things you can put bytes into. *)

(** A value of the type [t] is a destination. *)
type t

(** [available t] is the remaining available space (in number of bytes) in [t]. *)
val available : t -> int

(** [added t] is the number of bytes already added to [t]. *)
val added : t -> int

(** {2 Adders}

    [add_X t x] adds the binary representation of [x] to [t].

    @raise Invalid_argument if adding the binary representation would exceed the
    length of [t]. *)

(** [add_char] adds a character (one byte). *)
val add_char : t -> char -> unit

(** [add_uint8] adds an unsigned 8-bit integer (one byte).

    The behaviour is unspecified if the provided int is beyond the range of uint8.
    It may even raise an exception.

    @raise X May or may not raise an exception if the integer is out of bounds.
  *)
val add_uint8 : t -> int -> unit

(** [add_int8] adds a signed 8-bit integer (one byte).

    The behaviour is unspecified if the provided int is beyond the range of
    int8. It may even raise an exception.

    @raise X May or may not raise an exception if the integer is out of bounds.
  *)
val add_int8 : t -> int -> unit

(** [add_uint16_be] adds an unsigned 16-bit integer in big-endian representation
    (two bytes).

    The behaviour is unspecified if the provided int is beyond the range of
    uint16. It may even raise an exception.

    @raise X May or may not raise an exception if the integer is out of bounds.
  *)
val add_uint16_be : t -> int -> unit

(** [add_uint16_le] adds an unsigned 16-bit integer in little-endian representation
    (two bytes).

    The behaviour is unspecified if the provided int is beyond the range of
    uint16. It may even raise an exception.

    @raise X May or may not raise an exception if the integer is out of bounds.
  *)
val add_uint16_le : t -> int -> unit

(** [add_int16_be] adds a signed 16-bit integer in big-endian representation
    (two bytes).

    The behaviour is unspecified if the provided int is beyond the range of
    int16. It may even raise an exception.

    @raise X May or may not raise an exception if the integer is out of bounds.
  *)
val add_int16_be : t -> int -> unit

(** [add_int16_le] adds a signed 16-bit integer in little-endian representation
    (two bytes).

    The behaviour is unspecified if the provided int is beyond the range of
    int16. It may even raise an exception.

    @raise X May or may not raise an exception if the integer is out of bounds.
  *)
val add_int16_le : t -> int -> unit

(** [add_int32_be] adds a 32-bit integer in big-endian representation (four
    bytes). *)
val add_int32_be : t -> int32 -> unit

(** [add_int32_le] adds a 32-bit integer in little-endian representation (four
    bytes). *)
val add_int32_le : t -> int32 -> unit

(** [add_int64_be] adds a 64-bit integer in big-endian representation (eight
    bytes). *)
val add_int64_be : t -> int64 -> unit

(** [add_int64_be] adds a 64-bit integer in little-endian representation (eight
    bytes). *)
val add_int64_le : t -> int64 -> unit

(** [add_string] adds a full string. This is equivalent to setting each of the
    byte one after the other with increasing indices. *)
val add_string : t -> string -> unit

(** [add_string_slice] adds a slice of a string designated by an offset and a
    length.

    The behaviour is unspecified if the provided offset and length do not
    designate a valid slice of the string. It may even raise an exception.

    @raise X May or may not raise an exception if the offset and length is not
    valid for the string. *)
val add_string_slice : t -> string -> int -> int -> unit

(** [add_bytes] adds a full bytes. This is equivalent to setting each of the
    byte one after the other with increasing indices. *)
val add_bytes : t -> bytes -> unit

(** [add_bytes_slice] adds a slice of a bytes designated by an offset and a
    length.

    The behaviour is unspecified if the provided offset and length do not
    designate a valid slice of the bytes. It may even raise an exception.

    @raise X May or may not raise an exception if the offset and length is not
    valid for the bytes. *)
val add_bytes_slice : t -> bytes -> int -> int -> unit

(** {2 Makers}

    The functions of this section are used to produce destinations. *)

(** [of_bytes b] is a destination such that writing writes on the underlying
    bytes [b]. The number of available bytes in the destination starts as the
    length of [b], it decreases as values are added.

    @param [offset] (default: [0]) Adders set the content of the bytes starting
    at this offset. E.g., [add_char (of_bytes ~offset b) '1'] has the same
    effect on [b] as [Bytes.set b offset '1'].

    @param [length] (default: [Bytes.length b - offset]) Specifies the starting
    amount of available space.

    @raise Invalid_argument if [offset] and [length] do not designate a valid
    slice of [b]. *)
val of_bytes : ?offset:int -> ?length:int -> bytes -> t

(** {2 Contents}

    The functions of this section are used to extract information from a
    destination. *)

(** [to_string t] is a string with the content which has been added to [t]. *)
val to_string : t -> string

(** [blit_onto_bytes t soff b doff len] blits [len] bytes from [t] starting at
    offset [soff] onto [b] starting at offset [doff].

    @param [soff] is relative to the writable part of the destination.

    @raise [Invalid_argument] if the offsets or length are out of bounds. *)
val blit_onto_bytes : t -> int -> bytes -> int -> int -> unit

(** [bytes_of_dst t] is a triplet [(b,o,l)] such that the contents of [b]
    starting at offset [o] and of length [l] is the written part of [t] (i.e.,
    the part which contains bytes that have been added to [t]).

    Modifying the content of the destination or the returned bytes affects the
    other. *)
val bytes_of_dst : t -> bytes * int * int
