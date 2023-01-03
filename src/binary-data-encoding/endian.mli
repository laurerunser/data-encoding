val get_int32 : bytes -> int -> int32
val get_int32_string : string -> int -> int32
val get_uint30 : bytes -> int -> Commons.Sizedints.Uint30.t
val get_uint30_string : string -> int -> Commons.Sizedints.Uint30.t
val set_int32 : bytes -> int -> int32 -> unit
val set_uint30 : bytes -> int -> Commons.Sizedints.Uint30.t -> unit
val set_int8 : bytes -> int -> int -> unit
val set_uint8 : bytes -> int -> Commons.Sizedints.Uint8.t -> unit
val get_int8 : bytes -> int -> int
val get_int8_string : string -> int -> int
val set_int16 : bytes -> int -> int -> unit
val set_uint16 : bytes -> int -> Commons.Sizedints.Uint16.t -> unit
val get_int16 : bytes -> int -> int
val get_int16_string : string -> int -> int
val set_int64 : bytes -> int -> int64 -> unit
val get_int64 : bytes -> int -> int64
val get_int64_string : string -> int -> int64
val get_uint8 : bytes -> int -> Commons.Sizedints.Uint8.t
val get_uint8_string : string -> int -> Commons.Sizedints.Uint8.t
val get_uint16 : bytes -> int -> Commons.Sizedints.Uint16.t
val get_uint16_string : string -> int -> Commons.Sizedints.Uint16.t
val set_double : bytes -> int -> float -> unit
val get_double : bytes -> int -> float
val get_double_string : string -> int -> float
