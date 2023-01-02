val get_int32 : bytes -> int -> int32
val get_int32_string : string -> int -> int32
val get_uint32 : bytes -> int -> Stdint.Uint32.t
val get_uint32_string : string -> int -> Stdint.Uint32.t
val set_int32 : bytes -> int -> int32 -> unit
val set_uint32 : bytes -> int -> Stdint.Uint32.t -> unit
val set_int8 : bytes -> int -> int -> unit
val set_uint8 : bytes -> int -> Stdint.Uint8.t -> unit
val get_int8 : bytes -> int -> int
val get_int8_string : string -> int -> int
val set_int16 : bytes -> int -> int -> unit
val set_uint16 : bytes -> int -> Stdint.Uint16.t -> unit
val get_int16 : bytes -> int -> int
val get_int16_string : string -> int -> int
val set_int64 : bytes -> int -> int64 -> unit
val set_uint64 : bytes -> int -> Stdint.Uint64.t -> unit
val get_int64 : bytes -> int -> int64
val get_int64_string : string -> int -> int64
val get_uint64 : bytes -> int -> Stdint.Uint64.t
val get_uint64_string : string -> int -> Stdint.Uint64.t
val get_uint8 : bytes -> int -> Stdint.Uint8.t
val get_uint8_string : string -> int -> Stdint.Uint8.t
val get_uint16 : bytes -> int -> Stdint.Uint16.t
val get_uint16_string : string -> int -> Stdint.Uint16.t
val set_double : bytes -> int -> float -> unit
val get_double : bytes -> int -> float
val get_double_string : string -> int -> float
