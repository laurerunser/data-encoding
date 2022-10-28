let set_int32 = Bytes.set_int32_be
let get_int32 = Bytes.get_int32_be
let get_int32_string s off = Bytes.get_int32_be (Bytes.unsafe_of_string s) off
let set_int8 = Bytes.set_int8
let get_int8 = Bytes.get_int8
let get_int8_string s off = Bytes.get_int8 (Bytes.unsafe_of_string s) off
let set_int16 = Bytes.set_int16_be
let get_int16 = Bytes.get_int16_be
let get_int16_string s off = Bytes.get_int16_be (Bytes.unsafe_of_string s) off
let set_int64 = Bytes.set_int64_be
let get_int64 = Bytes.get_int64_be
let get_int64_string s off = Bytes.get_int64_be (Bytes.unsafe_of_string s) off
let get_uint8 = Bytes.get_uint8
let get_uint8_string s off = Bytes.get_uint8 (Bytes.unsafe_of_string s) off
let get_uint16 = Bytes.get_uint16_be
let get_uint16_string s off = Bytes.get_uint16_be (Bytes.unsafe_of_string s) off
let get_double buff i = Int64.float_of_bits (Bytes.get_int64_be buff i)

let get_double_string buff i =
  Int64.float_of_bits (Bytes.get_int64_be (Bytes.unsafe_of_string buff) i)
;;

let set_double buff i v = Bytes.set_int64_be buff i (Int64.bits_of_float v)
