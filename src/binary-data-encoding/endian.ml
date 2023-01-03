let get_double buff i = Int64.float_of_bits (Bytes.get_int64_be buff i)

let get_double_string buff i =
  Int64.float_of_bits (Bytes.get_int64_be (Bytes.unsafe_of_string buff) i)
;;

let get_int16 = Bytes.get_int16_be
let get_int16_string s off = Bytes.get_int16_be (Bytes.unsafe_of_string s) off
let get_int32 = Bytes.get_int32_be
let get_int32_string s off = Bytes.get_int32_be (Bytes.unsafe_of_string s) off
let get_int64 = Bytes.get_int64_be
let get_int64_string s off = Bytes.get_int64_be (Bytes.unsafe_of_string s) off
let get_int8 = Bytes.get_int8
let get_int8_string s off = Bytes.get_int8 (Bytes.unsafe_of_string s) off
let get_uint16 = Commons.Sizedints.Uint16.get_be

let get_uint16_string s off =
  Commons.Sizedints.Uint16.get_be (Bytes.unsafe_of_string s) off
;;

let get_uint30 = Commons.Sizedints.Uint30.get_be

let get_uint30_string s off =
  Commons.Sizedints.Uint30.get_be (Bytes.unsafe_of_string s) off
;;

let get_uint62 = Commons.Sizedints.Uint62.get_be
let get_uint62_string = Commons.Sizedints.Uint62.get_be_string
let get_uint8 = Commons.Sizedints.Uint8.get
let get_uint8_string s off = Commons.Sizedints.Uint8.get (Bytes.unsafe_of_string s) off
let set_double buff i v = Bytes.set_int64_be buff i (Int64.bits_of_float v)
let set_int16 = Bytes.set_int16_be
let set_int32 = Bytes.set_int32_be
let set_int64 = Bytes.set_int64_be
let set_int8 = Bytes.set_int8
let set_uint16 = Commons.Sizedints.Uint16.set_be
let set_uint30 = Commons.Sizedints.Uint30.set_be
let set_uint62 = Commons.Sizedints.Uint62.set_be
let set_uint8 = Commons.Sizedints.Uint8.set
