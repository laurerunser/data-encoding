module Uint30 = struct
  type t = int

  let min_int = 0
  let max_int = 0x40_00_00_00
  let of_int v = if min_int <= v && v <= max_int then Some v else None
  let set_be b o v = Bytes.set_int32_be b o (Int32.of_int v)

  let get_be b o =
    let i32 = Bytes.get_int32_be b o in
    let i = Int32.to_int i32 in
    if i < 0 then 0 else if i > max_int then max_int else i
  ;;
end

module Uint16 = struct
  type t = int

  let min_int = 0
  let max_int = 0xff_ff
  let of_int v = if min_int <= v && v <= max_int then Some v else None
  let to_uint30 v = v
  let set_be = Bytes.set_uint16_be
  let get_be = Bytes.get_uint16_be
end

module Uint8 = struct
  type t = int

  let min_int = 0
  let zero = 0
  let one = 1
  let max_int = 0xff
  let of_int v = if min_int <= v && v <= max_int then Some v else None
  let to_uint30 v = v
  let set = Bytes.set_uint8
  let get = Bytes.get_uint8
end
