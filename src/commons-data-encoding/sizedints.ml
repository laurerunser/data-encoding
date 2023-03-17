module Uint62 = struct
  type t = Optint.Int63.t

  let mul =
    (* TODO: catch overflow *)
    Optint.Int63.mul
  ;;

  let add =
    (* TODO: catch overflow *)
    Optint.Int63.add
  ;;

  let min_int = Optint.Int63.zero
  let zero = Optint.Int63.zero
  let max_int = Optint.Int63.max_int
  let max_intL = Optint.Int63.(to_int64 max_int)

  let of_int64 v =
    if Int64.compare 0L v <= 0 && Int64.compare v max_intL <= 0
    then Some (Optint.Int63.of_int64 v)
    else None
  ;;

  let of_int v = if 0 <= v then Some (Optint.Int63.of_int v) else None

  (* TODO: [of_int] but make it 32-bit architecture compatible (no literals that
   are too big) *)

  let set_be b o v =
    (* TODO: optimise based on arch *)
    let i64 = Optint.Int63.to_int64 v in
    Bytes.set_int64_be b o i64
  ;;

  let set_le b o v =
    (* TODO: optimise based on arch *)
    let i64 = Optint.Int63.to_int64 v in
    Bytes.set_int64_le b o i64
  ;;

  let get_be b o =
    (* TODO: optimise based on arch *)
    let i64 = String.get_int64_be b o in
    if Int64.compare i64 0L < 0
    then min_int
    else if Int64.compare max_intL i64 < 0
    then max_int
    else Optint.Int63.of_int64 i64
  ;;

  let get_le b o =
    (* TODO: optimise based on arch *)
    let i64 = String.get_int64_le b o in
    if Int64.compare i64 0L < 0
    then min_int
    else if Int64.compare max_intL i64 < 0
    then max_int
    else Optint.Int63.of_int64 i64
  ;;
end

module Uint30 = struct
  type t = int

  let min_int = 0
  let zero = 0
  let max_int = 0x3f_ff_ff_ff
  let of_int v = if min_int <= v && v <= max_int then Some v else None
  let to_uint62 v = Optint.Int63.of_int v
  let set_be b o v = Bytes.set_int32_be b o (Int32.of_int (v :> int))
  let set_le b o v = Bytes.set_int32_le b o (Int32.of_int (v :> int))

  let get_be b o =
    let i32 = String.get_int32_be b o in
    let i = Int32.to_int i32 in
    if i < 0 then 0 else if i > max_int then max_int else i
  ;;

  let get_le b o =
    let i32 = String.get_int32_le b o in
    let i = Int32.to_int i32 in
    if i < 0 then 0 else if i > max_int then max_int else i
  ;;
end

module Uint16 = struct
  type t = int

  let min_int = 0
  let zero = 0
  let max_int = 0xff_ff
  let of_int v = if min_int <= v && v <= max_int then Some v else None
  let to_uint62 v = Optint.Int63.of_int v
  let set_be = Bytes.set_uint16_be
  let set_le = Bytes.set_uint16_le
  let get_be = String.get_uint16_be
  let get_le = String.get_uint16_le
end

module Uint8 = struct
  type t = int

  let min_int = 0
  let zero = 0
  let one = 1
  let max_int = 0xff
  let of_int v = if min_int <= v && v <= max_int then Some v else None
  let to_uint62 v = Optint.Int63.of_int v
  let get = String.get_uint8
end
