let int64 = 8
let int32 = 4
let uint62 = 8
let uint30 = 4
let uint16 = 2
let uint8 = 1
let bool = 1
let unit = 0

module As_uint62 = struct
  let int64 = Option.get @@ Commons.Sizedints.Uint62.of_int int64
  let int32 = Option.get @@ Commons.Sizedints.Uint62.of_int int32
  let uint62 = Option.get @@ Commons.Sizedints.Uint62.of_int uint62
  let uint30 = Option.get @@ Commons.Sizedints.Uint62.of_int uint30
  let uint16 = Option.get @@ Commons.Sizedints.Uint62.of_int uint16
  let uint8 = Option.get @@ Commons.Sizedints.Uint62.of_int uint8
  let bool = Option.get @@ Commons.Sizedints.Uint62.of_int bool
  let unit = Option.get @@ Commons.Sizedints.Uint62.of_int unit
end
