let binary_size_of encoding v =
  Binary_data_encoding.Query.size_of encoding.Encoding.binary v
;;

let maximum_binary_size_of encoding =
  Binary_data_encoding.Query.maximum_size_of encoding.Encoding.binary
;;

let equal_of encoding = Binary_data_encoding.Query.equal_of encoding.Encoding.binary
let pp_of encoding = Binary_data_encoding.Query.pp_of encoding.Encoding.binary
