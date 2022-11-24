let data =
  let open Data_encoding.Encoding.Hlist in
  [ 0L; 4L; [ 16L; () ]; 0xff_ff_ff_ff_ffL ]
;;

let encoding =
  let open Data_encoding.Encoding in
  tuple [ int64; int64; obj [("foo", int64); ("bar", unit)]; int64 ]
;;

let buffer = Bytes.create 1024

let to_string e v =
  match
    Binary_data_encoding.Backend.write
      ~dst:buffer
      ~offset:0
      ~maximum_length:1024
      (Data_encoding.Encoding.to_binary e)
      v
  with
  | Error _ -> exit 1
  | Ok l -> Bytes.sub_string buffer 0 l
;;

let () =
  let s = to_string encoding data in
  let (`Hex s) = Hex.of_string s in
  print_endline s;
;;

let to_json e v =
  match
    Json_data_encoding.Backend.construct
      (Data_encoding.Encoding.to_json e)
      v
  with
  | Error _ -> exit 1
  | Ok j -> j
;;

let () =
  let j = to_json encoding data in
  Format.printf "%a\n" Json_data_encoding.PP.mini j;
;;

let () =
  exit 0
;;
