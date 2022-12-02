let testables =
  [ Pbtlib.mk Json_data_encoding.Encoding.unit
  ; Pbtlib.mk Json_data_encoding.Encoding.int64
  ; Pbtlib.mk Json_data_encoding.Encoding.string
  ; Pbtlib.mk Json_data_encoding.Encoding.(tuple [ int64; string; string ])
  ; Pbtlib.mk Json_data_encoding.Encoding.(obj [ req "foo" int64; opt "bar" string ])
  ]
;;

let () = List.iter (fun t -> QCheck2.Test.check_exn (Pbtlib.to_test t)) testables
