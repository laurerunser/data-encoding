let testables =
  [ Pbtlib.mk Data_encoding.Encoding.unit
  ; Pbtlib.mk Data_encoding.Encoding.int64
  ; Pbtlib.mk Data_encoding.Encoding.string
  ; Pbtlib.mk Data_encoding.Encoding.bytes
  ; Pbtlib.mk Data_encoding.Encoding.(tuple [ int64; string; string ])
  ; Pbtlib.mk Data_encoding.Encoding.(obj [ req "foo" int64; opt "bar" string ])
  ]
;;

let () =
  List.iter
    (fun t ->
      QCheck2.Test.check_exn (Pbtlib.to_test_binary t);
      QCheck2.Test.check_exn (Pbtlib.to_test_json t))
    testables
;;
