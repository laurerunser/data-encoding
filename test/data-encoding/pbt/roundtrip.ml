type any_encoding = AnyE : _ Data_encoding.Encoding.t -> any_encoding

let testables =
  [ AnyE Data_encoding.Encoding.unit
  ; AnyE Data_encoding.Encoding.int64
  ; AnyE Data_encoding.Encoding.string
  ; AnyE Data_encoding.Encoding.bytes
  ; AnyE Data_encoding.Encoding.(tuple [ int64; string; string ])
  ; AnyE Data_encoding.Encoding.(obj [ req "foo" int64; opt "bar" string ])
  ]
;;

let () =
  List.iter
    (fun (AnyE t) ->
      QCheck2.Test.check_exn (Pbtlib.to_test_binary t);
      QCheck2.Test.check_exn (Pbtlib.to_test_json t))
    testables
;;
