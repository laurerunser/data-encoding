type any_encoding = AnyE : string * _ Binary_data_encoding.Encoding.t -> any_encoding

let testables =
  [ AnyE ("unit", Binary_data_encoding.Encoding.unit)
  ; AnyE ("i64", Binary_data_encoding.Encoding.int64)
  ; AnyE ("i64,opt(i32)", Binary_data_encoding.Encoding.[ int64; option int32 ])
  ]
;;

let () =
  List.iter
    (fun (AnyE (name, t)) -> QCheck2.Test.check_exn (Pbtlib.to_test name t))
    testables
;;
