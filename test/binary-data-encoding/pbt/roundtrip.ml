let testables =
  [ Pbtlib.mk Binary_data_encoding.Encoding.unit
  ; Pbtlib.mk Binary_data_encoding.Encoding.int64
  ; Pbtlib.mk Binary_data_encoding.Encoding.[ int64; option int32 ]
  ]
;;

let () = List.iter (fun t -> QCheck2.Test.check_exn (Pbtlib.to_test t)) testables
