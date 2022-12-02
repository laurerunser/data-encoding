type any_encoding = AnyE : _ Json_data_encoding.Encoding.t -> any_encoding

let testables =
  [ AnyE Json_data_encoding.Encoding.unit
  ; AnyE Json_data_encoding.Encoding.int64
  ; AnyE Json_data_encoding.Encoding.string
  ; AnyE Json_data_encoding.Encoding.(tuple [ int64; string; string ])
  ; AnyE Json_data_encoding.Encoding.(obj [ req "foo" int64; opt "bar" string ])
  ; AnyE
      Json_data_encoding.Encoding.(
        conv ~serialisation:Int64.succ ~deserialisation:(fun v -> Ok (Int64.pred v)) int64)
  ]
;;

let () = List.iter (fun (AnyE t) -> QCheck2.Test.check_exn (Pbtlib.to_test t)) testables
