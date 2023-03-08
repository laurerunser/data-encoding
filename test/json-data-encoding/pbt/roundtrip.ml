let seed_gen =
  match Sys.getenv_opt "PBT_ROUNDTRIP_SEED" with
  | None ->
    let prng = Random.State.make_self_init () in
    let seed = Random.State.int prng (1 lsl 29) in
    Printf.printf "Seed picked randomly: PBT_ROUNDTRIP_SEED=%d\n" seed;
    Random.State.make [| seed |]
  | Some s ->
    (match int_of_string_opt s with
     | None ->
       Printf.eprintf "Malformed PBT_ROUNDTRIP_SEED";
       exit 1
     | Some seed -> Random.State.make [| seed |])
;;

type any_encoding = AnyE : _ Json_data_encoding.Encoding.t -> any_encoding

let testables =
  [ AnyE Json_data_encoding.Encoding.unit
  ; AnyE Json_data_encoding.Encoding.null
  ; AnyE Json_data_encoding.Encoding.bool
  ; AnyE Json_data_encoding.Encoding.int64
  ; AnyE Json_data_encoding.Encoding.string
  ; AnyE Json_data_encoding.Encoding.(seq int64)
  ; AnyE Json_data_encoding.Encoding.(list bool)
  ; AnyE Json_data_encoding.Encoding.(array string)
  ; AnyE Json_data_encoding.Encoding.(tuple [ int64; string; string ])
  ; AnyE Json_data_encoding.Encoding.(obj [ req "foo" int64; opt "bar" string ])
  ; AnyE
      Json_data_encoding.Encoding.(
        conv ~serialisation:Int64.succ ~deserialisation:(fun v -> Ok (Int64.pred v)) int64)
  ; AnyE Json_data_encoding.Encoding.(Union.either bool string)
  ; AnyE Json_data_encoding.Encoding.(Union.either unit unit)
  ; AnyE Json_data_encoding.Encoding.(array (Union.either int64 (seq bool)))
  ]
;;

let run tests =
  let rand = Random.State.make [| Random.State.int seed_gen (1 lsl 29) |] in
  let exitcode =
    QCheck_runner.run_tests ~rand (List.map (fun (AnyE t) -> Pbtlib.to_test t) tests)
  in
  if exitcode <> 0 then exit exitcode
;;

let () = run testables
