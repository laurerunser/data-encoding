type any_e = Any : _ Data_encoding.Encoding.t -> any_e
type any_b = Anyb : _ Binary_data_encoding.Encoding.t -> any_b
type any_j = Anyj : _ Json_data_encoding.Encoding.t -> any_j

let inputs : (string * any_e) list =
  let open Data_encoding.Encoding in
  [ "unit", Any unit; "int64", Any int64; "int64*int64", Any (tuple [ int64; int64 ]) ]
;;

let commands =
  List.map
    (fun (name, Any encoding) ->
      Core_bench.Bench.Test.create ~name:("binary_of__" ^ name) (fun () ->
        Anyb (Data_encoding.Encoding.to_binary encoding)))
    inputs
  @ List.map
      (fun (name, Any encoding) ->
        Core_bench.Bench.Test.create ~name:("json_of__" ^ name) (fun () ->
          Anyj (Data_encoding.Encoding.to_json encoding)))
      inputs
;;

let main () =
  Random.self_init ();
  Command_unix.run (Core_bench.Bench.make_command commands)
;;

let () = main ()
