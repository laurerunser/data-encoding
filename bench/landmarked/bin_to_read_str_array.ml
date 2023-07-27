(* little binary that reads json from stdin and says if the json is a valid array of strings *)

open Json_data_encoding.Destruct_incremental

let rec run f =
  let str = read_line () in
  let res =
    if str = "end" then f (Buffy.Src.of_string "") else f (Buffy.Src.of_string str)
  in
  match res with
  | Ok _ -> print_string "valid json\n"
  | Error e ->
    print_string "invalid json";
    print_string e;
    print_newline ()
  | Await f ->
    print_string "waiting for more input\n";
    run f
;;

let () =
  print_string
    "Type your json input below. This programs reads an array of strings.\n\
     Type 'end' to signal the end of your input (it's probably invalid)\n";
  run (destruct_incremental (Seq String))
;;
