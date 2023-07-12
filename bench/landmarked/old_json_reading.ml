let run name encoding =
  let buffer = Bytes.make Benchlib.json_buffer_size '\x00' in
  List.iter
    (fun size ->
      if size < 5000
      then Format.printf "File: %s \n" (Benchlib.payload_file_name_json name size);
      let sources =
        Benchlib.src_seq_of_file (Benchlib.payload_file_name_json name size) buffer
      in
      let read = Json_data_encoding.Destruct.destruct encoding in
      let source = Seq.fold_left (fun acc a -> acc ^ Buffy.Src.to_string a) "" sources in
      let source = Ezjsonm.from_string source in
      let _ =
        match
          read (source : Json_data_encoding.JSON.compat :> Json_data_encoding.JSON.flex)
        with
        | Ok value -> Ok value
        | Error error ->
          Format.printf "\tError: %s\n" error;
          Error error
      in
      ())
    Benchlib.json_sizes
;;

let run (module M : Benchlib.S) = run M.name M.encoding.json

let () =
  print_string "Old Json reading\n";
  print_string "Benchable0\n";
  run (module Benchlib.Benchable0)
;;

let () =
  print_string "\n\nBenchable1\n";
  run (module Benchlib.Benchable1)
;;

let () =
  print_string "\n\nBenchable2\n";
  run (module Benchlib.Benchable2)
;;
