let run name encoding =
  Format.kasprintf Benchlib.log "MORE DETAILED NEW PARSER %s\n" name;
  let buffer = Bytes.make Benchlib.json_buffer_size '\x00' in
  List.iter
    (fun size ->
      Format.printf "File: %s \n" (Benchlib.payload_file_name_json name size);
      let sources =
        Benchlib.strs_seq_of_file (Benchlib.payload_file_name_json name size) buffer
      in
      let read = Json_data_encoding.Destruct_incremental.destruct_incremental encoding in
      let serialisations = Benchlib.measurer2 Benchlib.repeats read sources in
      let serialisations = Benchlib.flatten serialisations in
      Benchlib.print_summary size Benchlib.buffer_size serialisations;
      ())
    Benchlib.json_sizes
;;

let run (module M : Benchlib.S) = run M.name M.encoding.json

let () =
  print_string "Incremental Json reading\n";
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
