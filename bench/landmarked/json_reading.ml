let run name encoding =
  let buffer = Bytes.make Benchlib.json_buffer_size '\x00' in
  List.iter
    (fun size ->
      Format.printf "File: %s \n" (Benchlib.payload_file_name_json name size);
      let sources =
        Benchlib.src_seq_of_file (Benchlib.payload_file_name_json name size) buffer
      in
      let read = Json_data_encoding.Destruct_incremental.destruct_incremental encoding in
      (* let sources = Seq.fold_left (fun acc a -> acc ^ Buffy.Src.to_string a) "" sources in
      let sources = Seq.cons (Buffy.Src.of_string sources) Seq.empty in *)
      let _ = Benchlib.rr_json read sources in
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
