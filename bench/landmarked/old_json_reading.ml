(* let run name encoding =
  Format.kasprintf Benchlib.log "%s.of_string (%d samples)\n" name Benchlib.repeats;
  let buffer = Bytes.make Benchlib.json_buffer_size '\x00' in
  List.iter
    (fun size ->
      let sources =
        Benchlib.strs_seq_of_file (Benchlib.payload_file_name_json name size) buffer
      in
      let read = Json_data_encoding.Destruct.destruct encoding in
      let deserialisations = Benchlib.measurer3 Benchlib.repeats read sources in
      let deserialisations = Benchlib.flatten deserialisations in
      Benchlib.print_summary size Benchlib.buffer_size deserialisations)
    Benchlib.json_sizes
;;*)

let run name encoding =
  Format.kasprintf Benchlib.log "%s.of_string (%d samples)\n" name Benchlib.repeats;
  let buffer = Bytes.make Benchlib.buffer_size '\x00' in
  List.iter
    (fun size ->
      let sources =
        Benchlib.strs_seq_of_file (Benchlib.payload_file_name_json name size) buffer
      in
      let read = Json_data_encoding.Destruct.destruct encoding in
      let deserialisations = Benchlib.measurer3 Benchlib.repeats read sources in
      let deserialisations = Benchlib.flatten deserialisations in
      Benchlib.print_summary size Benchlib.buffer_size deserialisations)
    Benchlib.sizes
;;

(* let deserialise read src =
  let state = Buffy.Src.of_string src in
  read state
;;

let run name encoding =
  let buffer = Bytes.make Benchlib.buffer_size '\x00' in
  List.iter
    (fun size ->
      let sources =
        Benchlib.strs_seq_of_file (Benchlib.payload_file_name name size) buffer
      in
      let read = Json_data_encoding.Destruct_incremental.destruct_incremental encoding in
      let _ = Benchlib.rr (deserialise read) sources in
      ())
    Benchlib.sizes
;; *)

let run (module M : Benchlib.S) = run M.name M.encoding.json

(* let () = run (module Benchlib.Benchable0) *)
(* let () = run (module Benchlib.Benchable1) *)
let () = run (module Benchlib.Benchable2)