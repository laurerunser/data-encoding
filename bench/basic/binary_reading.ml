let deserialise read src =
  let state = Buffy.R.mk_state src in
  read state
;;

let run name encoding sizes buffer_size repeats quiet =
  let log s = Benchlib.log s quiet in
  Format.kasprintf log "%s.of_string (%d samples)\n" name repeats;
  let buffer = Bytes.make buffer_size '\x00' in
  List.iter
    (fun size ->
      let sources =
        Benchlib.strs_seq_of_file (Benchlib.payload_file_name name size) buffer
      in
      let read = Binary_data_encoding.Reader.readk encoding in
      let deserialisations = Benchlib.measurer repeats (deserialise read) sources in
      let deserialisations = Benchlib.flatten deserialisations in
      Benchlib.print_summary size buffer_size deserialisations quiet)
    sizes
;;

let run (module M : Benchlib.S) = run M.name M.encoding.binary

let get_bench_name b =
  if b = 0
  then run (module Benchlib.Benchable0)
  else if b = 1
  then run (module Benchlib.Benchable1)
  else run (module Benchlib.Benchable2)
;;

let main sizes benches buffer_size repeats quiet =
  let rec run_all benches =
    match benches with
    | [] -> ()
    | b :: bs ->
      let r = get_bench_name b in
      r sizes buffer_size repeats quiet;
      run_all bs
  in
  run_all benches
;;

let main_t =
  let open Benchlib in
  Cmdliner.Term.(const main $ sizes_t $ bench_t $ buffer_size_t $ repeats_t $ quiet_t)
;;

let main_cmd =
  let open Cmdliner in
  let doc = "Runs the binary writing benches repeatedly and prints report" in
  let man = [ `S Manpage.s_bugs; `P "Email bugs reports to ??" ] in
  let info = Cmd.info "binary_reading_bench" ~version:"0.1" ~doc ~man in
  Cmd.v info main_t
;;

let () = exit (Cmdliner.Cmd.eval main_cmd)
