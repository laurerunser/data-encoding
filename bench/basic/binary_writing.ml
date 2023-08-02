let serialise write data dst =
  let state = Buffy.W.mk_state dst in
  write state data
;;

let run name encoding make_data sizes buffer_size repeats quiet =
  let log s = Benchlib.log s quiet in
  Format.kasprintf log "%s.to_string (%d samples)\n" name repeats;
  let buffer = Bytes.make buffer_size '0' in
  List.iter
    (fun size ->
      let data = make_data size in
      let write = Binary_data_encoding.Writer.writek encoding in
      let serialisations = Benchlib.measurew repeats (serialise write data) buffer in
      let serialisations = Benchlib.flatten serialisations in
      let size =
        Optint.Int63.to_int
        @@ Result.get_ok
        @@ Binary_data_encoding.Query.size_of encoding data
      in
      Benchlib.print_summary size buffer_size serialisations quiet)
    sizes
;;

let run (module M : Benchlib.S) = run M.name M.encoding.binary M.make_data

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
  let doc = "Runs the binary reading benches repeatedly and prints report" in
  let man = [ `S Manpage.s_bugs; `P "Email bugs reports to ??" ] in
  let info = Cmd.info "binary_writing_bench" ~version:"0.1" ~doc ~man in
  Cmd.v info main_t
;;

let () = exit (Cmdliner.Cmd.eval main_cmd)
