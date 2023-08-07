let deserialise read src =
  let state = Buffy.R.mk_state src in
  read state
;;

let run name encoding sizes buffer_size =
  let buffer = Bytes.make buffer_size '\x00' in
  List.iter
    (fun size ->
      let sources =
        Benchlib.src_seq_of_file (Benchlib.payload_file_name name size) buffer
      in
      let read = Binary_data_encoding.Reader.readk encoding in
      let _ = Benchlib.rr (deserialise read) sources in
      ())
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

let main sizes all_benches bench_kind buffer_size =
  let rec run_all benches =
    match benches with
    | [] -> ()
    | b :: bs ->
      let r = get_bench_name b in
      r sizes buffer_size;
      run_all bs
  in
  let benches = if all_benches then [ 0; 1; 2 ] else [ bench_kind ] in
  run_all benches
;;

let main_t =
  let open Benchlib in
  Cmdliner.Term.(const main $ sizes_t $ bench_all_t $ bench_t $ buffer_size_t)
;;

let main_cmd =
  let open Cmdliner in
  let doc = "Runs the binary writing benches repeatedly and prints report" in
  let man = [ `S Manpage.s_bugs; `P "Email bugs reports to ??" ] in
  let info = Cmd.info "binary writing bench" ~version:"0.1" ~doc ~man in
  Cmd.v info main_t
;;

let () = exit (Cmdliner.Cmd.eval main_cmd)
