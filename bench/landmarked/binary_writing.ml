let serialise writek data dst =
  let state = Buffy.W.mk_state dst in
  writek state data
;;

let run encoding make_data sizes buffer_size =
  let buffer = Bytes.make buffer_size '0' in
  List.iter
    (fun size ->
      let data = make_data size in
      let writek = Binary_data_encoding.Writer.writek encoding in
      let _ = Benchlib.ww (serialise writek data) buffer in
      ())
    sizes
;;

let run (module M : Benchlib.S) sizes buffer_size =
  run M.encoding.binary M.make_data sizes buffer_size
;;

let get_bench_name b =
  if b = 0
  then run (module Benchlib.Benchable0)
  else if b = 1
  then run (module Benchlib.Benchable1)
  else run (module Benchlib.Benchable2)
;;

let main sizes all_bench bench_kind buffer_size =
  let rec run_all benches =
    match benches with
    | [] -> ()
    | b :: bs ->
      let r = get_bench_name b in
      r sizes buffer_size;
      run_all bs
  in
  let benches = if all_bench then [ 0; 1; 2 ] else [ bench_kind ] in
  run_all benches
;;

let main_t =
  let open Benchlib in
  Cmdliner.Term.(const main $ sizes_t $ bench_all_t $ bench_t $ buffer_size_t)
;;

let main_cmd =
  let open Cmdliner in
  let doc = "Runs the binary reading benches repeatedly and prints report" in
  let man = [] in
  let info = Cmd.info "binary reading bench" ~version:"0.1" ~doc ~man in
  Cmd.v info main_t
;;

let () = exit (Cmdliner.Cmd.eval main_cmd)
