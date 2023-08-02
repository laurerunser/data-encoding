open Cmdliner
open Benchlib

let run name encoding make_data size =
  let blob =
    Result.get_ok @@ Binary_data_encoding.Writer.string_of encoding (make_data size)
  in
  let fname = Benchlib.payload_file_name name size in
  let oc = open_out fname in
  output_string oc blob;
  close_out oc;
  ()
;;

let run (module M : Benchlib.S) sizes =
  List.iter (run M.name M.encoding.binary M.make_data) sizes
;;

let get_bench_name b =
  if b = 0
  then run (module Benchlib.Benchable0)
  else if b = 1
  then run (module Benchlib.Benchable1)
  else run (module Benchlib.Benchable2)
;;

let main sizes benches =
  let rec run_all benches =
    match benches with
    | [] -> ()
    | b :: bs ->
      let r = get_bench_name b in
      r sizes;
      run_all bs
  in
  run_all benches
;;

let main_t = Term.(const main $ sizes_t $ bench_t)

let main_cmd =
  let open Cmdliner in
  let doc = "Generates the payloads for the binary benches" in
  let man = [ `S Manpage.s_bugs; `P "Email bugs reports to ??" ] in
  let info = Cmd.info "make_binary_reading_payload" ~version:"0.1" ~doc ~man in
  Cmd.v info main_t
;;

let () = exit (Cmdliner.Cmd.eval main_cmd)
