let run name make_json_string size =
  (* let fname = Benchlib.payload_file_name_json name size in
  let oc = open_out fname in
  make_json_string size oc;
  close_out oc;
  ()
;; *)
  let blob = make_json_string size in
  let fname = Benchlib.payload_file_name_json name size in
  let oc = open_out fname in
  output_string oc blob;
  close_out oc;
  ()
;;

let run (module M : Benchlib.S) = List.iter (run M.name M.make_json_string) Benchlib.sizes
let () = run (module Benchlib.Benchable0)
let () = run (module Benchlib.Benchable1)
let () = run (module Benchlib.Benchable2)
