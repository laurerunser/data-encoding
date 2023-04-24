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

let run (module M : Benchlib.S) =
  List.iter (run M.name M.encoding.binary M.make_data) Benchlib.sizes
;;

let () = run (module Benchlib.Benchable0)
let () = run (module Benchlib.Benchable1)
