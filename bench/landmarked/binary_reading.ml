let deserialise read src =
  let state = Buffy.R.mk_state src in
  read state
;;

let run name encoding =
  let buffer = Bytes.make Benchlib.buffer_size '\x00' in
  List.iter
    (fun size ->
      let sources =
        Benchlib.src_seq_of_file (Benchlib.payload_file_name name size) buffer
      in
      let read state = Binary_data_encoding.Reader.readk state encoding in
      let _ = Benchlib.rr (deserialise read) sources in
      ())
    Benchlib.sizes
;;

let run (module M : Benchlib.S) = run M.name M.encoding.binary
let () = run (module Benchlib.Benchable0)
let () = run (module Benchlib.Benchable1)
let () = run (module Benchlib.Benchable2)
