let serialise writek data dst =
  let state = Buffy.W.mk_state dst in
  writek state data
;;

let run encoding make_data =
  let buffer = Bytes.make Benchlib.buffer_size '0' in
  List.iter
    (fun size ->
      let data = make_data size in
      let writek = Binary_data_encoding.Writer.writek encoding in
      let _ = Benchlib.ww (serialise writek data) buffer in
      ())
    Benchlib.sizes
;;

let run (module M : Benchlib.S) = run M.encoding.binary M.make_data
let () = run (module Benchlib.Benchable0)
let () = run (module Benchlib.Benchable1)
let () = run (module Benchlib.Benchable2)
