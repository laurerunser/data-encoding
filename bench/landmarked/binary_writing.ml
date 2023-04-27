let serialise encoding data dst =
  let state = Buffy.W.mk_state dst in
  Binary_data_encoding.Writer.writek state encoding data
;;

let run encoding make_data =
  let buffer = Bytes.make Benchlib.buffer_size '0' in
  List.iter
    (fun size ->
      let data = make_data size in
      let _ = Benchlib.ww (serialise encoding data) buffer in
      ())
    Benchlib.sizes
;;

let run (module M : Benchlib.S) = run M.encoding.binary M.make_data
let () = run (module Benchlib.Benchable0)
let () = run (module Benchlib.Benchable1)
let () = run (module Benchlib.Benchable2)
