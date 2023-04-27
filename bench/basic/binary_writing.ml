let serialise write data dst =
  let state = Buffy.W.mk_state dst in
  write state data
;;

let run name encoding make_data =
  Format.kasprintf Benchlib.log "%s.to_string (%d samples)\n" name Benchlib.repeats;
  let buffer = Bytes.make Benchlib.buffer_size '0' in
  List.iter
    (fun size ->
      let data = make_data size in
      let write = Binary_data_encoding.Writer.writek encoding in
      let serialisations =
        Benchlib.measurew Benchlib.repeats (serialise write data) buffer
      in
      let serialisations = Benchlib.flatten serialisations in
      let size =
        Optint.Int63.to_int
        @@ Result.get_ok
        @@ Binary_data_encoding.Query.size_of encoding data
      in
      Benchlib.print_summary size Benchlib.buffer_size serialisations)
    Benchlib.sizes
;;

let run (module M : Benchlib.S) = run M.name M.encoding.binary M.make_data
let () = run (module Benchlib.Benchable0)
let () = run (module Benchlib.Benchable1)
let () = run (module Benchlib.Benchable2)
