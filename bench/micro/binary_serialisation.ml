let int64_twople =
  let open Data_encoding.Encoding in
  to_binary (tuple [ int64; int64 ])
;;

let int64_triple =
  let open Data_encoding.Encoding in
  to_binary (tuple [ int64; int64; int64 ])
;;

let int64_quadle =
  let open Data_encoding.Encoding in
  to_binary (tuple [ int64; int64; int64; int64 ])
;;

let offset = 0
let length = 8 * 4
let dst = Bytes.make length 'x'

let twople ij =
  Core_bench.Bench.Test.create ~name:"twople" (fun () ->
    let _ = Binary_data_encoding.Writer.write ~dst ~offset ~length int64_twople ij in
    ())
;;

let triple ijk =
  Core_bench.Bench.Test.create ~name:"triple" (fun () ->
    let _ = Binary_data_encoding.Writer.write ~dst ~offset ~length int64_triple ijk in
    ())
;;

let quadle ijkl =
  Core_bench.Bench.Test.create ~name:"quadle" (fun () ->
    let _ = Binary_data_encoding.Writer.write ~dst ~offset ~length int64_quadle ijkl in
    ())
;;

let main () =
  Random.self_init ();
  let i64 = Random.int64 1024L in
  Command_unix.run
    (Core_bench.Bench.make_command
       [ twople Commons.Hlist.[ i64; i64 ]
       ; triple Commons.Hlist.[ i64; i64; i64 ]
       ; quadle Commons.Hlist.[ i64; i64; i64; i64 ]
       ])
;;

let () = main ()
