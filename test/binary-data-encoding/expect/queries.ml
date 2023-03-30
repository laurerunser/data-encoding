(* Some of these tests are easier to host outside of the library's source code
   because of inter-module dependencies. *)

open Binary_data_encoding

let%expect_test _ =
  let w : type a. a Encoding.t -> a -> unit =
   fun e v ->
    match Query.size_of e v with
    | Ok s -> Format.printf "%a\n" Optint.Int63.pp s
    | Error msg -> Format.printf "Error: %s\n" msg
  in
  w Encoding.unit ();
  [%expect {| 0 |}];
  w Encoding.int64 0x00L;
  [%expect {| 8 |}];
  w Encoding.int64 0xffL;
  [%expect {| 8 |}];
  w Encoding.(tuple [ unit; unit; int32; unit; int32 ]) [ (); (); 0x00l; (); 0x00l ];
  [%expect {| 8 |}];
  w Encoding.(tuple [ string `Uint30; unit ]) [ "FOO"; () ];
  [%expect {| 7 |}];
  w Encoding.(tuple [ string `Uint16; unit ]) [ "FOO"; () ];
  [%expect {| 5 |}];
  w Encoding.(tuple [ string `Uint8; unit ]) [ "FOO"; () ];
  [%expect {| 4 |}];
  w
    Encoding.(
      tuple [ string (`Fixed (Option.get @@ Sizedints.Uint62.of_int64 3L)); unit ])
    [ "FOO"; () ];
  [%expect {| 3 |}];
  w Encoding.(option int32) None;
  [%expect {| 1 |}];
  w Encoding.(option int32) (Some 0xff_ffl);
  [%expect {| 5 |}];
  ()
;;

let%expect_test _ =
  let w : type a. a Encoding.t -> unit =
   fun e ->
    match Query.sizability e with
    | S Extrinsic -> Format.printf "Extrinsics don't have max_size"
    | S (Intrinsic _) -> Format.printf "%a\n" Optint.Int63.pp (Query.maximum_size_of e)
  in
  w Encoding.unit;
  [%expect {| 0 |}];
  w Encoding.int64;
  [%expect {| 8 |}];
  w Encoding.(tuple [ unit; unit; int32; unit; int32 ]);
  [%expect {| 8 |}];
  w Encoding.(option int32);
  [%expect {| 5 |}];
  ()
;;
