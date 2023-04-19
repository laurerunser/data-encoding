(* Some of these tests are easier to host outside of the library's source code
   because of inter-module dependencies. *)

open Binary_data_encoding

let%expect_test _ =
  let w : type a. a Encoding.t -> a -> unit =
   fun e v ->
    Format.printf "Ok: %a\n%!" (Query.pp_of e) v;
    match Writer.string_of e v with
    | Error error -> Format.printf "Error: %s\n%!" error
    | Ok blob ->
      Format.printf
        "Ok: %a\n%!"
        (Format.pp_print_seq
           ~pp_sep:(fun _ () -> ())
           (fun fmt c -> Format.fprintf fmt "%02x" c))
        (String.to_seq blob |> Seq.map Char.code);
      (match Reader.read_string blob e with
       | Error error -> Format.printf "Error: %s\n%!" error
       | Ok vv ->
         Format.printf "Ok: %a\n%!" (Query.pp_of e) vv;
         if not (Query.equal_of e v vv)
         then (
           Format.printf "Not equal!\n";
           raise Exit))
  in
  w Encoding.bool true;
  [%expect {|
    Ok: true
    Ok: ff
    Ok: true |}];
  w Encoding.bool false;
  [%expect {|
    Ok: false
    Ok: 00
    Ok: false |}];
  w (Encoding.string (`Fixed (Option.get (Commons.Sizedints.Uint62.of_int 3)))) "abc";
  [%expect {|
    Ok: "abc"
    Ok: 616263
    Ok: "abc" |}];
  w Encoding.int64 0L;
  [%expect {|
    Ok: 0
    Ok: 0000000000000000
    Ok: 0 |}];
  w Encoding.int64 0x11223344L;
  [%expect {|
    Ok: 287454020
    Ok: 0000000011223344
    Ok: 287454020 |}];
  w Encoding.int64 0xdeadbeefdeadbeefL;
  [%expect
    {|
    Ok: -2401053088876216593
    Ok: deadbeefdeadbeef
    Ok: -2401053088876216593 |}];
  w Encoding.(either bool int64) (Either.Right 0xdeadbeefdeadbeefL);
  [%expect
    {|
    Ok: case(false:-2401053088876216593)
    Ok: 00deadbeefdeadbeef
    Ok: case(false:-2401053088876216593) |}];
  w Encoding.(either bool int64) (Either.Left false);
  [%expect {|
    Ok: case(true:false)
    Ok: ff00
    Ok: case(true:false) |}];
  w Encoding.(seq `Uint16 int32) (Array.to_seq [| 0l; 0l |]);
  [%expect
    {|
    Ok: conved(seq(0,0))
    Ok: 00020000000000000000
    Ok: conved(seq(0,0)) |}];
  w
    Encoding.(either uint30 bool)
    (Either.Left (Option.get (Commons.Sizedints.Uint30.of_int32 0xfeel)));
  [%expect {|
    Ok: case(true:4078)
    Ok: ff00000fee
    Ok: case(true:4078) |}];
  w Encoding.(either uint30 bool) (Either.Right false);
  [%expect {|
    Ok: case(false:false)
    Ok: 0000
    Ok: case(false:false) |}];
  w
    Encoding.(
      tuple
        [ seq `Uint16 int32
        ; either uint30 bool
        ; either Little_endian.uint8 Little_endian.uint16
        ])
    [ Array.to_seq [| 0l; 0l |]
    ; Either.Left Commons.Sizedints.Uint30.zero
    ; Either.Left Commons.Sizedints.Uint8.zero
    ];
  [%expect
    {|
    Ok: conved(seq(0,0));case(true:0);case(true:0)
    Ok: 00020000000000000000ff00000000ff00
    Ok: conved(seq(0,0));case(true:0);case(true:0) |}];
  w
    Encoding.(
      tuple
        [ With_size.array `Uint16 int32
        ; either uint30 bool
        ; either Little_endian.uint8 Little_endian.uint16
        ])
    [ [| 0l; 0l |]
    ; Either.Left Commons.Sizedints.Uint30.zero
    ; Either.Left Commons.Sizedints.Uint8.zero
    ];
  [%expect
    {|
    Ok: conved(seq(0,0));case(true:0);case(true:0)
    Ok: 00080000000000000000ff00000000ff00
    Ok: conved(seq(0,0));case(true:0);case(true:0) |}];
  ()
;;
