(* Some of these tests are easier to host outside of the library's source code
   because of inter-module dependencies. *)

open Binary_data_encoding

let%expect_test _ =
  let print_limits fmt (state : Buffy.R.state) =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.pp_print_char fmt '-')
      Format.pp_print_int
      fmt
      state.size_limits
  in
  let w : type a. int -> int -> a Encoding.t -> a -> unit =
   fun initread morereads e v ->
    match Writer.string_of e v with
    | Error error -> Format.printf "Error whilst serialising: %S\n" error
    | Ok blob ->
      Format.printf
        "Blob: %a\n"
        (Format.pp_print_seq
           ~pp_sep:(fun _ () -> ())
           (fun fmt c -> Format.fprintf fmt "%02x" c))
        (String.to_seq blob |> Seq.map Char.code);
      let initread = min initread (String.length blob) in
      let state =
        Buffy.R.mk_state (Buffy.Src.of_string blob ~offset:0 ~length:initread)
      in
      (match Reader.readk state e with
       | Failed { error; state } ->
         Format.printf
           "Error: %S, Readed: %d, Limits: %a\n"
           error
           state.readed
           print_limits
           state
       | Readed { value; state } ->
         Format.printf "Ok, Readed: %d, Limits: %a\n" state.readed print_limits state;
         assert (Query.equal_of e v value)
       | Suspended { cont; state } ->
         Format.printf
           "Suspended, Readed: %d, Limits: %a\n"
           state.readed
           print_limits
           state;
         let rec go offset (cont : Buffy.Src.t -> a Buffy.R.readed) =
           let length = min (String.length blob - offset) morereads in
           let source = Buffy.Src.of_string blob ~offset ~length in
           match cont source with
           | Failed { error; state } ->
             Format.printf
               "Error: %S, Readed: %d, Limits: %a\n"
               error
               state.readed
               print_limits
               state
           | Readed { value; state } ->
             Format.printf "Ok, Readed: %d, Limits: %a\n" state.readed print_limits state;
             assert (Query.equal_of e v value)
           | Suspended { cont; state } ->
             Format.printf
               "Suspended, Readed: %d, Limits: %a\n"
               state.readed
               print_limits
               state;
             go (offset + length) cont
         in
         go initread cont)
  in
  (* a complex encoding with some nesting structure *)
  let encoding =
    let open Encoding in
    let base = with_size_limit 56 (seq `UInt8 uint16) in
    let tup = with_size_limit 32 (seq `UInt8 (tuple [ base; base ])) in
    let arr = array `UInt30 tup in
    arr
  in
  let uint16 n = Seq.return (Option.get @@ Encoding.Sizedints.Uint16.of_int n) in
  let v =
    let open Encoding.Hlist in
    [| Seq.return [ uint16 0xdead; uint16 0xbeef ]
     ; Seq.return [ uint16 0xfeed; uint16 0xbeef ]
    |]
  in
  (* full all-in-one read (with [-1] to ensure crash otherwise) *)
  w 18 (-1) encoding v;
  [%expect
    {|
    Blob: 000000020101dead01beef0101feed01beef
    Ok, Readed: 18, Limits: |}];
  (* somewhat friendly cuts *)
  w 10 6 encoding v;
  [%expect
    {|
    Blob: 000000020101dead01beef0101feed01beef
    Suspended, Readed: 10, Limits: 36
    Suspended, Readed: 6, Limits: 33
    Ok, Readed: 2, Limits: |}];
  (* unfriendly cuts *)
  w 6 5 encoding v;
  [%expect
    {|
    Blob: 000000020101dead01beef0101feed01beef
    Suspended, Readed: 6, Limits: 36
    Suspended, Readed: 5, Limits: 37
    Suspended, Readed: 5, Limits: 32
    Ok, Readed: 2, Limits: |}];
  (* a more complex test mixing in strings which use chunkread *)
  let encoding =
    let open Encoding in
    let tup =
      with_size_limit
        128
        (With_size.seq_with_size
           `UInt8
           (with_size_limit
              64
              (tuple
                 [ With_size.seq_with_size `UInt8 uint16; string `UInt8; string `UInt8 ])))
    in
    array `UInt30 tup
  in
  let v =
    let open Encoding.Hlist in
    [| Seq.return [ uint16 0xdead; "\xee\xee"; "\xff\xff\xff\xff\xff" ]
     ; Seq.return [ uint16 0xfeed; "\xee\xee"; "\xee\xee" ]
    |]
  in
  w 10 6 encoding v;
  [%expect
    {|
    Blob: 000000020c02dead02eeee05ffffffffff0902feed02eeee02eeee
    Suspended, Readed: 10, Limits: 132
    Suspended, Readed: 6, Limits: 122
    Suspended, Readed: 6, Limits: 129
    Ok, Readed: 5, Limits: |}];
  w 6 5 encoding v;
  [%expect
    {|
    Blob: 000000020c02dead02eeee05ffffffffff0902feed02eeee02eeee
    Suspended, Readed: 6, Limits: 132
    Suspended, Readed: 5, Limits: 126
    Suspended, Readed: 5, Limits: 121
    Suspended, Readed: 5, Limits: 129
    Suspended, Readed: 5, Limits: 124
    Ok, Readed: 1, Limits: |}];
  w 3 5 encoding v;
  [%expect
    {|
    Blob: 000000020c02dead02eeee05ffffffffff0902feed02eeee02eeee
    Suspended, Readed: 3, Limits:
    Suspended, Readed: 5, Limits: 129
    Suspended, Readed: 5, Limits: 124
    Suspended, Readed: 5, Limits: 132
    Suspended, Readed: 5, Limits: 127
    Ok, Readed: 4, Limits: |}];
  (* a more complex test with some size-limits being too small *)
  let encoding =
    let open Encoding in
    let tup =
      with_size_limit
        8
        (tuple [ With_size.seq_with_size `UInt8 uint16; string `UInt8; string `UInt8 ])
    in
    let seq = with_size_limit 128 (With_size.seq_with_size `UInt8 tup) in
    array `UInt30 seq
  in
  w 10 6 encoding [||];
  [%expect {|
    Blob: 00000000
    Ok, Readed: 4, Limits: |}];
  w 3 5 encoding [||];
  [%expect
    {|
    Blob: 00000000
    Suspended, Readed: 3, Limits:
    Ok, Readed: 1, Limits: |}];
  let v =
    let open Encoding.Hlist in
    [| Seq.return [ uint16 0xdead; "\xee\xee"; "\xff\xff\xff\xff\xff" ]
     ; Seq.return [ uint16 0xfeed; "\xee\xee"; "\xee\xee" ]
    |]
  in
  w 10 6 encoding v;
  [%expect {| Error whilst serialising: "size exceeds limit" |}];
  w 6 5 encoding v;
  [%expect {| Error whilst serialising: "size exceeds limit" |}];
  w 3 5 encoding v;
  [%expect {| Error whilst serialising: "size exceeds limit" |}];
  let encoding =
    let open Encoding in
    let tup =
      with_size_limit
        1024
        (tuple
           [ With_size.seq_with_size `UInt8 uint16
           ; with_size_limit 10 (string `UInt8)
           ; string `UInt8
           ])
    in
    let seq = with_size_limit 64 (With_size.seq_with_size `UInt8 tup) in
    array `UInt30 seq
  in
  let v sz =
    let open Encoding.Hlist in
    [| Seq.return [ uint16 0xdead; String.make sz '\xee'; "\xff\xff\xff\xff\xff" ]
     ; Seq.return [ uint16 0xfeed; "\xee\xee"; "\xee\xee" ]
    |]
  in
  w 10 6 encoding (v 2);
  [%expect
    {|
    Blob: 000000020c02dead02eeee05ffffffffff0902feed02eeee02eeee
    Suspended, Readed: 10, Limits: 68
    Suspended, Readed: 6, Limits: 58
    Suspended, Readed: 6, Limits: 65
    Ok, Readed: 5, Limits: |}];
  w 10 6 encoding (v 9);
  [%expect
    {|
    Blob: 000000021302dead09eeeeeeeeeeeeeeeeee05ffffffffff0902feed02eeee02eeee
    Suspended, Readed: 10, Limits: 18-68
    Suspended, Readed: 6, Limits: 8-58
    Suspended, Readed: 6, Limits: 52
    Suspended, Readed: 6, Limits: 66
    Ok, Readed: 6, Limits: |}];
  w 10 6 encoding (v 11);
  [%expect {|
    Error whilst serialising: "size exceeds limit" |}];
  ()
;;
