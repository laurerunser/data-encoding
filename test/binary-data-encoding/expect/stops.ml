(* Some of these tests are easier to host outside of the library's source code
   because of inter-module dependencies. *)

open Binary_data_encoding

let%expect_test _ =
  let print_stops fmt (state : Buffy.R.state) =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.pp_print_char fmt '-')
      Format.pp_print_int
      fmt
      state.stop_hints
  in
  let w : type a. int -> int -> a Encoding.t -> a -> unit =
   fun initread morereads e v ->
    let (E descr) = Binary_data_encoding.Encoding.Advanced_low_level.introspect e in
    match Writer.string_of descr v with
    | Error _ -> assert false
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
      (match Reader.readk state descr with
       | Failed { error; state } ->
         Format.printf
           "Error: %S, Readed: %d, Stops: %a\n"
           error
           state.readed
           print_stops
           state
       | Readed { value; state } ->
         Format.printf "Ok, Readed: %d, Stops: %a\n" state.readed print_stops state;
         assert (Query.equal_of descr v value)
       | Suspended { cont; state } ->
         Format.printf "Suspended, Readed: %d, Stops: %a\n" state.readed print_stops state;
         let rec go offset (cont : Buffy.Src.t -> a Buffy.R.readed) =
           let length = min (String.length blob - offset) morereads in
           let source = Buffy.Src.of_string blob ~offset ~length in
           match cont source with
           | Failed { error; state } ->
             Format.printf
               "Error: %S, Readed: %d, Stops: %a\n"
               error
               state.readed
               print_stops
               state
           | Readed { value; state } ->
             Format.printf "Ok, Readed: %d, Stops: %a\n" state.readed print_stops state;
             assert (Query.equal_of descr v value)
           | Suspended { cont; state } ->
             Format.printf
               "Suspended, Readed: %d, Stops: %a\n"
               state.readed
               print_stops
               state;
             go (offset + length) cont
         in
         go initread cont)
  in
  (* a complex encoding with some nesting structure *)
  let encoding =
    let open Encoding in
    let base = With_size.seq_with_size `UInt8 uint16 in
    let tup = With_size.seq_with_size `UInt8 (tuple [ base; base ]) in
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
    Blob: 000000020602dead02beef0602feed02beef
    Ok, Readed: 18, Stops: |}];
  (* somewhat friendly cuts *)
  w 10 6 encoding v;
  [%expect
    {|
    Blob: 000000020602dead02beef0602feed02beef
    Suspended, Readed: 10, Stops: 11-11
    Error: "expected-stop point exceeded", Readed: 1, Stops: 1-1 |}];
  (* unfriendly cuts *)
  w 6 5 encoding v;
  [%expect
    {|
    Blob: 000000020602dead02beef0602feed02beef
    Suspended, Readed: 6, Stops: 8-11
    Error: "expected-stop point exceeded", Readed: 2, Stops: 2-5 |}];
  (* a more complex test mixing in strings which use chunkread *)
  let encoding =
    let open Encoding in
    let tup =
      With_size.seq_with_size
        `UInt8
        (tuple [ With_size.seq_with_size `UInt8 uint16; string `UInt8; string `UInt8 ])
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
    Suspended, Readed: 10, Stops: 17
    Suspended, Readed: 6, Stops: 7
    Error: "expected-stop point exceeded", Readed: 1, Stops: 1 |}];
  w 6 5 encoding v;
  [%expect
    {|
    Blob: 000000020c02dead02eeee05ffffffffff0902feed02eeee02eeee
    Suspended, Readed: 6, Stops: 8-17
    Error: "expected-stop point exceeded", Readed: 2, Stops: 2-11 |}];
  w 3 5 encoding v;
  [%expect
    {|
    Blob: 000000020c02dead02eeee05ffffffffff0902feed02eeee02eeee
    Suspended, Readed: 3, Stops:
    Suspended, Readed: 5, Stops: 14
    Suspended, Readed: 5, Stops: 9
    Error: "expected-stop point exceeded", Readed: 4, Stops: 4 |}];
  ()
;;
