(* Some of these tests are easier to host outside of the library's source code
   because of inter-module dependencies. *)

open Binary_data_encoding

let%expect_test _ =
  let print_stops fmt (source : Buffy.R.source) =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.pp_print_char fmt '-')
      Format.pp_print_int
      fmt
      source.stop_at_readed
  in
  let w : type a. int -> int -> a Encoding.t -> a -> unit =
   fun initread morereads e v ->
    match Writer.string_of e v with
    | Error _ -> assert false
    | Ok blob ->
      Format.printf
        "Blob: %a\n"
        (Format.pp_print_seq
           ~pp_sep:(fun _ () -> ())
           (fun fmt c -> Format.fprintf fmt "%02x" c))
        (String.to_seq blob |> Seq.map Char.code);
      let initread = min initread (String.length blob) in
      let source = Buffy.R.mk_source blob 0 initread in
      (match Reader.readk source e with
      | Failed { error; source } ->
        Format.printf
          "Error: %S, Readed: %d, Stops: %a\n"
          error
          source.readed
          print_stops
          source
      | Readed { value; source } ->
        Format.printf "Ok, Readed: %d, Stops: %a\n" source.readed print_stops source;
        assert (Query.equal_of e v value)
      | Suspended { cont; source } ->
        Format.printf
          "Suspended, Readed: %d, Stops: %a\n"
          source.readed
          print_stops
          source;
        let rec go offset (cont : string -> int -> int -> a Buffy.R.readed) =
          let length = min (String.length blob - offset) morereads in
          match cont blob offset length with
          | Failed { error; source } ->
            Format.printf
              "Error: %S, Readed: %d, Stops: %a\n"
              error
              source.readed
              print_stops
              source
          | Readed { value; source } ->
            Format.printf "Ok, Readed: %d, Stops: %a\n" source.readed print_stops source;
            assert (Query.equal_of e v value)
          | Suspended { cont; source } ->
            Format.printf
              "Suspended, Readed: %d, Stops: %a\n"
              source.readed
              print_stops
              source;
            go (offset + length) cont
        in
        go initread cont)
  in
  let encoding =
    let open Encoding in
    with_size_header
      ~sizeencoding:`UInt62
      ~encoding:
        (with_size_header
           ~sizeencoding:`UInt62
           ~encoding:
             (with_size_header
                ~sizeencoding:`UInt62
                ~encoding:
                  (with_size_header
                     ~sizeencoding:`UInt62
                     ~encoding:
                       (with_size_header
                          ~sizeencoding:`UInt62
                          ~encoding:
                            (with_size_header
                               ~sizeencoding:`UInt62
                               ~encoding:
                                 (with_size_header
                                    ~sizeencoding:`UInt62
                                    ~encoding:
                                      (with_size_header
                                         ~sizeencoding:`UInt62
                                         ~encoding:uint62)))))))
  in
  w 16 8 encoding (Option.get @@ Encoding.Sizedints.Uint62.of_int64 0L);
  [%expect
    {|
    Blob: 000000000000004000000000000000380000000000000030000000000000002800000000000000200000000000000018000000000000001000000000000000080000000000000000
    Suspended, Readed: 16, Stops: 72-72
    Suspended, Readed: 8, Stops: 56-56-56
    Suspended, Readed: 8, Stops: 48-48-48-48
    Suspended, Readed: 8, Stops: 40-40-40-40-40
    Suspended, Readed: 8, Stops: 32-32-32-32-32-32
    Suspended, Readed: 8, Stops: 24-24-24-24-24-24-24
    Suspended, Readed: 8, Stops: 16-16-16-16-16-16-16-16
    Ok, Readed: 8, Stops: |}];
  w 7 9 encoding (Option.get @@ Encoding.Sizedints.Uint62.of_int64 0L);
  [%expect
    {|
    Blob: 000000000000004000000000000000380000000000000030000000000000002800000000000000200000000000000018000000000000001000000000000000080000000000000000
    Suspended, Readed: 0, Stops:
    Suspended, Readed: 9, Stops: 65-65
    Suspended, Readed: 8, Stops: 56-56-56
    Suspended, Readed: 7, Stops: 47-47-47-47
    Suspended, Readed: 6, Stops: 38-38-38-38-38
    Suspended, Readed: 5, Stops: 29-29-29-29-29-29
    Suspended, Readed: 4, Stops: 20-20-20-20-20-20-20
    Suspended, Readed: 3, Stops: 11-11-11-11-11-11-11-11
    Ok, Readed: 2, Stops: |}];
  let encoding =
    let open Encoding in
    let base = with_size_header ~sizeencoding:`UInt8 ~encoding:uint16 in
    let tup = with_size_header ~sizeencoding:`UInt8 ~encoding:[ base; base ] in
    let arr = array `UInt30 tup in
    arr
  in
  let uint16 n = Option.get @@ Encoding.Sizedints.Uint16.of_int n in
  let v =
    let open Encoding.Hlist in
    [| [ uint16 0xdead; uint16 0xbeef ]; [ uint16 0xfeed; uint16 0xbeef ] |]
  in
  w 10 6 encoding v;
  [%expect
    {|
    Blob: 000000020602dead02beef0602feed02beef
    Suspended, Readed: 9, Stops: 11-11
    Suspended, Readed: 6, Stops: 8-8
    Ok, Readed: 2, Stops: |}];
  w 6 5 encoding v;
  [%expect
    {|
    Blob: 000000020602dead02beef0602feed02beef
    Suspended, Readed: 6, Stops: 8-11
    Suspended, Readed: 5, Stops:
    Suspended, Readed: 5, Stops: 7-7
    Ok, Readed: 2, Stops: |}];
  let encoding =
    let open Encoding in
    let tup =
      with_size_header
        ~sizeencoding:`UInt8
        ~encoding:
          [ with_size_header ~sizeencoding:`UInt8 ~encoding:uint16
          ; string `UInt8
          ; string `UInt8
          ]
    in
    let arr = array `UInt30 tup in
    with_size_header ~sizeencoding:`UInt30 ~encoding:arr
  in
  let v =
    let open Encoding.Hlist in
    [| [ uint16 0xdead; "\xee\xee"; "\xff\xff\xff\xff\xff" ]
     ; [ uint16 0xfeed; "\xee\xee"; "\xee\xee" ]
    |]
  in
  w 10 6 encoding v;
  [%expect
    {|
    Blob: 0000001b000000020c02dead02eeee05ffffffffff0902feed02eeee02eeee
    Suspended, Readed: 10, Stops: 12-21-31
    Suspended, Readed: 6, Stops: 11-21
    Suspended, Readed: 6, Stops: 15-15
    Suspended, Readed: 6, Stops: 9-9
    Ok, Readed: 3, Stops: |}];
  w 6 5 encoding v;
  [%expect
    {|
    Blob: 0000001b000000020c02dead02eeee05ffffffffff0902feed02eeee02eeee
    Suspended, Readed: 4, Stops: 31
    Suspended, Readed: 4, Stops: 6-15-25
    Suspended, Readed: 5, Stops: 10-20
    Suspended, Readed: 5, Stops: 15
    Suspended, Readed: 5, Stops: 10-10
    Ok, Readed: 5, Stops: |}];
  ()
;;
