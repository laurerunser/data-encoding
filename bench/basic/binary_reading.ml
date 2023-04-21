let deserialise encoding src =
  let state = Buffy.R.mk_state src in
  Binary_data_encoding.Reader.readk state encoding
;;

let run name encoding make_data =
  Format.kasprintf Benchlib.log "\n%s.of_string (%d samples)\n" name Benchlib.repeats;
  List.iter
    (fun size ->
      let blob =
        Result.get_ok @@ Binary_data_encoding.Writer.string_of encoding (make_data size)
      in
      let blobs =
        let rec go blobs offset =
          if offset + Benchlib.buffer_size >= String.length blob
          then String.sub blob offset (String.length blob - offset) :: blobs
          else
            go
              (String.sub blob offset Benchlib.buffer_size :: blobs)
              (offset + Benchlib.buffer_size)
        in
        List.rev (go [] 0)
      in
      let deserialisations =
        Benchlib.measurer Benchlib.repeats (deserialise encoding) blobs
      in
      let deserialisations = Benchlib.flatten deserialisations in
      Benchlib.print_summary (String.length blob) Benchlib.buffer_size deserialisations)
    Benchlib.sizes
;;

module M0 = struct
  type datum =
    { x : int64
    ; y : int64
    }

  type data = datum array list

  let encoding : data Binary_data_encoding.Encoding.t =
    let open Binary_data_encoding.Encoding in
    list
      `Uint30
      (array
         `Uint8 (*TODO: support sized arrays, support N*)
         (conv
            ~serialisation:(fun { x; y } -> Commons.Hlist.[ x; y ])
            ~deserialisation:(fun Commons.Hlist.[ x; y ] -> Ok { x; y })
            (tuple [ int64; int64 ])))
  ;;

  let make_data sz =
    let datum = { x = 0L; y = 1L } in
    let arr n = Array.make n datum in
    let rec mk count acc =
      if count <= 0
      then acc
      else if count = 1
      then arr 10 :: acc
      else (
        assert (count >= 2);
        let delta = count mod 5 in
        mk (count - 2) (arr (10 - delta) :: arr (10 + delta) :: acc))
    in
    mk sz []
  ;;

  let () = run "list[ui30](array[ui8](conv(tup(i64,i64))))" encoding make_data
end

module M1 = struct
  type l =
    { x : int64
    ; y : int64
    }

  type r = (Commons.Sizedints.Uint30.t * int list) option
  type datum = (l, r) Either.t
  type data = datum array

  let encoding : data Binary_data_encoding.Encoding.t =
    let open Binary_data_encoding.Encoding in
    array
      `Uint30
      (either
         (conv
            ~serialisation:(fun { x; y } -> Commons.Hlist.[ x; y ])
            ~deserialisation:(fun Commons.Hlist.[ x; y ] -> Ok { x; y })
            (tuple [ int64; int64 ]))
         (option
            (conv
               ~serialisation:(fun (x, xs) ->
                 let xs =
                   List.map (fun x -> Option.get (Commons.Sizedints.Uint8.of_int x)) xs
                 in
                 Commons.Hlist.[ x; xs ])
               ~deserialisation:(fun Commons.Hlist.[ x; xs ] ->
                 let xs = (xs :> int list) in
                 Ok (x, xs))
               (tuple [ uint30; list `Uint30 uint8 ]))))
  ;;

  let make_data sz : data =
    Array.init sz (fun i ->
      if i mod 3 = 0
      then Either.Left { x = 0L; y = Int64.of_int i }
      else if i mod 5 = 0
      then Either.Right None
      else Either.Right (Some (Option.get (Commons.Sizedints.Uint30.of_int i), [])))
  ;;

  let () =
    run
      "array[ui30](either(conv(i64,i64),option(conv(ui30,list(i8)))))"
      encoding
      make_data
  ;;
end
