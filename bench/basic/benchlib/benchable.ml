module type S = sig
  type data

  val encoding : data Data_encoding.Encoding.t
  val make_data : int -> data
  val name : string
end

(* TODO: avoid the split declaration encodings by filling up the common interface *)

module Benchable0 : S = struct
  type datum =
    { x : int64
    ; y : int64
    }

  type data = datum array list

  let encoding : data Data_encoding.Encoding.t =
    let binary =
      let open Binary_data_encoding.Encoding in
      list
        `Uint30
        (array
           `Uint8 (*TODO: support sized arrays, support N*)
           (conv
              ~serialisation:(fun { x; y } -> Commons.Hlist.[ x; y ])
              ~deserialisation:(fun Commons.Hlist.[ x; y ] -> Ok { x; y })
              (tuple [ int64; int64 ])))
    in
    let json =
      let open Json_data_encoding.Encoding in
      list
        (array
           Record.(
             record
               (fun x y -> { x; y })
               [ field "x" (fun { x; _ } -> x) int64
               ; field "y" (fun { y; _ } -> y) int64
               ]))
    in
    { Data_encoding.Encoding.binary; json }
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

  let name = "list[ui30](array[ui8](record(i64,i64)))"
end

module Benchable1 : S = struct
  type l =
    { x : int64
    ; y : int64
    }

  type r = (Commons.Sizedints.Uint30.t * int list) option
  type datum = (l, r) Either.t
  type data = datum array

  let encoding : data Data_encoding.Encoding.t =
    let binary =
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
    in
    let json =
      let open Json_data_encoding.Encoding in
      conv
        ~serialisation:(fun _ -> assert false)
        ~deserialisation:(fun _ -> assert false)
        unit
    in
    { Data_encoding.Encoding.binary; json }
  ;;

  let make_data sz : data =
    Array.init sz (fun i ->
      if i mod 3 = 0
      then Either.Left { x = 0L; y = Int64.of_int i }
      else if i mod 5 = 0
      then Either.Right None
      else Either.Right (Some (Option.get (Commons.Sizedints.Uint30.of_int i), [])))
  ;;

  let name = "array[ui30](either(conv(i64,i64),option(conv(ui30,list(i8)))))"
end
