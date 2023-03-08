type any_encoding = AnyE : string * _ Binary_data_encoding.Encoding.t -> any_encoding

let all_ground_encodings : any_encoding Seq.t =
  let open Binary_data_encoding.Encoding in
  List.to_seq
    [ AnyE ("unit", unit)
    ; AnyE ("bool", bool)
    ; AnyE ("i64", int64)
    ; AnyE ("i32", int32)
    ; AnyE ("ui62", uint62)
    ; AnyE ("ui30", uint30)
    ; AnyE ("ellastic_ui30", ellastic_uint30)
    ; AnyE ("ui16", uint16)
    ; AnyE ("i64le", Little_endian.int64)
    ; AnyE ("i32le", Little_endian.int32)
    ; AnyE ("ui62le", Little_endian.uint62)
    ; AnyE ("ui30le", Little_endian.uint30)
    ; AnyE ("ui16le", Little_endian.uint16)
    ; AnyE ("ui8le", Little_endian.uint8)
    ; AnyE ("str[ui8]", string `UInt8) (* TODO: support bigger strings *)
    ]
;;

let str_encodings : any_encoding Seq.t =
  Seq.append
    (Seq.map
       (fun n ->
         AnyE
           ( Format.asprintf "string[%Ld]" n
           , Binary_data_encoding.Encoding.string
               (`Fixed (Option.get @@ Commons.Sizedints.Uint62.of_int64 n)) ))
       (List.to_seq [ 0L; 1L; 129L ]))
    (Seq.map
       (fun n ->
         AnyE
           ( Format.asprintf "bytes[%Ld]" n
           , Binary_data_encoding.Encoding.bytes
               (`Fixed (Option.get @@ Commons.Sizedints.Uint62.of_int64 n)) ))
       (List.to_seq [ 0L; 1L; 129L ]))
;;

let four = Option.get @@ Commons.Sizedints.Uint62.of_int64 4L

let sequences : any_encoding Seq.t -> any_encoding Seq.t =
  let open Binary_data_encoding.Encoding in
  Seq.flat_map (fun (AnyE (s, e)) ->
    let l : any_encoding list =
      [ AnyE (Format.asprintf "array[ui8](%s)" s, array `UInt8 e)
      ; AnyE (Format.asprintf "array[4](%s)" s, array (`Fixed four) e)
      ; AnyE (Format.asprintf "seql[ui8](%s)" s, seq_with_length `UInt8 e)
      ; AnyE (Format.asprintf "seql[4](%s)" s, seq_with_length (`Fixed four) e)
      ]
    in
    let l =
      match Binary_data_encoding.Query.Sizability.sizability e with
      | S (Static Zero) ->
        (* We cannot apply [seq_with_size] to zero-length elements *)
        l
      | S (Static Plus | Dynamic) ->
        AnyE (Format.asprintf "sequ[ui16](%s)" s, seq_with_size `UInt16 e) :: l
    in
    List.to_seq l)
;;

let either : any_encoding Seq.t -> any_encoding Seq.t -> any_encoding Seq.t =
  let open Binary_data_encoding.Encoding in
  Seq.map_product (fun (AnyE (s1, e1)) (AnyE (s2, e2)) ->
    AnyE (Format.asprintf "either(%s,%s)" s1 s2, Union.either e1 e2))
;;

let simple_combinators : any_encoding Seq.t -> any_encoding Seq.t =
  Seq.concat_map (fun (AnyE (s, e)) ->
    List.to_seq
      [ AnyE ("option(" ^ s ^ ")", Binary_data_encoding.Encoding.option e)
      ; AnyE
          ( "conv[id](" ^ s ^ ")"
          , Binary_data_encoding.Encoding.conv
              ~serialisation:Fun.id
              ~deserialisation:Result.ok
              e )
      ; AnyE
          ( "headered[unit](" ^ s ^ ")"
          , Binary_data_encoding.Encoding.(
              with_header
                ~headerencoding:unit
                ~mkheader:(fun _ -> Ok ())
                ~mkencoding:(fun () -> Ok e))
              ~equal:(Binary_data_encoding.Query.equal_of e)
              ~maximum_size:(Binary_data_encoding.Query.maximum_size_of e) )
      ; AnyE
          ( "sized(" ^ s ^ ")"
          , Binary_data_encoding.Encoding.with_size_header
              ~sizeencoding:`UInt16
              ~encoding:e )
      ])
;;

type any_hlist_encoding =
  | AnyHNil : any_hlist_encoding
  | AnyH :
      string * _ Binary_data_encoding.Encoding.Hlist.t Binary_data_encoding.Encoding.t
      -> any_hlist_encoding

let tuplify : any_encoding -> any_hlist_encoding =
 fun (AnyE (s, e)) -> AnyH (Format.asprintf "[%s]" s, [ e ])
;;

let detuplify : any_hlist_encoding -> any_encoding option = function
  | AnyH (s, e) -> Some (AnyE (s, e))
  | AnyHNil -> None
;;

let tuple_encodings
  : any_encoding Seq.t -> any_hlist_encoding Seq.t -> any_hlist_encoding Seq.t
  =
 fun es hs ->
  Seq.concat_map
    (function
     | AnyHNil -> Seq.map tuplify es
     | AnyH (hs, h) ->
       Seq.map (fun (AnyE (es, e)) -> AnyH (Format.asprintf "%s;%s" es hs, [ e; h ])) es)
    hs
;;

let tuples : any_encoding Seq.t Seq.t -> any_encoding Seq.t =
 fun anyes ->
  Seq.fold_left
    (fun anyhs anyes -> tuple_encodings anyes anyhs)
    (Seq.return AnyHNil)
    anyes
  |> Seq.filter_map detuplify
;;
