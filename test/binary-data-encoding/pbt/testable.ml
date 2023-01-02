type any_encoding = AnyE : string * _ Binary_data_encoding.Encoding.t -> any_encoding

let all_ground_encodings : any_encoding Seq.t =
  let open Binary_data_encoding.Encoding in
  List.to_seq
    [ AnyE ("unit", unit)
    ; AnyE ("bool", bool)
    ; AnyE ("i64", int64)
    ; AnyE ("ui64", uint64)
    ; AnyE ("i32", int32)
    ; AnyE ("ui32", uint32)
    ; AnyE ("ui16", uint16)
    ; AnyE ("ui8", uint8)
    ; AnyE ("str[ui8]", Binary_data_encoding.Encoding.string `UInt8)
      (* TODO: support bigger strings *)
    ]
;;

let str_encodings : any_encoding Seq.t =
  Seq.append
    (Seq.map
       (fun n ->
         AnyE
           ( Format.asprintf "str[%d]" n
           , Binary_data_encoding.Encoding.string (`Fixed (Stdint.Uint32.of_int n)) ))
       (List.to_seq [ 0; 1; 7; 129 ]))
    (Seq.map
       (fun n ->
         AnyE
           ( Format.asprintf "bytes[%d]" n
           , Binary_data_encoding.Encoding.bytes (`Fixed (Stdint.Uint32.of_int n)) ))
       (List.to_seq [ 0; 1; 7; 129 ]))
;;

let simplish_encodings : any_encoding Seq.t -> any_encoding Seq.t =
 fun s ->
  Seq.concat_map
    (fun (AnyE (s, e)) ->
      List.to_seq
        [ AnyE ("opt(" ^ s ^ ")", Binary_data_encoding.Encoding.option e)
        ; AnyE
            ( "conv[id](" ^ s ^ ")"
            , Binary_data_encoding.Encoding.conv
                ~serialisation:Fun.id
                ~deserialisation:Result.ok
                e )
        ; AnyE
            ( "headered[unit](" ^ s ^ ")"
            , Binary_data_encoding.Encoding.(
                with_header unit (fun _ -> Ok ()) (fun () -> Ok e))
                (Pbtlib.equal_of_encoding e) )
        ])
    s
;;

let lvl0 = Seq.append all_ground_encodings str_encodings
let lvl1 = simplish_encodings lvl0
let all_simplish_encodings = Seq.append lvl0 lvl1
let lvl2 = simplish_encodings lvl1
let basics = Seq.append all_simplish_encodings lvl2

type any_hlist_encoding =
  | AnyH :
      string * _ Binary_data_encoding.Encoding.Hlist.t Binary_data_encoding.Encoding.t
      -> any_hlist_encoding

let tuplify : any_encoding -> any_hlist_encoding =
 fun (AnyE (s, e)) -> AnyH ("[" ^ s ^ "]", [ e ])
;;

let detuplify : any_hlist_encoding -> any_encoding = fun (AnyH (s, e)) -> AnyE (s, e)

let tuple_encodings
    : any_encoding Seq.t -> any_hlist_encoding Seq.t -> any_hlist_encoding Seq.t
  =
 fun es hs ->
  Seq.concat_map
    (fun (AnyH (hs, h)) ->
      Seq.map (fun (AnyE (es, e)) -> AnyH (es ^ ";" ^ hs, [ e; h ])) es)
    hs
;;

let not_so_basic : any_encoding Seq.t =
  let lvl3 =
    tuple_encodings all_simplish_encodings (Seq.map tuplify all_simplish_encodings)
  in
  let lvl4 = tuple_encodings all_simplish_encodings lvl3 in
  Seq.map detuplify (Seq.append lvl3 lvl4)
;;
