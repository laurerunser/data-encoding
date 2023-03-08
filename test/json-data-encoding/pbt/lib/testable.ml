type any_encoding = AnyE : string * _ Json_data_encoding.Encoding.t -> any_encoding

let all_ground_encodings : any_encoding Seq.t =
  List.to_seq
    [ AnyE ("unit", Json_data_encoding.Encoding.unit)
    ; AnyE ("bool", Json_data_encoding.Encoding.bool)
    ; AnyE ("int64", Json_data_encoding.Encoding.int64)
    ; AnyE ("string", Json_data_encoding.Encoding.string)
    ]
;;

let extras : any_encoding Seq.t =
  List.to_seq
    [ AnyE
        ( "[int64;unit;string]"
        , Json_data_encoding.Encoding.(tuple [ int64; unit; string ]) )
    ; AnyE
        ( "{foo:int64;bar?:unit}"
        , Json_data_encoding.Encoding.(obj [ req "foo" int64; opt "bar" unit ]) )
    ; AnyE
        ( "{foo?:int64;bar?:bool}"
        , Json_data_encoding.Encoding.(obj [ opt "foo" int64; opt "bar" bool ]) )
    ]
;;

let sequences : any_encoding Seq.t -> any_encoding Seq.t =
  Seq.flat_map (fun (AnyE (s, e)) ->
    List.to_seq
      [ AnyE (Format.asprintf "seq(%s)" s, Json_data_encoding.Encoding.seq e)
      ; AnyE (Format.asprintf "list(%s)" s, Json_data_encoding.Encoding.list e)
      ; AnyE (Format.asprintf "array(%s)" s, Json_data_encoding.Encoding.array e)
      ])
;;

let basic_combinators : any_encoding Seq.t -> any_encoding Seq.t =
  Seq.flat_map (fun (AnyE (s, e)) ->
    List.to_seq
      [ AnyE
          ( Format.asprintf "conv[id](%s)" s
          , Json_data_encoding.Encoding.conv
              ~serialisation:Fun.id
              ~deserialisation:Result.ok
              e )
      ; AnyE
          ( Format.asprintf "obj{this=%s}" s
          , Json_data_encoding.Encoding.(obj [ req "this" e ]) )
      ])
;;

let either : any_encoding Seq.t -> any_encoding Seq.t -> any_encoding Seq.t =
  Seq.map_product (fun (AnyE (s1, e1)) (AnyE (s2, e2)) ->
    AnyE
      ( Format.asprintf "either(%s,%s)" s1 s2
      , Json_data_encoding.Encoding.Union.either e1 e2 ))
;;

type any_hlist_encoding =
  | AnyHNil : any_hlist_encoding
  | AnyH :
      string * _ Json_data_encoding.Encoding.Hlist.t Json_data_encoding.Encoding.tuple
      -> any_hlist_encoding

let tuplify : any_encoding -> any_hlist_encoding =
 fun (AnyE (s, e)) -> AnyH (Format.asprintf "[%s]" s, [ e ])
;;

let detuplify : any_hlist_encoding -> any_encoding option = function
  | AnyH (s, e) -> Some (AnyE (s, Json_data_encoding.Encoding.tuple e))
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
       Seq.map (fun (AnyE (es, e)) -> AnyH (Format.asprintf "%s;%s" es hs, e :: h)) es)
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

let new_name =
  let r = ref 0 in
  fun () ->
    incr r;
    Format.asprintf "fld%04d" !r
;;

type any_olist_encoding =
  | AnyONil : any_olist_encoding
  | AnyO :
      string * _ Json_data_encoding.Encoding.Hlist.t Json_data_encoding.Encoding.obj
      -> any_olist_encoding

let objify : any_encoding -> any_olist_encoding =
 fun (AnyE (s, e)) ->
  AnyO (Format.asprintf "{%s}" s, [ Json_data_encoding.Encoding.req (new_name ()) e ])
;;

let deobjify : any_olist_encoding -> any_encoding option = function
  | AnyO (s, e) -> Some (AnyE (s, Json_data_encoding.Encoding.obj e))
  | AnyONil -> None
;;

let obj_encodings
  : any_encoding Seq.t -> any_olist_encoding Seq.t -> any_olist_encoding Seq.t
  =
 fun es hs ->
  Seq.concat_map
    (function
     | AnyONil -> Seq.map objify es
     | AnyO (hs, h) ->
       Seq.map
         (fun (AnyE (es, e)) ->
           AnyO
             ( Format.asprintf "%s;%s" es hs
             , Json_data_encoding.Encoding.req (new_name ()) e :: h ))
         es)
    hs
;;

let objs : any_encoding Seq.t Seq.t -> any_encoding Seq.t =
 fun anyes ->
  Seq.fold_left (fun anyhs anyes -> obj_encodings anyes anyhs) (Seq.return AnyONil) anyes
  |> Seq.filter_map deobjify
;;
