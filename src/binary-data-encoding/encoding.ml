module Hlist = Commons.Hlist
module Sizedints = Commons.Sizedints

type _ t =
  | Unit : unit t
  | Bool : bool t
  | UInt8 : Sizedints.Uint8.t t
  | UInt16 : Sizedints.Uint16.t t
  | UInt30 : Sizedints.Uint30.t t
  | Int32 : int32 t
  | Int64 : int64 t
  | String : Sizedints.Uint30.t -> string t
  | Bytes : Sizedints.Uint30.t -> bytes t
  | Option : 'a t -> 'a option t
  | Headered :
      { mkheader : 'a -> ('header, string) result
      ; headerencoding : 'header t
      ; encoding : 'header -> ('a t, string) result
      ; equal : 'a -> 'a -> bool
      }
      -> 'a t
  | Conv :
      { serialisation : 'a -> 'b
      ; deserialisation : 'b -> ('a, string) result
      ; encoding : 'b t
      }
      -> 'a t
  | [] : unit Hlist.t t
  | ( :: ) : 'a t * 'b Hlist.t t -> ('a * 'b) Hlist.t t

let unit = Unit
let bool = Bool
let int64 = Int64
let int32 = Int32
let uint30 = UInt30
let uint16 = UInt16
let uint8 = UInt8
let option t = Option t

let with_header headerencoding mkheader encoding equal =
  Headered { mkheader; headerencoding; encoding; equal }
;;

let string = function
  | `Fixed u -> String u
  | `UInt30 ->
    with_header
      UInt30
      (fun v ->
        let len = String.length v in
        match Sizedints.Uint30.of_int len with
        | None -> Error "String larger than header-size can encode"
        | Some n -> Ok n)
      (fun n -> Ok (String n))
      String.equal
  | `UInt16 ->
    with_header
      UInt16
      (fun v ->
        let len = String.length v in
        match Sizedints.Uint16.of_int len with
        | None -> Error "String larger than header-size can encode"
        | Some n -> Ok n)
      (fun n -> Ok (String (Sizedints.Uint16.to_uint30 n)))
      String.equal
  | `UInt8 ->
    with_header
      UInt8
      (fun v ->
        let len = String.length v in
        match Sizedints.Uint8.of_int len with
        | None -> Error "String larger than header-size can encode"
        | Some n -> Ok n)
      (fun n -> Ok (String (Sizedints.Uint8.to_uint30 n)))
      String.equal
;;

let bytes = function
  | `Fixed u -> Bytes u
  | `UInt30 ->
    with_header
      UInt30
      (fun v ->
        match Sizedints.Uint30.of_int (Bytes.length v) with
        | None -> Error "Bytes larger than header-size can encode"
        | Some n -> Ok n)
      (fun n -> Ok (Bytes n))
      Bytes.equal
  | `UInt16 ->
    with_header
      UInt16
      (fun v ->
        match Sizedints.Uint16.of_int (Bytes.length v) with
        | None -> Error "Bytes larger than header-size can encode"
        | Some n -> Ok n)
      (fun n -> Ok (Bytes (Sizedints.Uint16.to_uint30 n)))
      Bytes.equal
  | `UInt8 ->
    with_header
      UInt8
      (fun v ->
        match Sizedints.Uint8.of_int (Bytes.length v) with
        | None -> Error "Bytes larger than header-size can encode"
        | Some n -> Ok n)
      (fun n -> Ok (Bytes (Sizedints.Uint8.to_uint30 n)))
      Bytes.equal
;;

let conv ~serialisation ~deserialisation encoding =
  Conv { serialisation; deserialisation; encoding }
;;
