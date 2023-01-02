module Hlist = Commons.Hlist

type _ t =
  | Unit : unit t
  | Bool : bool t
  | Int64 : int64 t
  | UInt64 : Stdint.Uint64.t t
  | Int32 : int32 t
  | UInt32 : Stdint.Uint32.t t
  | UInt16 : Stdint.Uint16.t t
  | UInt8 : Stdint.Uint8.t t
  | String : Stdint.Uint32.t -> string t
  | Bytes : Stdint.Uint32.t -> bytes t
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
let uint64 = UInt64
let int32 = Int32
let uint32 = UInt32
let uint16 = UInt16
let uint8 = UInt8
let option t = Option t

let with_header headerencoding mkheader encoding equal =
  Headered { mkheader; headerencoding; encoding; equal }
;;

let string = function
  | `Fixed u -> String u
  | `UInt32 ->
    with_header
      UInt32
      (fun v ->
        let len = String.length v in
        if len > Stdint.Uint32.(to_int max_int)
        then Error "String larger than header-size can encode"
        else Ok (Stdint.Uint32.of_int len))
      (fun n -> Ok (String n))
      String.equal
  | `UInt16 ->
    with_header
      UInt16
      (fun v ->
        let len = String.length v in
        if len > Stdint.Uint16.(to_int max_int)
        then Error "String larger than header-size can encode"
        else Ok (Stdint.Uint16.of_int len))
      (fun n -> Ok (String (Stdint.Uint32.of_int (Stdint.Uint16.to_int n))))
      String.equal
  | `UInt8 ->
    with_header
      UInt8
      (fun v ->
        let len = String.length v in
        if len > Stdint.Uint8.(to_int max_int)
        then Error "String larger than header-size can encode"
        else Ok (Stdint.Uint8.of_int len))
      (fun n -> Ok (String (Stdint.Uint32.of_int (Stdint.Uint8.to_int n))))
      String.equal
;;

let bytes = function
  | `Fixed u -> Bytes u
  | `UInt32 ->
    with_header
      UInt32
      (fun v ->
        let len = Bytes.length v in
        if len > Stdint.Uint32.(to_int max_int)
        then Error "Bytes larger than header-size can encode"
        else Ok (Stdint.Uint32.of_int len))
      (fun n -> Ok (Bytes n))
      Bytes.equal
  | `UInt16 ->
    with_header
      UInt16
      (fun v ->
        let len = Bytes.length v in
        if len > Stdint.Uint16.(to_int max_int)
        then Error "Bytes larger than header-size can encode"
        else Ok (Stdint.Uint16.of_int len))
      (fun n -> Ok (Bytes (Stdint.Uint32.of_int (Stdint.Uint16.to_int n))))
      Bytes.equal
  | `UInt8 ->
    with_header
      UInt8
      (fun v ->
        let len = Bytes.length v in
        if len > Stdint.Uint8.(to_int max_int)
        then Error "Bytes larger than header-size can encode"
        else Ok (Stdint.Uint8.of_int len))
      (fun n -> Ok (Bytes (Stdint.Uint32.of_int (Stdint.Uint8.to_int n))))
      Bytes.equal
;;

let conv ~serialisation ~deserialisation encoding =
  Conv { serialisation; deserialisation; encoding }
;;
