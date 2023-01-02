module Hlist = Commons.Hlist

type _ t =
  | Unit : unit t
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
let int64 = Int64
let uint64 = UInt64
let int32 = Int32
let uint32 = UInt32
let uint16 = UInt16
let uint8 = UInt8
let option t = Option t

let with_header headerencoding mkheader encoding =
  Headered { mkheader; headerencoding; encoding }
;;

let string = function
  | `Fixed u -> String u
  | `UInt32 ->
    with_header
      UInt32
      (fun v -> Ok (Stdint.Uint32.of_int (String.length v)))
      (fun n -> Ok (String n))
  | `UInt16 ->
    with_header
      UInt16
      (fun v -> Ok (Stdint.Uint16.of_int (String.length v)))
      (fun n -> Ok (String (Stdint.Uint32.of_int (Stdint.Uint16.to_int n))))
  | `UInt8 ->
    with_header
      UInt8
      (fun v -> Ok (Stdint.Uint8.of_int (String.length v)))
      (fun n -> Ok (String (Stdint.Uint32.of_int (Stdint.Uint8.to_int n))))
;;

let bytes = function
  | `Fixed u -> Bytes u
  | `UInt32 ->
    with_header
      UInt32
      (fun v -> Ok (Stdint.Uint32.of_int (Bytes.length v)))
      (fun n -> Ok (Bytes n))
  | `UInt16 ->
    with_header
      UInt16
      (fun v -> Ok (Stdint.Uint16.of_int (Bytes.length v)))
      (fun n -> Ok (Bytes (Stdint.Uint32.of_int (Stdint.Uint16.to_int n))))
  | `UInt8 ->
    with_header
      UInt8
      (fun v -> Ok (Stdint.Uint8.of_int (Bytes.length v)))
      (fun n -> Ok (Bytes (Stdint.Uint32.of_int (Stdint.Uint8.to_int n))))
;;

let conv ~serialisation ~deserialisation encoding =
  Conv { serialisation; deserialisation; encoding }
;;
