module Hlist = Commons.Hlist

type _ t =
  | Unit : unit t
  | Int64 : int64 t
  | UInt64 : Unsigned.UInt64.t t
  | Int32 : int32 t
  | UInt32 : Unsigned.UInt32.t t
  | UInt16 : Unsigned.UInt16.t t
  | UInt8 : Unsigned.UInt8.t t
  | String : Unsigned.UInt32.t -> string t
  | Bytes : Unsigned.UInt32.t -> bytes t
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

let with_uint8_header mkheader encoding =
  Headered { mkheader; headerencoding = UInt8; encoding }
;;

let with_uint16_header mkheader encoding =
  Headered { mkheader; headerencoding = UInt16; encoding }
;;

let with_uint32_header mkheader encoding =
  Headered { mkheader; headerencoding = UInt32; encoding }
;;

let string = function
  | `Fixed u -> String u
  | `UInt32 ->
    with_uint32_header
      (fun v -> Ok (Unsigned.UInt32.of_int (String.length v)))
      (fun n -> Ok (String n))
  | `UInt16 ->
    with_uint16_header
      (fun v -> Ok (Unsigned.UInt16.of_int (String.length v)))
      (fun n -> Ok (String (Unsigned.UInt32.of_int (Unsigned.UInt16.to_int n))))
  | `UInt8 ->
    with_uint8_header
      (fun v -> Ok (Unsigned.UInt8.of_int (String.length v)))
      (fun n -> Ok (String (Unsigned.UInt32.of_int (Unsigned.UInt8.to_int n))))
;;

let bytes = function
  | `Fixed u -> Bytes u
  | `UInt32 ->
    with_uint32_header
      (fun v -> Ok (Unsigned.UInt32.of_int (Bytes.length v)))
      (fun n -> Ok (Bytes n))
  | `UInt16 ->
    with_uint16_header
      (fun v -> Ok (Unsigned.UInt16.of_int (Bytes.length v)))
      (fun n -> Ok (Bytes (Unsigned.UInt32.of_int (Unsigned.UInt16.to_int n))))
  | `UInt8 ->
    with_uint8_header
      (fun v -> Ok (Unsigned.UInt8.of_int (Bytes.length v)))
      (fun n -> Ok (Bytes (Unsigned.UInt32.of_int (Unsigned.UInt8.to_int n))))
;;

let conv ~serialisation ~deserialisation encoding =
  Conv { serialisation; deserialisation; encoding }
;;
