module Hlist = Commons.Hlist

type _ t =
  | Unit : unit t
  | Int64 : int64 t
  | UInt64 : Unsigned.UInt64.t t
  | Int32 : int32 t
  | UInt32 : Unsigned.UInt32.t t
  | UInt16 : Unsigned.UInt16.t t
  | String : Unsigned.UInt32.t -> string t
  | Bytes : Unsigned.UInt32.t -> bytes t
  | Option : 'a t -> 'a option t
  | Headered :
      { mkheader : 'a -> 'header
      ; headerencoding : 'header t
      ; encoding : 'header -> 'a t
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
let option t = Option t

let string =
  (* TODO: give control over representation of header *)
  (* TODO: error management *)
  Headered
    { mkheader =
        (fun s ->
          let l = String.length s in
          assert (l < 0xff_ff_ff_ff);
          Unsigned.UInt32.of_int l)
    ; headerencoding = UInt32
    ; encoding = (fun i -> String i)
    }
;;

let bytes =
  (* TODO: give control over representation of header *)
  (* TODO: error management *)
  Headered
    { mkheader =
        (fun s ->
          let l = Bytes.length s in
          assert (l < 0xff_ff_ff_ff);
          Unsigned.UInt32.of_int l)
    ; headerencoding = UInt32
    ; encoding = (fun i -> Bytes i)
    }
;;
