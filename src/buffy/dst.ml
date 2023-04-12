(* TODO: expect tests *)
(* TODO: support more than just bytes *)
(* TODO: support unlimited-length destinations (e.g., files, hashers) *)
(* TODO: support writers that don't have getters (e.g., hashers) *)

(* A value of the type [t] is a destination. Writing onto [t] writes the bytes
   onto the [bytes] of the [buffer] field.
   Writing is limited to the [[offset,offset+length]] range. The offset is
   automatically adjusted when writing onto the destination so the user doesn't
   need to know about it. (And indeed the type [t] is abstract.) Writing outside
   of the limits (e.g., with negative offsets or with excessive lengths) raises
   exceptions. *)
type t =
  { buffer : bytes (* the underlying bytes *)
  ; offset : int (* the first of the writable bytes of [buffer] *)
  ; length : int (* the length of the writable span of [buffer] *)
  }

(* extracting information from a destination *)

let length { length; _ } = length
let to_string { buffer; offset; length } = Bytes.sub_string buffer offset length

let blit_onto_bytes source soff dest doff len =
  if soff < 0 then invalid_arg "Buffy.Dst: underflow";
  if soff + len > source.length then invalid_arg "Buffy.Dst: overflow";
  if doff < 0 then invalid_arg "Buffy.Dst: blit underflow";
  if doff + len > Bytes.length dest then invalid_arg "Buffy.Dst: blit overflow";
  Bytes.blit source.buffer (source.offset + soff) dest doff len
;;

let bytes_of_dst t = t.buffer, t.offset, t.length

(* making destinations *)

let of_bytes ?(offset = 0) ?length buffer =
  if offset < 0 then invalid_arg "Buffy.Dst.of_bytes: negative offset";
  let length =
    match length with
    | None -> Bytes.length buffer - offset
    | Some length -> length
  in
  if length < 0 then invalid_arg "Buffy.Dst.of_bytes: negative length";
  if offset + length > Bytes.length buffer
  then invalid_arg "Buffy.Dst.of_bytes: offset+length overflow";
  { buffer; offset; length }
;;

let of_dst ~offset ~length dest =
  if offset < 0 then invalid_arg "Buffy.Dst.of_dst: negative offset";
  if length < 0 then invalid_arg "Buffy.Dst.of_dst: negative length";
  if offset + length > dest.length
  then invalid_arg "Buffy.Dst.of_dst: offset+length overflow";
  { buffer = dest.buffer; offset = dest.offset + offset; length }
;;

(* actually adding bytes onto destinations *)

(* In this and all the definitions below, [o] is the offset provided by the caller. *)
let wrap_setter setter width { buffer; offset; length } o setted =
  if o < 0 then invalid_arg "Buffy.Dst: underflow";
  if o + width > length then invalid_arg "Buffy.Dst: overflow";
  setter buffer (offset + o) setted
;;

let set_char o c = wrap_setter Bytes.set 1 o c
let set_uint8 o u8 = wrap_setter Bytes.set_uint8 1 o u8
let set_int8 o i8 = wrap_setter Bytes.set_int8 1 o i8
let set_uint16_be o u16 = wrap_setter Bytes.set_uint16_be 2 o u16
let set_uint16_le o u16 = wrap_setter Bytes.set_uint16_le 2 o u16
let set_int16_be o i16 = wrap_setter Bytes.set_int16_be 2 o i16
let set_int16_le o i16 = wrap_setter Bytes.set_int16_le 2 o i16
let set_int32_be o i32 = wrap_setter Bytes.set_int32_be 4 o i32
let set_int32_le o i32 = wrap_setter Bytes.set_int32_le 4 o i32
let set_int64_be o i64 = wrap_setter Bytes.set_int64_be 8 o i64
let set_int64_le o i64 = wrap_setter Bytes.set_int64_le 8 o i64

let set_string t o s =
  let l = String.length s in
  wrap_setter (fun b o s -> Bytes.blit_string s 0 b o l) l t o s
;;

let set_string_slice t o s oo l =
  wrap_setter (fun b o s -> Bytes.blit_string s oo b o l) l t o s
;;

let set_bytes t o s =
  let l = Bytes.length s in
  wrap_setter (fun b o s -> Bytes.blit s 0 b o l) l t o s
;;

(* [oo] is the offset for the source that we are blitting from *)
let set_bytes_slice t o s oo l = wrap_setter (fun b o s -> Bytes.blit s oo b o l) l t o s
