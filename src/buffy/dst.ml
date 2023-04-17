(* TODO: expect tests *)
(* TODO: support more than just bytes *)
(* TODO: support unlimited-length destinations (e.g., files, hashers) *)

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
  ; mutable added : int (* the number of bytes already written on [buffer] *)
  }

(* extracting information from a destination *)

let available { length; added; _ } = length - added
let added { added; _ } = added

let to_string { buffer; offset; length = _length; added } =
  assert (added <= _length);
  Bytes.sub_string buffer offset added
;;

let blit_onto_bytes source soff dest doff len =
  assert (source.added <= source.length);
  if soff < 0 then invalid_arg "Buffy.Dst: underflow";
  if len > source.added then invalid_arg "Buffy.Dst: overflow";
  if doff < 0 then invalid_arg "Buffy.Dst: blit underflow";
  if doff + len > Bytes.length dest then invalid_arg "Buffy.Dst: blit overflow";
  Bytes.blit source.buffer (source.offset + soff) dest doff len
;;

let bytes_of_dst t = t.buffer, t.offset, t.added

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
  { buffer; offset; length; added = 0 }
;;

(* actually adding bytes onto destinations *)

let wrap_setter setter width dst setted =
  if dst.added + width > dst.length then invalid_arg "Buffy.Dst: overflow";
  setter dst.buffer (dst.offset + dst.added) setted;
  dst.added <- dst.added + width
;;

let add_char = wrap_setter Bytes.set 1
let add_uint8 = wrap_setter Bytes.set_uint8 1
let add_int8 = wrap_setter Bytes.set_int8 1
let add_uint16_be = wrap_setter Bytes.set_uint16_be 2
let add_uint16_le = wrap_setter Bytes.set_uint16_le 2
let add_int16_be = wrap_setter Bytes.set_int16_be 2
let add_int16_le = wrap_setter Bytes.set_int16_le 2
let add_int32_be = wrap_setter Bytes.set_int32_be 4
let add_int32_le = wrap_setter Bytes.set_int32_le 4
let add_int64_be = wrap_setter Bytes.set_int64_be 8
let add_int64_le = wrap_setter Bytes.set_int64_le 8

let add_string t s =
  let l = String.length s in
  wrap_setter (fun b o s -> Bytes.blit_string s 0 b o l) l t s
;;

(* [oo] is the offset for the source that we are blitting from *)
let add_string_slice t s oo l =
  wrap_setter (fun b o s -> Bytes.blit_string s oo b o l) l t s
;;

let add_bytes t s =
  let l = Bytes.length s in
  wrap_setter (fun b o s -> Bytes.blit s 0 b o l) l t s
;;

(* [oo] is the offset for the source that we are blitting from *)
let add_bytes_slice t s oo l = wrap_setter (fun b o s -> Bytes.blit s oo b o l) l t s
