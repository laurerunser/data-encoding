type t =
  { buffer : bytes
  ; offset : int
  ; length : int
  }

let length { length; _ } = length
let to_string { buffer; offset; length } = Bytes.sub_string buffer offset length

let of_bytes ?(offset = 0) ?length buffer =
  if offset < 0 then failwith "Buffy.Dst.of_bytes: negative offset";
  let length =
    match length with
    | None -> Bytes.length buffer - offset
    | Some length -> length
  in
  if length < 0 then failwith "Buffy.Dst.of_bytes: negative length";
  if offset + length > Bytes.length buffer
  then failwith "Buffy.Dst.of_bytes: offset+length overflow";
  { buffer; offset; length }
;;

let of_dst ~offset ~length dest =
  if offset < 0 then failwith "Buffy.Dst.of_dst: negative offset";
  if length < 0 then failwith "Buffy.Dst.of_dst: negative length";
  if offset + length > dest.length
  then failwith "Buffy.Dst.of_dst: offset+length overflow";
  { buffer = dest.buffer; offset = dest.offset + offset; length }
;;

let wrap_setter setter width { buffer; offset; length } index setted =
  if index < 0 then failwith "Buffy.Dst: underflow";
  if index + width > length then failwith "Buffy.Dst: overflow";
  setter buffer (offset + index) setted
;;

let set_char = wrap_setter Bytes.set 1
let set_uint8 = wrap_setter Bytes.set_uint8 1
let set_int8 = wrap_setter Bytes.set_int8 1
let set_uint16_be = wrap_setter Bytes.set_uint16_be 2
let set_uint16_le = wrap_setter Bytes.set_uint16_le 2
let set_int16_be = wrap_setter Bytes.set_int16_be 2
let set_int16_le = wrap_setter Bytes.set_int16_le 2
let set_int32_be = wrap_setter Bytes.set_int32_be 4
let set_int32_le = wrap_setter Bytes.set_int32_le 4
let set_int64_be = wrap_setter Bytes.set_int64_be 8
let set_int64_le = wrap_setter Bytes.set_int64_le 8

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

let set_bytes_slice t o s oo l = wrap_setter (fun b o s -> Bytes.blit s oo b o l) l t o s

(* the source is a [t] *)
let blit_onto_bytes source soff dest doff len =
  if soff < 0 then failwith "Buffy.Dst: underflow";
  if soff + len > source.length then failwith "Buffy.Dst: overflow";
  if doff < 0 then failwith "Buffy.Dst: blit underflow";
  if doff + len > Bytes.length dest then failwith "Buffy.Dst: blit overflow";
  Bytes.blit source.buffer (source.offset + soff) dest doff len
;;
