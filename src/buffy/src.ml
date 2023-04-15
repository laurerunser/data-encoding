(* TODO: expect tests *)
(* TODO: support unlimited-length sources (e.g., sockets) *)
(* TODO: support more than just strings *)

type t =
  { blob : string
  ; offset : int
  ; length : int
  ; mutable readed : int
  }

let available { length; readed; _ } = length - readed
let readed { readed; _ } = readed

let to_string { blob; offset; length; readed = _ } =
  if offset = 0 && length = String.length blob
  then blob
  else String.sub blob offset length
;;

let of_string ?(offset = 0) ?length blob =
  if offset < 0 then failwith "Buffy.Src.of_string: negative offset";
  let length =
    match length with
    | None -> String.length blob - offset
    | Some length -> length
  in
  if length < 0 then failwith "Buffy.Src.of_string: negative length";
  if offset + length > String.length blob
  then failwith "Buffy.Src.of_string: offset+length overflow";
  { blob; offset; length; readed = 0 }
;;

let of_bytes ?offset ?length blob =
  of_string ?offset ?length (Bytes.unsafe_to_string blob)
;;

let wrap_getter getter width src =
  if src.readed + width > src.length then failwith "Buffy.Src: overflow";
  let v = getter src.blob (src.offset + src.readed) in
  src.readed <- src.readed + width;
  v
;;

let get_char = wrap_getter String.get 1
let get_uint8 = wrap_getter String.get_uint8 1
let get_int8 = wrap_getter String.get_int8 1
let get_uint16_be = wrap_getter String.get_uint16_be 2
let get_uint16_le = wrap_getter String.get_uint16_le 2
let get_int16_be = wrap_getter String.get_int16_be 2
let get_int16_le = wrap_getter String.get_int16_le 2
let get_int32_be = wrap_getter String.get_int32_be 4
let get_int32_le = wrap_getter String.get_int32_le 4
let get_int64_be = wrap_getter String.get_int64_be 8
let get_int64_le = wrap_getter String.get_int64_le 8
let get_string t l = wrap_getter (fun s o -> String.sub s o l) l t

let get_blit_onto_bytes t dest doff len =
  if t.readed + len > t.length then failwith "Buffy.Src: overflow";
  if doff < 0 then failwith "Buffy.Src: blit underflow";
  if doff + len > Bytes.length dest then failwith "Buffy.Src: blit overflow";
  Bytes.blit_string t.blob (t.offset + t.readed) dest doff len;
  t.readed <- t.readed + len
;;
