module Hlist = Commons.Hlist
module Sizedints = Commons.Sizedints

type ('step, 'finish) reducer = ('step, 'finish) Descr.reducer =
  | K of 'step
  | Finish of 'finish

type 'a numeral = 'a Descr.numeral =
  | UInt8 : Sizedints.Uint8.t numeral
  | UInt16 : Sizedints.Uint16.t numeral
  | UInt30 : Sizedints.Uint30.t numeral
  | UInt62 : Sizedints.Uint62.t numeral
  | Int32 : int32 numeral
  | Int64 : int64 numeral

type endianness = Descr.endianness =
  | Big_endian
  | Little_endian

type 'a seq_with_length = 'a Descr.seq_with_length =
  { seq : 'a Seq.t
  ; length : Sizedints.Uint62.t Lazy.t
  }

(** [α t] is a encoding for values of type [α]. The encoding can be used to
    de/serialise these values—see {!Backend}.

    - Product are supported via the overloading of the list constructors ([[]]
    and [(::)]). This allows compact writing of product encodings of width.

    - [Headered] and [Fold] are low-level constructors. They are meant to be
    used to create advanced encodings which are not supported by the library.
    E.g., they can be used to support lists with a length header. E.g., they can
    be used to support Zarith's [Z.t] values. Note that both of those are
    supported natively by the library; these examples just demonstrate the kind
    of expressive power that [Headered] and [Fold] give to the library user.
*)
type 'a t = 'a Descr.t =
  | Unit : unit t
  | Bool : bool t
  | Numeral :
      { numeral : 'a numeral
      ; endianness : endianness
      }
      -> 'a t
  | String : Sizedints.Uint62.t -> string t
  | Bytes : Sizedints.Uint62.t -> bytes t
  | Array :
      { length : Sizedints.Uint62.t
      ; elementencoding : 'a t
      }
      -> 'a array t
  | LSeq :
      { length : Sizedints.Uint62.t
      ; elementencoding : 'a t
      }
      -> 'a seq_with_length t
  | USeq : { elementencoding : 'a t } -> 'a Seq.t t
  | Option : 'a t -> 'a option t
  | Headered :
      { mkheader : 'a -> ('header, string) result
      ; headerencoding : 'header t
      ; mkencoding : 'header -> ('a t, string) result
      ; equal : 'a -> 'a -> bool
      ; maximum_size : Optint.Int63.t (* the max size of the payload *)
      }
      -> 'a t
  | Fold :
      { chunkencoding : 'chunk t
      ; chunkify : 'a -> 'chunk Seq.t
      ; readinit : 'acc
      ; reducer : 'acc -> 'chunk -> ('acc, 'a) reducer
      ; equal : 'a -> 'a -> bool
      ; maximum_size : Optint.Int63.t
      }
      -> 'a t
  | Conv :
      { serialisation : 'a -> 'b
      ; deserialisation : 'b -> ('a, string) result
      ; encoding : 'b t
      }
      -> 'a t
  | Size_headered :
      { size : _ numeral
      ; encoding : 'a t
      }
      -> 'a t
  | Size_limit :
      { at_most : Sizedints.Uint62.t
      ; encoding : 'a t
      }
      -> 'a t
  | [] : unit Hlist.t t
  | ( :: ) : 'a t * 'b Hlist.t t -> ('a * 'b) Hlist.t t

let unit = Unit
let bool = Bool

module Big_endian = struct
  let int64 = Numeral { numeral = Int64; endianness = Big_endian }
  let int32 = Numeral { numeral = Int32; endianness = Big_endian }
  let uint30 = Numeral { numeral = UInt30; endianness = Big_endian }
  let uint62 = Numeral { numeral = UInt62; endianness = Big_endian }
  let uint16 = Numeral { numeral = UInt16; endianness = Big_endian }
  let uint8 = Numeral { numeral = UInt8; endianness = Big_endian }
end

include Big_endian

let default_endianness = Big_endian

module Little_endian = struct
  let int64 = Numeral { numeral = Int64; endianness = Little_endian }
  let int32 = Numeral { numeral = Int32; endianness = Little_endian }
  let uint30 = Numeral { numeral = UInt30; endianness = Little_endian }
  let uint62 = Numeral { numeral = UInt62; endianness = Little_endian }
  let uint16 = Numeral { numeral = UInt16; endianness = Little_endian }
  let uint8 = Numeral { numeral = UInt8; endianness = Little_endian }
end

let option t = Option t

let with_header ~headerencoding ~mkheader ~mkencoding ~equal ~maximum_size =
  Headered { mkheader; headerencoding; mkencoding; equal; maximum_size }
;;

type variable_size_spec =
  [ `UInt62
  | `UInt30
  | `UInt16
  | `UInt8
  ]

type size_spec =
  [ `Fixed of Sizedints.Uint62.t
  | variable_size_spec
  ]

let with_length_header
  : type a.
    lengthencoding:size_spec
    -> length:(a -> Sizedints.Uint62.t)
    -> mkencoding:(Sizedints.Uint62.t -> (a t, string) result)
    -> equal:(a -> a -> bool)
    -> maximum_size:Optint.Int63.t
    -> a t
  =
 fun ~lengthencoding ~length ~mkencoding ~equal ~maximum_size ->
  match lengthencoding with
  | `Fixed length ->
    (match mkencoding length with
     | Ok encoding -> encoding
     | Error msg ->
       raise (Invalid_argument ("data-encoding.binary.with_length_header: " ^ msg)))
  | `UInt62 ->
    with_header
      ~headerencoding:uint62
      ~mkheader:(fun v -> Ok (length v))
      ~mkencoding
      ~equal
      ~maximum_size
  | `UInt30 ->
    with_header
      ~headerencoding:uint30
      ~mkheader:(fun v ->
        let length = length v in
        if Sizedints.Uint30.(to_uint62 max_int) < length
        then Error "Length larger than header-size (30 bits) can encode"
        else
          Ok
            (Option.get
               (Sizedints.Uint30.of_int (Optint.Int63.to_int (length :> Optint.Int63.t)))))
      ~mkencoding:(fun n -> mkencoding (Sizedints.Uint30.to_uint62 n))
      ~equal
      ~maximum_size
  | `UInt16 ->
    with_header
      ~headerencoding:uint16
      ~mkheader:(fun v ->
        let length = length v in
        if Sizedints.Uint16.(to_uint62 max_int) < length
        then Error "Length larger than header-size (16 bits) can encode"
        else
          Ok
            (Option.get
               (Sizedints.Uint16.of_int (Optint.Int63.to_int (length :> Optint.Int63.t)))))
      ~mkencoding:(fun n -> mkencoding (Sizedints.Uint16.to_uint62 n))
      ~equal
      ~maximum_size
  | `UInt8 ->
    with_header
      ~headerencoding:uint8
      ~mkheader:(fun v ->
        let length = length v in
        if Sizedints.Uint8.(to_uint62 max_int) < length
        then Error "Length larger than header-size (16 bits) can encode"
        else
          Ok
            (Option.get
               (Sizedints.Uint8.of_int (Optint.Int63.to_int (length :> Optint.Int63.t)))))
      ~mkencoding:(fun n -> mkencoding (Sizedints.Uint8.to_uint62 n))
      ~equal
      ~maximum_size
;;

let with_size_header : type a. sizeencoding:variable_size_spec -> encoding:a t -> a t =
 fun ~sizeencoding ~encoding ->
  match sizeencoding with
  | `UInt62 -> Size_headered { size = UInt62; encoding }
  | `UInt30 -> Size_headered { size = UInt30; encoding }
  | `UInt16 -> Size_headered { size = UInt16; encoding }
  | `UInt8 -> Size_headered { size = UInt8; encoding }
;;

let conv ~serialisation ~deserialisation encoding =
  Conv { serialisation; deserialisation; encoding }
;;

let seq_with_length lengthencoding elementencoding =
  with_length_header
    ~lengthencoding
    ~length:(fun (s : 'f seq_with_length) -> Lazy.force s.length)
    ~mkencoding:(fun length -> Ok (LSeq { length; elementencoding }))
    ~equal:(fun a b -> Seq.equal (Query.equal_of elementencoding) a.seq b.seq)
    ~maximum_size:(Query.maximum_size_of elementencoding)
;;

let seq size_spec encoding =
  conv
    ~serialisation:(fun seq ->
      let length =
        lazy (Option.get (Sizedints.Uint62.of_int64 (Int64.of_int (Seq.length seq))))
      in
      { seq; length })
    ~deserialisation:(fun { seq; length = _ } -> Ok seq)
    (seq_with_length size_spec encoding)
;;

let seq_with_size sizeencoding elementencoding =
  with_size_header ~sizeencoding ~encoding:(USeq { elementencoding })
;;

let list size_spec encoding =
  conv
    ~serialisation:(fun l ->
      let length =
        lazy (Option.get (Sizedints.Uint62.of_int64 (Int64.of_int (List.length l))))
      in
      { seq = List.to_seq l; length })
    ~deserialisation:(fun { seq; length = _ } -> Ok (List.of_seq seq))
    (seq_with_length size_spec encoding)
;;

let fold ~chunkencoding ~chunkify ~readinit ~reducer ~equal ~maximum_size =
  Fold { chunkencoding; chunkify; readinit; reducer; equal; maximum_size }
;;

let array : type a. size_spec -> a t -> a array t =
 fun lengthencoding elementencoding ->
  with_length_header
    ~lengthencoding
    ~length:(fun a ->
      Option.get (Sizedints.Uint62.of_int64 (Int64.of_int (Array.length a))))
    ~mkencoding:(fun length -> Ok (Array { length; elementencoding }))
    ~equal:(fun xs ys ->
      Array.length xs = Array.length ys
      && Array.for_all2 (Query.equal_of elementencoding) xs ys)
    ~maximum_size:
      (let maximum_length =
         match lengthencoding with
         | `Fixed n -> n
         | `UInt8 -> Sizedints.Uint8.(to_uint62 max_int)
         | `UInt16 -> Sizedints.Uint16.(to_uint62 max_int)
         | `UInt30 -> Sizedints.Uint30.(to_uint62 max_int)
         | `UInt62 -> Sizedints.Uint62.max_int
       in
       Optint.Int63.mul
         (maximum_length :> Optint.Int63.t)
         (Query.maximum_size_of elementencoding))
;;

let string lengthencoding =
  with_length_header
    ~lengthencoding
    ~length:(fun s ->
      Option.get (Sizedints.Uint62.of_int64 (Int64.of_int (String.length s))))
    ~mkencoding:(fun n -> Ok (String n))
    ~equal:String.equal
    ~maximum_size:
      (match lengthencoding with
       | `Fixed n -> (n :> Optint.Int63.t)
       | `UInt8 -> (Sizedints.Uint8.(to_uint62 max_int) :> Optint.Int63.t)
       | `UInt16 -> (Sizedints.Uint16.(to_uint62 max_int) :> Optint.Int63.t)
       | `UInt30 -> (Sizedints.Uint30.(to_uint62 max_int) :> Optint.Int63.t)
       | `UInt62 -> (Sizedints.Uint62.max_int :> Optint.Int63.t))
;;

let bytes lengthencoding =
  with_length_header
    ~lengthencoding
    ~length:(fun b ->
      Option.get (Sizedints.Uint62.of_int64 (Int64.of_int (Bytes.length b))))
    ~mkencoding:(fun n -> Ok (Bytes n))
    ~equal:Bytes.equal
    ~maximum_size:
      (match lengthencoding with
       | `Fixed n -> (n :> Optint.Int63.t)
       | `UInt8 -> (Sizedints.Uint8.(to_uint62 max_int) :> Optint.Int63.t)
       | `UInt16 -> (Sizedints.Uint16.(to_uint62 max_int) :> Optint.Int63.t)
       | `UInt30 -> (Sizedints.Uint30.(to_uint62 max_int) :> Optint.Int63.t)
       | `UInt62 -> (Sizedints.Uint62.max_int :> Optint.Int63.t))
;;

let ellastic_uint30 : Sizedints.Uint30.t t =
  let payload_mask = (* significant bits of each byte *) 0b0111_1111 in
  let tag_mask = (* metadata bits of each byte *) 0b1000_0000 in
  let payload_width = (* number of significant bits in each byte of payload *) 7 in
  fold
    ~chunkencoding:uint8
    ~chunkify:(fun (u30 : Sizedints.Uint30.t) ->
      let u30 = (u30 :> int) in
      let rec chunkify u30 () =
        assert (u30 >= 0);
        if u30 land payload_mask = u30
        then (
          let chunk =
            (* TODO: optimise by avoiding boxing w/ option *)
            Option.get @@ Sizedints.Uint8.of_int u30
          in
          Seq.Cons (chunk, Seq.empty))
        else (
          let chunk = u30 land payload_mask lor tag_mask in
          let chunk =
            (* TODO: optimise by avoiding boxing w/ option *)
            Option.get @@ Sizedints.Uint8.of_int chunk
          in
          let rest = u30 lsr payload_width in
          Seq.Cons (chunk, chunkify rest))
      in
      chunkify u30)
    ~readinit:(0, 0)
    ~reducer:(fun (acc, shift) chunk ->
      let chunk = (chunk :> int) in
      let acc = acc lor ((chunk land payload_mask) lsl shift) in
      if chunk land tag_mask = 0
      then
        (* TODO: handle errors here *)
        (* TODO: check overflow as we go along to avoid decoding too many bytes;
         maybe limit the number of chunks too? *)
        Finish (Option.get @@ Sizedints.Uint30.of_int acc)
      else K (acc, shift + payload_width))
    ~equal:(fun a b -> Int.equal (a :> int) (b :> int))
    ~maximum_size:(Optint.Int63.of_int 5)
;;
