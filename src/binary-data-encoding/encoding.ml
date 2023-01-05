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
  ; mutable len : int option
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
  | Seq :
      { encoding : 'a t
      ; length : Sizedints.Uint62.t
      }
      -> 'a seq_with_length t
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

include Big_endian (* default *)

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

type size_spec =
  [ `Fixed of Sizedints.Uint62.t
  | `UInt62
  | `UInt30
  | `UInt16
  | `UInt8
  ]

let with_length_header
    : type a.
      lengthencoding:
        [ `Fixed of Sizedints.Uint62.t | `UInt62 | `UInt30 | `UInt16 | `UInt8 ]
      -> length:(a -> int)
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
      ~mkheader:(fun v ->
        match Sizedints.Uint62.of_int64 (Int64.of_int (length v)) with
        | None -> Error "Length larger than header-size (62 bits) can encode"
        | Some n -> Ok n)
      ~mkencoding
      ~equal
      ~maximum_size
  | `UInt30 ->
    with_header
      ~headerencoding:uint30
      ~mkheader:(fun v ->
        match Sizedints.Uint30.of_int (length v) with
        | None -> Error "Length larger than header-size (30 bits) can encode"
        | Some n -> Ok n)
      ~mkencoding:(fun n -> mkencoding (Sizedints.Uint30.to_uint62 n))
      ~equal
      ~maximum_size
  | `UInt16 ->
    with_header
      ~headerencoding:uint16
      ~mkheader:(fun v ->
        match Sizedints.Uint16.of_int (length v) with
        | None -> Error "Length larger than header-size (16 bits) can encode"
        | Some n -> Ok n)
      ~mkencoding:(fun n -> mkencoding (Sizedints.Uint16.to_uint62 n))
      ~equal
      ~maximum_size
  | `UInt8 ->
    with_header
      ~headerencoding:uint8
      ~mkheader:(fun v ->
        match Sizedints.Uint8.of_int (length v) with
        | None -> Error "Length larger than header-size (8 bits) can encode"
        | Some n -> Ok n)
      ~mkencoding:(fun n -> mkencoding (Sizedints.Uint8.to_uint62 n))
      ~equal
      ~maximum_size
;;

let conv ~serialisation ~deserialisation encoding =
  Conv { serialisation; deserialisation; encoding }
;;

let length s =
  match s.len with
  | Some len -> len
  | None ->
    let len = Seq.length s.seq in
    s.len <- Some len;
    len
;;

let seq_with_length encoding lengthencoding =
  with_length_header
    ~lengthencoding
    ~length
    ~mkencoding:(fun length -> Ok (Seq { encoding; length }))
    ~equal:(fun a b -> Seq.equal (Query.equal_of encoding) a.seq b.seq)
    ~maximum_size:(Query.maximum_size_of encoding)
;;

let seq encoding size_spec =
  conv
    ~serialisation:(fun seq -> { seq; len = None })
    ~deserialisation:(fun { seq; len = _ } -> Ok seq)
    (seq_with_length encoding size_spec)
;;

let list encoding size_spec =
  conv (* TODO: compute length during conversion list -> seq ? *)
    ~serialisation:(fun l -> { seq = List.to_seq l; len = None })
    ~deserialisation:(fun { seq; len = _ } -> Ok (List.of_seq seq))
    (seq_with_length encoding size_spec)
;;

let fold ~chunkencoding ~chunkify ~readinit ~reducer ~equal ~maximum_size =
  Fold { chunkencoding; chunkify; readinit; reducer; equal; maximum_size }
;;

let array
    : type a.
      [ `Fixed of Sizedints.Uint62.t | `UInt62 | `UInt30 | `UInt16 | `UInt8 ]
      -> a t
      -> a array t
  =
 fun lengthencoding elementencoding ->
  with_length_header
    ~lengthencoding
    ~length:Array.length
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
    ~length:String.length
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
    ~length:Bytes.length
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
