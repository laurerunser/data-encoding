module Hlist = Commons.Hlist
module Sizedints = Commons.Sizedints

type ('step, 'finish) reducer =
  | K of 'step
  | Finish of 'finish

type _ t =
  | Unit : unit t
  | Bool : bool t
  | UInt8 : Sizedints.Uint8.t t
  | UInt16 : Sizedints.Uint16.t t
  | UInt30 : Sizedints.Uint30.t t
  | UInt62 : Sizedints.Uint62.t t
  | Int32 : int32 t
  | Int64 : int64 t
  | String : Sizedints.Uint62.t -> string t
  | Bytes : Sizedints.Uint62.t -> bytes t
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
let int64 = Int64
let int32 = Int32
let uint30 = UInt30
let uint62 = UInt62
let uint16 = UInt16
let uint8 = UInt8
let option t = Option t

let with_header ~headerencoding ~mkheader ~mkencoding ~equal ~maximum_size =
  Headered { mkheader; headerencoding; mkencoding; equal; maximum_size }
;;

let string = function
  | `Fixed u -> String u
  | `UInt62 ->
    with_header
      ~headerencoding:UInt62
      ~mkheader:(fun v ->
        let len = String.length v in
        match Sizedints.Uint62.of_int64 (Int64.of_int len) with
        | None -> Error "String larger than header-size can encode"
        | Some n -> Ok n)
      ~mkencoding:(fun n -> Ok (String n))
      ~equal:String.equal
      ~maximum_size:(Sizedints.Uint62.max_int :> Optint.Int63.t)
  | `UInt30 ->
    with_header
      ~headerencoding:UInt30
      ~mkheader:(fun v ->
        let len = String.length v in
        match Sizedints.Uint30.of_int len with
        | None -> Error "String larger than header-size can encode"
        | Some n -> Ok n)
      ~mkencoding:(fun n -> Ok (String (Sizedints.Uint30.to_uint62 n)))
      ~equal:String.equal
      ~maximum_size:(Sizedints.Uint30.(to_uint62 max_int) :> Optint.Int63.t)
  | `UInt16 ->
    with_header
      ~headerencoding:UInt16
      ~mkheader:(fun v ->
        let len = String.length v in
        match Sizedints.Uint16.of_int len with
        | None -> Error "String larger than header-size can encode"
        | Some n -> Ok n)
      ~mkencoding:(fun n -> Ok (String (Sizedints.Uint16.to_uint62 n)))
      ~equal:String.equal
      ~maximum_size:(Sizedints.Uint16.(to_uint62 max_int) :> Optint.Int63.t)
  | `UInt8 ->
    with_header
      ~headerencoding:UInt8
      ~mkheader:(fun v ->
        let len = String.length v in
        match Sizedints.Uint8.of_int len with
        | None -> Error "String larger than header-size can encode"
        | Some n -> Ok n)
      ~mkencoding:(fun n -> Ok (String (Sizedints.Uint8.to_uint62 n)))
      ~equal:String.equal
      ~maximum_size:(Sizedints.Uint8.(to_uint62 max_int) :> Optint.Int63.t)
;;

let bytes = function
  | `Fixed u -> Bytes u
  | `UInt62 ->
    with_header
      ~headerencoding:UInt62
      ~mkheader:(fun v ->
        match Sizedints.Uint62.of_int64 (Int64.of_int (Bytes.length v)) with
        | None -> Error "Bytes larger than header-size can encode"
        | Some n -> Ok n)
      ~mkencoding:(fun n -> Ok (Bytes n))
      ~equal:Bytes.equal
      ~maximum_size:(Sizedints.Uint62.max_int :> Optint.Int63.t)
  | `UInt30 ->
    with_header
      ~headerencoding:UInt30
      ~mkheader:(fun v ->
        match Sizedints.Uint30.of_int (Bytes.length v) with
        | None -> Error "Bytes larger than header-size can encode"
        | Some n -> Ok n)
      ~mkencoding:(fun n -> Ok (Bytes (Sizedints.Uint30.to_uint62 n)))
      ~equal:Bytes.equal
      ~maximum_size:(Sizedints.Uint30.(to_uint62 max_int) :> Optint.Int63.t)
  | `UInt16 ->
    with_header
      ~headerencoding:UInt16
      ~mkheader:(fun v ->
        match Sizedints.Uint16.of_int (Bytes.length v) with
        | None -> Error "Bytes larger than header-size can encode"
        | Some n -> Ok n)
      ~mkencoding:(fun n -> Ok (Bytes (Sizedints.Uint16.to_uint62 n)))
      ~equal:Bytes.equal
      ~maximum_size:(Sizedints.Uint16.(to_uint62 max_int) :> Optint.Int63.t)
  | `UInt8 ->
    with_header
      ~headerencoding:UInt8
      ~mkheader:(fun v ->
        match Sizedints.Uint8.of_int (Bytes.length v) with
        | None -> Error "Bytes larger than header-size can encode"
        | Some n -> Ok n)
      ~mkencoding:(fun n -> Ok (Bytes (Sizedints.Uint8.to_uint62 n)))
      ~equal:Bytes.equal
      ~maximum_size:(Sizedints.Uint8.(to_uint62 max_int) :> Optint.Int63.t)
;;

let conv ~serialisation ~deserialisation encoding =
  Conv { serialisation; deserialisation; encoding }
;;

let fold ~chunkencoding ~chunkify ~readinit ~reducer ~equal ~maximum_size =
  Fold { chunkencoding; chunkify; readinit; reducer; equal; maximum_size }
;;

let ellastic_uint30 : Sizedints.Uint30.t t =
  let payload_mask = (* significant bits of each byte *) 0b0111_1111 in
  let tag_mask = (* metadata bits of each byte *) 0b1000_0000 in
  let payload_width = (* number of significant bits in each byte of payload *) 7 in
  fold
    ~chunkencoding:UInt8
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
