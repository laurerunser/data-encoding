type ('step, 'finish) reducer =
  | K of 'step
  | Finish of 'finish

type _ numeral =
  | UInt8 : Commons.Sizedints.Uint8.t numeral
  | UInt16 : Commons.Sizedints.Uint16.t numeral
  | UInt30 : Commons.Sizedints.Uint30.t numeral
  | UInt62 : Commons.Sizedints.Uint62.t numeral
  | Int32 : int32 numeral
  | Int64 : int64 numeral

type endianness =
  | Big_endian
  | Little_endian

type 'a seq_with_length =
  { seq : 'a Seq.t
  ; mutable len : int option
  }

type _ t =
  | Unit : unit t
  | Bool : bool t
  | Numeral :
      { numeral : 'a numeral
      ; endianness : endianness
      }
      -> 'a t
  | String : Commons.Sizedints.Uint62.t -> string t
  | Bytes : Commons.Sizedints.Uint62.t -> bytes t
  | Array :
      { length : Commons.Sizedints.Uint62.t
      ; elementencoding : 'a t
      }
      -> 'a array t
  | Seq :
      { encoding : 'a t
      ; length : Commons.Sizedints.Uint62.t
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
  | [] : unit Commons.Hlist.t t
  | ( :: ) : 'a t * 'b Commons.Hlist.t t -> ('a * 'b) Commons.Hlist.t t

(* In order to avoid some complications w.r.t. inter-module dependencies and in
   order to allow the (local) opening of the [Encoding] module (necessary to get
   access to the list-constructor-based products), we declare the base types in
   this module (accessible everywhere), and we reexport the types and
   constructors in [Encoding].

   E.g., this allows [Query] to access [Descr] and [Encoding] to access [Query].
*)
