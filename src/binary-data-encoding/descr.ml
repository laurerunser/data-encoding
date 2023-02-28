module Hlist = Commons.Hlist
module Sizedints = Commons.Sizedints

type ('step, 'finish) reducer =
  | K of 'step
  | Finish of 'finish

type _ numeral =
  | UInt8 : Sizedints.Uint8.t numeral
  | UInt16 : Sizedints.Uint16.t numeral
  | UInt30 : Sizedints.Uint30.t numeral
  | UInt62 : Sizedints.Uint62.t numeral
  | Int32 : int32 numeral
  | Int64 : int64 numeral

type endianness =
  | Big_endian
  | Little_endian

type 'a seq_with_length =
  { seq : 'a Seq.t
  ; length : Sizedints.Uint62.t Lazy.t
  }

type _ t =
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
  | Union :
      { tag : 'tag t
      ; serialisation : 'a -> ('tag, 'a) anycaseandpayload
      ; deserialisation : 'tag -> (('tag, 'a) anycase, string) result
      ; cases : ('tag, 'a) anycase list
      }
      -> 'a t
  | [] : unit Hlist.t t
  | ( :: ) : 'a t * 'b Hlist.t t -> ('a * 'b) Hlist.t t

and ('tag, 'payload, 'union) case_descr =
  { tag : 'tag
  ; encoding : 'payload t
  ; inject : 'payload -> 'union
  }

and ('tag, 'p, 'a) case_and_payload = ('tag, 'p, 'a) case_descr * 'p

and ('tag, 'a) anycaseandpayload =
  | AnyP : ('tag, _, 'a) case_and_payload -> ('tag, 'a) anycaseandpayload

and ('tag, 'a) anycase = AnyC : ('tag, _, 'a) case_descr -> ('tag, 'a) anycase

(* In order to avoid some complications w.r.t. inter-module dependencies and in
   order to allow the (local) opening of the [Encoding] module (necessary to get
   access to the list-constructor-based products), we declare the base types in
   this module (accessible everywhere), and we reexport the types and
   constructors in [Encoding].

   E.g., this allows [Query] to access [Descr] and [Encoding] to access [Query].
*)
