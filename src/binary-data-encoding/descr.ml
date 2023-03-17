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

type (_, _) t =
  | Unit : (Sizability.static, unit) t
  | Bool : (Sizability.static, bool) t
  | Numeral :
      { numeral : 'a numeral
      ; endianness : endianness
      }
      -> (Sizability.static, 'a) t
  | String : Sizedints.Uint62.t -> (Sizability.static, string) t
  | Bytes : Sizedints.Uint62.t -> (Sizability.static, bytes) t
  | Array :
      { length : Sizedints.Uint62.t
      ; elementencoding : ('s Sizability.intrinsic, 'a) t
      }
      -> ('s Sizability.intrinsic, 'a array) t
  | LSeq :
      { length : Sizedints.Uint62.t
      ; elementencoding : ('s Sizability.intrinsic, 'a) t
      }
      -> ('s Sizability.intrinsic, 'a seq_with_length) t
  | USeq :
      { elementencoding : ('s Sizability.intrinsic, 'a) t }
      -> (* INVARIANT (not encoded in the type-system): elementencoding cannot take
         zero-bytes of space otherwise it is impossible to distinguish sequences
         of different lengths *)
      (Sizability.extrinsic, 'a Seq.t) t
  | Option : ('s, 'ss) Sizability.optioner * ('s, 'a) t -> ('ss, 'a option) t
  | Headered :
      { mkheader : 'a -> ('header, string) result
      ; headerencoding : ('s Sizability.intrinsic, 'header) t
      ; mkencoding : 'header -> ('a anyintrinsic, string) result
      ; equal : 'a -> 'a -> bool
      ; maximum_size : Optint.Int63.t (* the max size of the payload *)
      }
      -> (* if [headerencoding] is static and [mkencoding] returns all encodings of
         the same static size then the encoding is static, but this level of
         fined-grained tracking requires dependent types. *)
      (Sizability.dynamic, 'a) t
  | Fold :
      { chunkencoding : ('s Sizability.intrinsic, 'chunk) t
      ; chunkify : 'a -> 'chunk Seq.t
      ; readinit : 'acc
      ; reducer : 'acc -> 'chunk -> ('acc, 'a) reducer
      ; equal : 'a -> 'a -> bool
      ; maximum_size : Optint.Int63.t
      }
      -> (Sizability.dynamic, 'a) t
  | Conv :
      { serialisation : 'a -> 'b
      ; deserialisation : 'b -> ('a, string) result
      ; encoding : ('s, 'b) t
      }
      -> ('s, 'a) t
  | Size_headered :
      { size : _ numeral
      ; encoding : (Sizability.extrinsic, 'a) t
      }
      -> (* NOTE: it's only possible to add a size-header to an extrinsic
           encoding *)
      (Sizability.dynamic, 'a) t
  | Size_limit :
      { at_most : Sizedints.Uint62.t
      ; encoding : ('s, 'a) t
      }
      -> ('s, 'a) t
  | Union :
      { tag : (Sizability.static, 'tag) t
      ; serialisation : 'a -> ('tag, 'a) anycaseandpayload
      ; deserialisation : 'tag -> (('tag, 'a) anycase, string) result
      ; cases : ('tag, 'a) anycase list
      }
      -> (* TODO? support dynamically sized tags? *)
         (* if all the [cases] have encodings of the same static size then the
           encoding is static, but this level of fined-grained tracking requires
           dependent types. *)
      (Sizability.dynamic, 'a) t
  | TupNil : (Sizability.static, unit Hlist.t) t
  | TupCons :
      ('l, 'r, 's) Sizability.tupler * ('l, 'a) t * ('r, 'b Hlist.t) t
      -> ('s, ('a * 'b) Hlist.t) t

(* existential to allow returning either of the intrinsic forms *)
and 'a anyintrinsic =
  | EDynamic : (Sizability.dynamic, 'a) t -> 'a anyintrinsic
  | EStatic : (Sizability.static, 'a) t -> 'a anyintrinsic

and ('s, 'tag, 'payload, 'union) case_descr =
  { tag : 'tag
  ; encoding : ('s Sizability.intrinsic, 'payload) t
  ; inject : 'payload -> 'union
  }

and ('tag, 'a) anycaseandpayload =
  | AnyP : (_, 'tag, 'p, 'a) case_descr * 'p -> ('tag, 'a) anycaseandpayload

and ('tag, 'a) anycase = AnyC : (_, 'tag, _, 'a) case_descr -> ('tag, 'a) anycase

(* In order to avoid some complications w.r.t. inter-module dependencies and in
   order to allow the (local) opening of the [Encoding] module (necessary to get
   access to the list-constructor-based products), we declare the base types in
   this module (accessible everywhere), and we reexport the types and
   constructors in [Encoding].

   E.g., this allows [Query] to access [Descr] and [Encoding] to access [Query].
*)
