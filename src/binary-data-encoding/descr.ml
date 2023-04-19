module Hlist = Commons.Hlist
module Sizedints = Commons.Sizedints

type ('step, 'finish) reducer =
  | K of 'step
  | Finish of 'finish

type 'a numeral = 'a Sizedints.numeral =
  | Uint8 : Sizedints.Uint8.t numeral
  | Uint16 : Sizedints.Uint16.t numeral
  | Uint30 : Sizedints.Uint30.t numeral
  | Uint62 : Sizedints.Uint62.t numeral
  | Int32 : int32 numeral
  | Int64 : int64 numeral

type endianness =
  | Big_endian
  | Little_endian

type 'a seq_with_length =
  { seq : 'a Seq.t
  ; length : Sizedints.Uint62.t Lazy.t
  }

(** {1: "The GADT"}

    This GADT describes OCaml values and their representation. E.g.,
    [Numeral { numeral = Int64; endianness = Big_endian }] describes an OCaml
    value of the type [int64] represented in big-endian form. On top of this
    descriptions are serialisation functions (see module [Writer]) and
    deserialisation functions (see module [Reader]) and some other helpers (see
    other modules).

    This is a low-level description used as the shared foundation for the
    binary-data-encoding library. It is not meant for casual users (who should
    use the [Encoding] module instead) but is still usable by advanced users.

    The first type parameter of this GADT describes the sizability of the
    representation. See module [Sizability] for more info.

    The second type parameter of this GADT describes the type of OCaml values.

    E.g., a [(dynamic, int array) t] describes integer arrays which are
    represented in a dynamically-sized form.

    {2: Design}

    This GADT is designed with two conflicting goals in mind: simplicity and
    efficiency. For simplicity, we want to keep the GADT small with a few
    expressive cases. This reduces the amount of code in all the functions which
    match on this GADT ([Writer.writek], [Reader.readk], a lot of functions in
    [Query], etc.). For efficiency, we want to keep the de/serialisation process
    fast.

    E.g., string-vs-bytes: OCaml has two distinct types for mutable and
    immutable strings of bytes. They are represented identically in the language
    runtime and in the serialised form produced by binary-data-encoding. We
    could have a single constructor, say [String], and represent bytes with
{[
Conv
  { descr = String
  ; serialisation = Bytes.unsafe_to_string
  ; deserialisation = Bytes.unsafe_of_string }
]}
    However, in doing so the serialisation process needs to traverse two
    constructors ([Conv] and [String]) and apply a function. We instead have
    two distinct constructors ([String] and [Bytes]) which leads to some
    (near) duplication in the code of the functions that match this AST.

    {2: Organisation}

    The GADT has constructors for:

    - ground types
    - product types and sum types
    - parametric types
    - special uses (for advanced features)

    The constructors appear in this order for readability. *)
type (_, _) t =
  | Unit : (Sizability.static, unit) t
  | Bool : (Sizability.static, bool) t
  | Numeral :
      { numeral : 'a numeral
      ; endianness : endianness
      }
      -> (Sizability.static, 'a) t
  | String : Sizedints.Uint62.t -> (Sizability.static, string) t
      (** The constructor's parameter is the length of the string *)
  | Bytes : Sizedints.Uint62.t -> (Sizability.static, bytes) t
      (** The constructor's parameter is the length of the bytes *)
  | TupNil : (Sizability.static, unit Hlist.t) t (** The empty tuple *)
  | TupCons :
      { tupler : ('l, 'r, 's) Sizability.tupler
      ; head : ('l, 'a) t
      ; tail : ('r, 'b Hlist.t) t
      }
      -> ('s, ('a * 'b) Hlist.t) t
      (** The tuple constructor. The [tupler] parameter enforces the tuple is
         deserialisable and keeps track of the sizability of the result. *)
  | Union :
      { tag : (Sizability.static, 'tag) t
      ; serialisation : 'a -> ('tag, 'a) anycaseandpayload
      ; deserialisation : 'tag -> (('tag, 'a) anycase, string) result
      ; cases : ('tag, 'a) anycase list
      }
      -> (* TODO? support dynamically sized tags? *)
         (* if all the [cases] have descrs of the same static size then the
           descr is static, but this level of fined-grained tracking requires
           dependent types. *)
      (Sizability.dynamic, 'a) t
  | Array :
      { length : Sizedints.Uint62.t
      ; descr : ('s Sizability.intrinsic, 'a) t
      }
      -> ('s Sizability.intrinsic, 'a array) t
      (** The [length] parameter is the number of elements in the array. *)
  | LSeq :
      { length : Sizedints.Uint62.t
      ; descr : ('s Sizability.intrinsic, 'a) t
      }
      -> ('s Sizability.intrinsic, 'a seq_with_length) t
      (** The [length] parameter is the number of elements in the sequence. *)
  | Option :
      { optioner : ('s, 'ss) Sizability.optioner
      ; descr : ('s, 'a) t
      }
      -> ('ss, 'a option) t
  | USeq :
      { descr : ('s Sizability.intrinsic, 'a) t }
      -> (* INVARIANT (not encoded in the type-system):
            - [descr] cannot take zero-bytes of space otherwise it is
              impossible to distinguish sequences of different lengths.
            - To encode this into the type system we need to make it possible to
              distinguish zero-byte and non-zero-byte descr. But doing so
              requires to make the distinction everywhere. This is problematic
              for, amongst other things, [String n] where the zero-byte-ability
              would depend on the value of [n]. *)
      (Sizability.extrinsic, 'a Seq.t) t
  | Headered :
      { mkheader : 'a -> ('header, string) result
      ; headerdescr : ('s Sizability.intrinsic, 'header) t
      ; descr_of_header : 'header -> ('a anyintrinsic, string) result
      ; equal : 'a -> 'a -> bool
      ; maximum_size : Optint.Int63.t (* the max size of the payload *)
      }
      -> (* if [headerdescr] is static and [descr_of_header] returns all descrs
         of the same static size then the descr is static, but this level of
         fined-grained tracking requires dependent types. *)
      (Sizability.dynamic, 'a) t
  | Fold :
      { chunkdescr : ('s Sizability.intrinsic, 'chunk) t
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
      ; descr : ('s, 'b) t
      }
      -> ('s, 'a) t
  | Size_headered :
      { size : _ numeral
      ; descr : (Sizability.extrinsic, 'a) t
      }
      -> (* NOTE: it's only possible to add a size-header to an extrinsic
           descr *)
      (Sizability.dynamic, 'a) t
  | Size_limit :
      { at_most : Sizedints.Uint62.t
      ; descr : ('s, 'a) t
      }
      -> ('s, 'a) t

(* existential to allow returning either of the intrinsic forms *)
and 'a anyintrinsic =
  | EDynamic : (Sizability.dynamic, 'a) t -> 'a anyintrinsic
  | EStatic : (Sizability.static, 'a) t -> 'a anyintrinsic

and ('s, 'tag, 'payload, 'union) case_descr =
  { tag : 'tag
  ; descr : ('s Sizability.intrinsic, 'payload) t
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

type 'a introspectable = E : (_ Sizability.t, 'a) t -> 'a introspectable
