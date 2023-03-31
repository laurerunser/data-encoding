(** {1: Sizes as known by the type system}

    The encoding (more precisely, the [Descr.t]) of binary-data-encoding have a
    type parameter indicating their sizability: the kind of sizing they have.

    - Static: An encoding of statically known size (e.g., a tuple of two int64)

    - Dynmamic: An encoding with a size that can be recovered by the content of
      the byte representation of the value (e.g., a list of elements with a size
      header)

    - Extrinsic: An encoding where the size cannot be determined by the byte
      content and must instead be provided extrinsically (e.g., a list of
      elements without a size header) *)

(** Intermediate types to use as parameters later on. *)
type s_static = SStatic

type s_dynamic = SDynamic

type _ s_intrinsic =
  | Static : Commons.Sizedints.Uint62.t -> s_static s_intrinsic
  | Dynamic : s_dynamic s_intrinsic

type s_extrinsic = SExtrinsic

(** main type *)
type _ t =
  | Intrinsic : 'a s_intrinsic -> 'a s_intrinsic t
  | Extrinsic : s_extrinsic t

(** aliases *)
type static = s_static s_intrinsic t

type dynamic = s_dynamic s_intrinsic t
type 'a intrinsic = 'a s_intrinsic t
type extrinsic = s_extrinsic t

(** universal quantifier *)
type s = S : _ t -> s

(** A [tupler] is a value which

    - proves that two encodings can be concatenated into a tuple, and
    - keeps track of the sizability of the concatenated tuple.

    E.g., it is not possible to have an intrinsic before a dynamic: the
    deserialisation process wouldn't know where the former ends and the latter
    starts. *)
type ('l, 'r, 's) tupler =
  | TExtrinsicStatic : (extrinsic, static, extrinsic) tupler
  | TIntrinsicExtrinsic : ('s intrinsic, extrinsic, extrinsic) tupler
  | TStaticIntrinsic : (static, 's intrinsic, 's intrinsic) tupler
  | TDynamicIntrinsic : (dynamic, 's intrinsic, dynamic) tupler

(** An [optioner] is a value which keeps track of the sizability of the option. *)
type ('x, 'xo) optioner =
  | OIntrinsic : ('s intrinsic, dynamic) optioner
  | OExtrinsic : (extrinsic, extrinsic) optioner
