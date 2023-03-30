(** {1: Sizes as known by the type system}

    The encodings of binary-data-encoding have a type parameter indicating their
    sizability: the kind of sizing they have.

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

type ('l, 'r, 's) tupler =
  | TExtrinsicStatic : (extrinsic, static, extrinsic) tupler
  | TIntrinsicExtrinsic : ('s intrinsic, extrinsic, extrinsic) tupler
  | TStaticIntrinsic : (static, 's intrinsic, 's intrinsic) tupler
  | TDynamicIntrinsic : (dynamic, 's intrinsic, dynamic) tupler

type ('x, 'xo) optioner =
  | OIntrinsic : ('s intrinsic, dynamic) optioner
  | OExtrinsic : (extrinsic, extrinsic) optioner
