(** {1 Sizes as known by the type system}

    The descriptions of values in binary-data-encoding ({!Descr.t}) have a
    type parameter indicating their sizability: the kind of sizing they have.

    - Static: A description of statically known size (e.g., a tuple of two int64
      takes 16 bytes).

    - Dynmamic: A description with a size that can be recovered by the content of
      the byte representation of the value (e.g., a list of elements with a size
      header)

    - Extrinsic: A description where the size cannot be determined by the byte
      content and must instead be provided extrinsically (e.g., a list of
      elements without a size header)

    Note that Extrinsic descriptions are mostly used as building-blocks for
    Dynamic descriptions.

    Also note that this is the sizability as tracked by the type system. There
    are limits to the expressiveness we can achieve. *)

(** {2 Intermediate types}

    To be used as parameters later on. *)

type s_static = SStatic
type s_dynamic = SDynamic

type _ s_intrinsic =
  | Static : Commons.Sizedints.Uint62.t -> s_static s_intrinsic
  | Dynamic : s_dynamic s_intrinsic

type s_extrinsic = SExtrinsic

(** {2 Main type for sizability} *)
type _ t =
  | Intrinsic : 'a s_intrinsic -> 'a s_intrinsic t
  | Extrinsic : s_extrinsic t

(** {2 Aliases to avoid verbosit} *)

type static = s_static s_intrinsic t
type dynamic = s_dynamic s_intrinsic t
type 'a intrinsic = 'a s_intrinsic t
type extrinsic = s_extrinsic t

(** {2 Universal quantifier} *)
type s = S : _ t -> s

(** {2 Constructors for tracking sizability} *)

(** A [tupler] is a value which

    - proves that two encodings can be concatenated into a tuple, and
    - keeps track of the sizability of the concatenated tuple.

    E.g., it is not possible to have an extrinsic before a dynamic: the
    deserialisation process wouldn't know where the former ends and the latter
    starts. *)
type ('l, 'r, 's) tupler =
  | TAnyStatic : ('any t, static, 'any t) tupler
  | TIntrinsicExtrinsic : (_ intrinsic, extrinsic, extrinsic) tupler
  | TIntrinsicDynamic : (_ intrinsic, dynamic, dynamic) tupler

(** An [optioner] is a value which keeps track of the sizability of the option. *)
type ('x, 'xo) optioner =
  | OIntrinsic : ('s intrinsic, dynamic) optioner
  | OExtrinsic : (extrinsic, extrinsic) optioner
