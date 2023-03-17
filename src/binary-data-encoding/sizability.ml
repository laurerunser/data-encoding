(* intermediate types *)
type s_static = SStatic
type s_dynamic = SDynamic

type _ s_intrinsic =
  | Static : Commons.Sizedints.Uint62.t -> s_static s_intrinsic
  | Dynamic : s_dynamic s_intrinsic

type s_extrinsic = SExtrinsic

(* main type *)
type _ t =
  | Intrinsic : 'a s_intrinsic -> 'a s_intrinsic t
  | Extrinsic : s_extrinsic t

(* aliases *)
type static = s_static s_intrinsic t
type dynamic = s_dynamic s_intrinsic t
type 'a intrinsic = 'a s_intrinsic t
type extrinsic = s_extrinsic t

(* universal quantifier *)
type s = S : _ t -> s

(* et là tu pleure *)
type ('l, 'r, 's) tupler =
  | TExtrinsicStatic : (extrinsic, static, extrinsic) tupler
  | TIntrinsicExtrinsic : ('s intrinsic, extrinsic, extrinsic) tupler
  | TStaticIntrinsic : (static, 's intrinsic, 's intrinsic) tupler
  | TDynamicIntrinsic : (dynamic, 's intrinsic, dynamic) tupler

type ('x, 'xo) optioner =
  | OIntrinsic : ('s intrinsic, dynamic) optioner
  | OExtrinsic : (extrinsic, extrinsic) optioner
