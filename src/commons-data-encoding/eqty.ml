type (_, _) eq = Eq : ('a, 'a) eq
type 'a witness = ..

module type S = sig
  type t

  val witness : t witness
  val eq : 'a witness -> ('a, t) eq option
end

type 'a t = (module S with type t = 'a)

let make (type a) () =
  let module Ty = struct
    type t = a
    type 'a witness += Ty : t witness

    let witness = Ty

    let eq (type b) : b witness -> (b, t) eq option = function
      | Ty -> Some Eq
      | _ -> None
    ;;
  end
  in
  (module Ty : S with type t = a)
;;

let eq : type a b. a t -> b t -> (a, b) eq option =
 fun (module TyA) (module TyB) -> TyB.eq TyA.witness
;;
