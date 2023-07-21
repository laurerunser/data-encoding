type 'v k = 'v Eqty.t
type kv = Anykv : ('v k * 'v) -> kv

let k () = Eqty.make ()

module Anyeqty = struct
  type t = Any : _ Eqty.t -> t

  let compare a b = compare a b
end

module M = Map.Make (Anyeqty)

type t = kv M.t

let empty = M.empty

let find : type v. t -> v k -> v option =
 fun t k ->
  match M.find_opt (Anyeqty.Any k) t with
  | None -> None
  | Some (Anykv (kk, v)) ->
    (match Eqty.eq k kk with
     | None -> assert false
     | Some Eq -> Some v)
;;

let add : type v. t -> v k -> v -> t = fun t k v -> M.add (Anyeqty.Any k) (Anykv (k, v)) t
