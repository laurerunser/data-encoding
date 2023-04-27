(* TODO: tests *)

module type S = sig
  type key
  type value

  val bound : int
  val put : key -> value -> unit
  val find : key -> value option
end

type ('k, 'v) t = (module S with type key = 'k and type value = 'v)

let make : type k v. (module Hashtbl.HashedType with type t = k) -> int -> (k, v) t =
 fun (module H) bound ->
  assert (bound > 0);
  let module Ht : S with type key = k and type value = v = struct
    type key = k
    type value = v

    let bound = bound

    module Ht = Hashtbl.Make (H)

    let ht : value Ht.t = Ht.create (min bound 8)
    let q : key Queue.t = Queue.create ()
    let find k = Ht.find_opt ht k

    let put k v =
      match Ht.find_opt ht k with
      | Some _ -> raise (Invalid_argument "key already bound")
      | None ->
        let ql = Queue.length q in
        assert (ql <= bound);
        if Queue.length q = bound
        then (
          let popkey = Queue.pop q in
          Ht.remove ht popkey);
        Queue.push k q;
        Ht.add ht k v
    ;;
  end
  in
  (module Ht)
;;

let put : type k v. (k, v) t -> k -> v -> unit = fun (module Ht) k v -> Ht.put k v
let find : type k v. (k, v) t -> k -> v option = fun (module Ht) k -> Ht.find k
let bound : type k v. (k, v) t -> int = fun (module Ht) -> Ht.bound
