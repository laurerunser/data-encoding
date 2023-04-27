type ('key, 'value) t

(** [make bound] *)
val make : (module Hashtbl.HashedType with type t = 'key) -> int -> ('key, 'value) t

(** [put t k v]

      @raise Invalid_argument if the key is already bound in [t]. *)
val put : ('key, 'value) t -> 'key -> 'value -> unit

(** [find t k] *)
val find : ('key, 'value) t -> 'key -> 'value option

val bound : (_, _) t -> int
