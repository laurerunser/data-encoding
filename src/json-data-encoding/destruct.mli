(** {1: JSON destructor}

    The functions in this module are used to extract OCaml values from JSON. *)

(** {2: [JSON.t] functions} *)

(** [destruct encoding j] destructs the [j:JSON.t] value into an OCaml value. *)
val destruct : 'a Encoding.t -> JSON.t -> ('a, string) result
