(** Pretty-prints an overview of the shape of a JSON value. This is useful to
    print error messages which are informative (give some info about the value
    that was observed) without dumping an entire JSON value.

    The characters which are printed by this function do not form valid JSON.
    They are intended for humans.

    Guaranteed to no tinclude any line break. *)
val shape : Format.formatter -> JSON.t -> unit

(* TODO:

val compact

val pretty

*)
