(** {1: JSON backend}

    The functions in this module are used to produce JSON out of OCaml values
    and back. *)

(** {2: [JSON.t] functions} *)

(** [construct encoding v] constructs a [JSON.t] representation of [v] as
    described by [encoding]. This construction is not lazy: it forces all the
    value to be represented. As a result, it's error management strategy is
    straightforward: it returns an [Error] in case of any issue. *)
val construct : 'a Encoding.t -> 'a -> (JSON.t, string) result

(** [destruct encoding j] destructs the [j:JSON.t] value into an OCaml value. *)
val destruct : 'a Encoding.t -> JSON.t -> ('a, string) result

(** {2: [JSON.lexeme] functions} *)

(** [construct_lexeme encoding v] constructs a [JSON.lexeme Seq.t]
    representation of [v] as described by [encoding]. Because of the lazyness of
    [Seq.t], and unlike [construct], this function cannot return a clean
    [result]. Instead, traversing the sequence can raise exceptions. *)
val construct_lexeme : 'a Encoding.t -> 'a -> JSON.lexeme Seq.t

(** {2: "String" (but actually buffers) functions} *)

(* TODO? a direct from OCaml to buffer function? *)

(** [write_lexemes destination lexemes] writes the JSON lexemes of [lexemes]
    onto the [destination] buffer. *)
val write_lexemes
  :  Suspendable_buffers.Writing.destination
  -> JSON.lexeme Seq.t
  -> Suspendable_buffers.Writing.written
