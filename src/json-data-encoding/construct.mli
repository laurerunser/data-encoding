(** {1: JSON constructor}

    The functions in this module are used to produce JSON out of OCaml values. *)

(** {2: [JSON.t] functions} *)

(** [construct encoding v] constructs a [JSON.t] representation of [v] as
    described by [encoding]. This construction is not lazy: it forces all the
    value to be represented. As a result, it's error management strategy is
    straightforward: it returns an [Error] in case of any issue. *)
val construct : 'a Encoding.t -> 'a -> (JSON.t, string) result

(** {2: [JSON.lexeme] functions} *)

(** [construct_lexemes encoding v] constructs a [JSON.lexeme Seq.t]
    representation of [v] as described by [encoding]. Because of the lazyness of
    [Seq.t], and unlike [construct], this function cannot return a clean
    [result]. Instead, traversing the sequence can raise exceptions. *)
val construct_lexemes : 'a Encoding.t -> 'a -> JSON.lexeme Seq.t

(** {2: "String" (but actually buffers) functions} *)

(** [write_lexemes destination lexemes] writes the JSON lexemes of [lexemes]
    onto the [destination] buffer. *)
val write_lexemes : Buffy.W.state -> JSON.lexeme Seq.t -> Buffy.W.written

(** [write destination encoding v] writes the JSON representation of [v] as
    described by [encoding].

    [write d e v] is equivalent to [write_lexemes d (construct_lexemes e v)] but
    more efficient. *)
val write : Buffy.W.state -> 'a Encoding.t -> 'a -> Buffy.W.written
