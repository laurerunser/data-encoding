(** [construct encoding v] constructs a [JSON.t] representation of [v] as
    described by [encoding]. This construction is not lazy: it forces all the
    value to be represented. As a result, it's error management strategy is
    straightforward: it returns an [Error] in case of any issue. *)
val construct : 'a Encoding.t -> 'a -> (JSON.t, string) result

(** [destruct encoding j] destructs the [j:JSON.t] value into an OCaml value. *)
val destruct : 'a Encoding.t -> JSON.t -> ('a, string) result

(** [construct_lexeme encoding v] constructs a [JSON.lexeme Seq.t]
    representation of [v] as described by [encoding]. Because of the lazyness of
    [Seq.t], and unlike [construct], this function cannot return a clean
    [result]. Instead, traversing the sequence can raise exceptions. *)
val construct_lexeme : 'a Encoding.t -> 'a -> JSON.lexeme Seq.t
