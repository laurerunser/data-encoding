(* TODO: documentation of @raise *)
(* TODO: documentation of string/bytes ownership and mutation discipline *)
(* TODO: more variants of read *)

(** {1: Low-level reader}

    The function in this section ([readk]) is a low-level building
    block intended primarily for defining high-level functions. The next
    section offers some of these high-level wrappers. *)

(** [readk source encoding] deserialise a value as per the [encoding]. (See
    documentation of [Encoding] for information about encodings.

    If [readk] has enough bytes available in [source] to complete the
    deserialisation, it returns [Readed].

    If [readk] encounters an error, it returns [Failed]. One possible error is
    for the deserialisation process to go over the [maximum_length] limit set
    via [mk_source]. Other errors are mismatches between [encoding] and the
    content of [source].

    If [readk] runs out of available bytes in [source], it returns [Suspended].
    In this case,, you can use the returned [cont]inuation to provide further
    bytes to read from.

    When you call the [cont]inuation, the [maximum_length] carries over so that
    the maximum number of bytes read by the call to [readk] and all the
    subsequent calls to [cont]s never exceeds this limit. *)
val readk : Buffy.R.source -> 'a Encoding.t -> 'a Buffy.R.readed

(** {2: High-level readers} *)

val read : src:string -> offset:int -> length:int -> 'a Encoding.t -> ('a, string) result
val read_string : string -> 'a Encoding.t -> ('a, string) result
val read_strings : (string * int * int) Seq.t -> 'a Encoding.t -> ('a, string) result
