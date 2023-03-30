(* TODO: documentation of @raise *)
(* TODO: documentation of string/bytes ownership and mutation discipline *)
(* TODO: more variants of write *)

(** {1: Low-level writer}

    The function in this section ([writek]) is a low-level building
    block intended primarily for defining high-level functions. The next
    section offers some of these high-level wrappers. *)

(** [writek destination encoding value] serialises the [value] as per the
    [encoding]. (See documentation in [Encoding] for information about the
    specific representation in serialised form.)

    If [writek] has enough bytes available in [destination] to complete the
    serialisation, it returns [Written].

    If [writek] encounters an error, it returns [Failed]. One possible error is
    for the serialisation process to go over the [maximum_length] limit set
    via [mk_destination]. Other errors are mismatches between [encoding] and
    [value] (e.g., a ranged-int that is out-of-range).

    If [writek] runs out of available bytes in [destination], it returns
    [Suspended]. In this case, you can use the returned [destination] to access
    the written bytes so far and you can use the returned [cont]inuation to
    provide further bytes to write onto.

    When you call the [cont]inuation, the [maximum_length] carries over so that
    the maximum number of bytes written by the call to [writek] and all
    subsequent calls to [cont]s never exceeds this limit.

    Depending on your use-case, you can provide [cont] with a fresh buffer, or
    you can re-use the buffer in [destination] (after you extract whatever
    information you need from it). *)
val writek : Buffy.W.state -> ('s, 'a) Descr.t -> 'a -> Buffy.W.written

(** {2: High-level writers} *)

val write
  :  dst:bytes
  -> offset:int
  -> length:int
  -> ('s, 'a) Descr.t
  -> 'a
  -> (int, int * string) result

val string_of : ?buffer_size:int -> ('s, 'a) Descr.t -> 'a -> (string, string) result
