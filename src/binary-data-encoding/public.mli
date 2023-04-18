(** {1 Public interface wrappers}

    The binary-data-encoding library uses descriptions in order to
    drive de/serialisations. However, the description type ({!Descr.t}) is
    public and complex (it tracks sizability). And so the binary-data-encoding
    library exposes an API based on encodings ({!Encoding.t}) which are private
    and simpler.

    The functions below are wrappers around core functions of
    binary-data-encoding which apply to encodings rather than descriptions. They
    are used in the top-level module of the library ({!Binary_data_encoding}) in
    order to expose a more consistent set of simpler primitives to the user.
    *)

(** Queries about the properties of some encodings. *)
module Query : sig
  val zero_of_numeral : 'a Encoding.numeral -> 'a
  val max_int_of : 'a Encoding.numeral -> Commons.Sizedints.Uint62.t
  val size_of_numeral : 'a Encoding.numeral -> Commons.Sizedints.Uint62.t
  val numeral_of_int : 'a Encoding.numeral -> int -> 'a
  val int_of_numeral : 'a Encoding.numeral -> 'a -> int
  val size_of : 'a Encoding.t -> 'a -> (Optint.Int63.t, string) result
  val maximum_size_of : 'a Encoding.t -> Optint.Int63.t
  val equal_of : 'a Encoding.t -> 'a -> 'a -> bool
  val pp_of : 'a Encoding.t -> Format.formatter -> 'a -> unit
  val sizability : 'a Encoding.t -> Sizability.s
end

module Reader : sig
  (** {1 Low-level reader}

    The function in this section ([readk]) is a low-level building
    block intended primarily for defining high-level functions. The next
    section offers some of these high-level wrappers. *)

  (** [readk state encoding] deserialise a value as per the [encoding]. (See
    documentation of [Encoding] for information about encodings.

    If [readk] has enough bytes available in [state] to complete the
    deserialisation, it returns [Readed].

    If [readk] encounters an error, it returns [Failed]. One possible error is
    for the deserialisation process to go over the [maximum_length] limit set
    via [mk_state]. Other errors are mismatches between [encoding] and the
    content of [state].

    If [readk] runs out of available bytes in [state], it returns [Suspended].
    In this case,, you can use the returned [cont]inuation to provide further
    bytes to read from.

    When you call the [cont]inuation, the [maximum_length] carries over so that
    the maximum number of bytes read by the call to [readk] and all the
    subsequent calls to [cont]s never exceeds this limit. *)
  val readk : Buffy.R.state -> 'a Encoding.t -> 'a Buffy.R.readed

  (** {2 High-level readers} *)

  val read
    :  src:string
    -> offset:int
    -> length:int
    -> 'a Encoding.t
    -> ('a, string) result

  val read_string : string -> 'a Encoding.t -> ('a, string) result
  val read_strings : (string * int * int) Seq.t -> 'a Encoding.t -> ('a, string) result
end

module Writer : sig
  (** low-level *)
  val writek : 'a Encoding.t -> Buffy.W.state -> 'a -> Buffy.W.written

  (** high-level *)
  val write
    :  'a Encoding.t
    -> dst:bytes
    -> offset:int
    -> length:int
    -> 'a
    -> (int, int * string) result

  val string_of : ?buffer_size:int -> 'a Encoding.t -> 'a -> (string, string) result
end
