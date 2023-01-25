(* TODO: documentation of @raise *)
(* TODO: documentation of string/bytes ownership and mutation discipline *)
(* TODO: more variants of read *)

(** {1: Low-level reader}

    The function in this section ([readk]) is a low-level building
    block intended primarily for defining high-level functions. The next
    section offers some of these high-level wrappers. *)

(** A [source] is a value that the [readk] function uses to read bytes from. It
    is a stateful wrapper around a [string] blob. *)
type source

(** [mk_source blob offset length] is a source. With such a source, the [readk]
    function can read from the bytes of [blob] from [offset] up to [length]
    bytes.

    @param ?maximum_length is a limit on the maximum number of bytes
    read. [maximum_length] is a limit for a whole deserialisation procedure
    which might use multiple blobs (see the documentation of [readk]).

    @raise Failure if [offset] and [length] do not form a valid slice of
    [blob]. Specifically if
    [ offset<0 || length<0 || offset+length>String.length blob ]. *)
val mk_source
  :  ?maximum_length:int
  -> ?stop_at_readed:int list
  -> string
  -> int
  -> int
  -> source

(** A [readed] is a value returned by [readk] in order to indicate the status of
    the deserialisation operation.

    The past-participle of {e to read} is {e read}. However, this is ambiguous
    with other forms of the verb. To avoid this ambiguity, the past-participle
    is mispelt into {e readed}. *)
type 'a readed =
  | Readed of
      { source : source
      ; value : 'a
            (** The deserialisation was successful and complete. The [value] is
                available. *)
      }
  | Failed of
      { source : source
      ; error : string
            (** [error] carries a human-readable message indicationg the reason
                for the failure. *)
      }
  | Suspended of
      { source : source
      ; cont : string -> int -> int -> 'a readed
            (** The deserialisation is suspeneded because it ran out of bytes to
                read from. Use [cont blob offset length] to provide one more
                slice that the deserialisation can read from. *)
      }

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
val readk : source -> 'a Encoding.t -> 'a readed

(** {2: High-level readers} *)

val read : src:string -> offset:int -> length:int -> 'a Encoding.t -> ('a, string) result
val read_strings : (string * int * int) Seq.t -> 'a Encoding.t -> ('a, string) result
