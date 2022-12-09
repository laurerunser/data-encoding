(* TODO: documentation of @raise *)
(* TODO: documentation of string/bytes ownership and mutation discipline *)
(* TODO: more variants of read/write *)
(* TODO: separate the low-level backend functions ([*k]) from the high-level
   backend functions (all others) *)

(** {1: Low-level backend}

    The functions in this section ([writek] and [readk]) are low-level building
    blocks intended primarily for defining high-level functions. The next
    section offers some of these high-level wrappers. *)

(** A [destination] is a value that the [writek] function uses to write bytes
    to. It is a stateful wrapper around a [bytes] buffer. *)
type destination

(** [mk_destination buffer offset length] is a destination. With such a
    destination, the [writek] function can write on the bytes of [buffer] from
    the [offset] up to [length] bytes.

    @param ?maximum_length is a limit on the maximum number of bytes
    written. [maximum_length] is a limit for a whole serialisation procedure
    which might use multiple buffers (see the documentation of [writek]).

    @raise Failure if [offset] and [length] do not form a valid slice of
    [buffer]. Specifically if
    [ offset<0 || length<0 || offset+length>Bytes.length buffer ]. *)
val mk_destination : ?maximum_length:int -> bytes -> int -> int -> destination

(** [slice_of_destination destination] is a tuple [buffer, offset, written]
    where [buffer] and [offset] are as passed by [mk_destination] and where
    [written] is the number of bytes that have been written onto the buffer. *)
val slice_of_destination : destination -> bytes * int * int

(** A [written] is a value returned by [writek] in order to indicate the status
    of the serialisation operation. *)
type written =
  | Written of
      { destination : destination
            (** The serialisation was successful and complete. Use
                [slice_of_destination] to extract useful information from this
                [destination]. *)
      }
  | Failed of
      { destination : destination
      ; error : string
            (** [error] carries a human-readable message indicating the reason
                for the failure. *)
      }
  | Suspended of
      { destination : destination
            (** The serialisation is partial. Some bytes have been written. Use
                [slice_of_destination] to extract useful information about from
                this [destination]. *)
      ; cont : bytes -> int -> int -> written
            (** The serialisation is suspended because it ran out of bytes to
                write to. Use [cont buffer offset length] to provide one more
                slice that the serialisation can write on. *)
      }

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
val writek : destination -> 'a Encoding.t -> 'a -> written

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
val mk_source : ?maximum_length:int -> string -> int -> int -> source

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

(** {2: High-level backend}

    The functions in this section ([writek] and [readk]) are low-level building
    blocks intended primarily for defining high-level functions. The next
    section offers some of these high-level wrappers. *)

val write
  :  dst:bytes
  -> offset:int
  -> length:int
  -> 'a Encoding.t
  -> 'a
  -> (int, int * string) result

val string_of : ?buffer_size:int -> 'a Encoding.t -> 'a -> (string, string) result
val read : src:string -> offset:int -> length:int -> 'a Encoding.t -> ('a, string) result
val read_strings : (string * int * int) Seq.t -> 'a Encoding.t -> ('a, string) result
