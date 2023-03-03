(** {1: Suspendable reading buffers}

    This module exports types and values to allow users to write deserialisers
    which manage suspending when more input is needed and resuming when more
    input is made available.

    Specifically, with this module, a user can write deserialisation functions
    whilst retaining full control over the use of the input stream of data. For
    example, a user can start deserialisation, which is suspended when all the
    provided has been consumed and resume the deserialisation when more data
    becomes available. A user can also decide to split a deserialisation
    operation in order to allow for concurrency cooperation points even if all
    of the data is available. *)

(** {2: Sources} *)

(** A [source] is a value that the [readk] function uses to read bytes from. It
    is a stateful wrapper around a [string] blob. *)
type source = private
  { blob : string
  ; offset : int
  ; length : int
  ; readed : int (* [read] is ambiguous so we make it unambiguously past as [readed] *)
  ; stop_hints : int list
      (* this list is grown when there is a size-header in the encoded binary data *)
  ; maximum_length : int
  }

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
  -> ?stop_hints:int list
  -> string
  -> int
  -> int
  -> source

(** [push_stop source o] adds a stop hint [o] bytes ahead in the reading buffer.

    A stop hint is a location in the buffer that a reader is marking so that it
    can later stop at this position. This is primarily intended for binary
    serialisation formats which include size headers: when encountering a
    size header the reader pushes a stop hint; during deserialisation the
    reader peaks at the recorded stop hint (via [peak_stop]); after reaching the
    stop hint the reader pops the recorded stop hint (via [pop_stop]).

    Note that the position [o] is relative to the current position in the
    [source].

    Returns an [Error] if the pushed stop hint is beyond the
    [maximum_length] limit of the [source].

    Returns an [Error] if the pushed stop hint is beyond an already placed
    stop hint. This means that the stops must be nested correctly like
    delimiters.
 *)
val push_stop : source -> int -> (source, string) result

(** [peak_stop source] is the next stop hint offset (if any have been set). The
    stop hint is relative to [source.offset]. In practical terms, it means there
    are [peak_stop source - source.readed] bytes left to read before reaching
    the stop hint. And as a special case, the stop hint is reached when
    [peak_stop source = source.readed].

    Returns [None] if there are no stop hints. *)
val peak_stop : source -> int option

(** [pop_stop source] removes and returns the next stop hint.

    Returns an [Error] if there are no stop hints. *)
val pop_stop : source -> (int * source, string) result

(** [set_maximum_length source maximum_length] is a source identical
    to [source] but with the [maximum_length] field set to
    [maximum_length].

    @raise Invalid_argument if [maximum_length > source.maximum_length].
    I.e., if this function is used to increase the limit.

    @raise Invalid_argument if [maximum_length < 0].

    @raise Invalid_argument if [maximum_length] is before any of the pushed stop
    hints (see [push_stop]). *)
val set_maximum_length : source -> int -> source

(** [source_too_small_to_continue_message] is an error message used when a
    suspended source is provided with a new blob which is too small to
    resume the suspended read.

    There are two approaches to avoiding this error.

    {ul
      {li You can provide larger and larger blobs until the suspension is
          successfully resumed.}
      {li You can make sure that you never call [readf] with sizes larger than
          the provided blobs. When reading a large value on the buffer, use
          [readchunked] instead. }
    }

    *)
val source_too_small_to_continue_message : string

(** A [readed] is a value returned by [readk] in order to indicate the status of
    the deserialisation operation.

    The past-participle of {e to read} is {e read}. However, this is ambiguous
    with other forms of the verb. To avoid this ambiguity, the past-participle
    is misspelt into {e readed}. *)
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
          (** [error] carries a human-readable message indicating the reason
                for the failure. *)
      }
  | Suspended of
      { source : source
      ; cont : string -> int -> int -> 'a readed
          (** The deserialisation is suspeneded because it ran out of bytes to
                read from. Use [cont blob offset length] to provide one more
                slice that the deserialisation can read from. *)
      }

(** {2: Simple reading functions} *)

(* [readf source reading f] is for reading [reading] bytes from [source]. If
   enough bytes are available, it calls [f blob offset] allowing the actual read
   to take place.

   Returns [Failed] if reading exceeds the [maximum_length] of the [source].

   Returns [Failed] if the next stop hint is exceeded. (See [push_stop],
   [peak_stop] and [pop_stop].)

   If there are not enough bytes available in [source], then
   [readf source reading f] will allocate a buffer of [reading] bytes which it
   uses to copy the bytes remaining in the current source and proceed with the
   reading when more bytes are given to the suspension.

   It is recommended to use [readf] with values of [reading] which are small.
   One of the reason being this allocation which might be performed here.
   Another is detailed in the documentation of
   {!source_too_small_to_continue_message}. Check out chunk readers (below) if
   you need to read large values. *)
val readf : source -> int -> (string -> int -> 'a) -> 'a readed

(** {2: Chunked writing functions} *)

(** [chunkreader] is the type of readers that can be used to read a single value
    spread over multiple chunks. See the documentation of [readchunked] below. *)
type 'a chunkreader = string -> int -> int -> 'a chunkreaded

and 'a chunkreaded =
  | K of int * 'a chunkreader
      (** [K (r, cr)] indicates that the chunkreader has
          read [r] bytes and has more bytes to read still. *)
  | Finish of 'a * int
      (** [Finish (v, r)] indicates that the chunkreader has
          finished reading. *)

(** [readchunked source r] interleaves calls to [r] within the suspend-resume
    mechanism of the [source], allowing the user to read more bytes than is
    available in a single blob, or to spread the reading into multiple chunks.

    [readchunked source r] calls [r b o l] where [b] is the underlying blob of
    [source], [o] is the offset [r] should read at, and [l] is the maximum
    number of bytes that [r] should read.

    - If [r] needs to read [ll] bytes with [ll<=l] then it should read [ll]
    bytes, transform those bytes into an OCaml value [v], and return
    [Finish (v, ll)].
    - If [r] needs to read [ll] bytes with [ll>l] then it should read as many
    bytes as possible within the limit of [l] and return [K (lll, r)] where
    [lll] is the number of bytes read and [r] is a [chunkreader] ready to
    consume the additional needed bytes.

    As a caller, it is your responsibility to ensure that [r] behaves as
    documentated here. *)
val readchunked : source -> 'a chunkreader -> 'a readed

(** {2: OCaml base-type readers} *)

(* TODO? place those in their own module? *)
(* TODO? type [('a, 'b) reader = 'a -> source -> 'b readed] *)

val read_small_string : source -> int -> string readed
val read_large_string : source -> int -> string readed
val read_large_bytes : source -> int -> bytes readed
val read_char : source -> char readed
val read_utf8_uchar : source -> Uchar.t readed
(* TODO? list/array/seq writing combinator? other combinators? *)
(* TODO? uint8, uint16, etc. reading functions (wrapping [Bytes.*]) *)

(** {2: Composing reading functions} *)

(** [let*] is a binding operator for sequencing calls to different reading
    functions. It handles failures and suspensions. E.g.,

{[
let source = mk_source … in
let* c, source = read_char source in
let n = Char.code c in
let* s, source = read_large_string source n in
…
]} *)
val ( let* ) : 'a readed -> ('a * source -> 'b readed) -> 'b readed

(** {2: Wrapping reading functions} *)

(** [of_string read s] performs [read] on the bytes carried by [s].

    @raise Invalid_argument if [s] does not contain enough bytes to perform
    [read].

    @raise Invalid_argument if [s] contains more bytes than needed to perform
    [read]. *)
val of_string : (source -> 'a readed) -> string -> ('a, string) result

(** [of_string_seq read s] performs [read] on the bytes carried by the sequence
    of strings [s].

    @raise Invalid_argument if [s] does not contain enough bytes to perform
    [read].

    @raise Invalid_argument if [s] contains more bytes than needed to perform
    [read]. *)
val of_string_seq : (source -> 'a readed) -> string Seq.t -> ('a, string) result
