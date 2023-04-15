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

(** {2: State} *)

(** A state is a value that tracks indexes and limits whilst reading. *)
type state = private
  { source : Src.t
  ; maximum_size : int
  ; size_limits : int list
  ; stop_hints : int list
  }

val readed : state -> int

(** [mk_state source] is a reading [state]. With such a state, the [readk]
    function can read from the bytes of [source].

    @param ?maximum_size is a limit on the maximum number of bytes
    read. [maximum_size] is a limit for a whole deserialisation procedure
    which might use multiple blobs (see the documentation of [readk]). *)
val mk_state : ?maximum_size:int -> Src.t -> state

(** [push_stop state o] adds a stop hint [o] bytes ahead in the reading buffer.

    A stop hint is a location in the buffer that a reader is marking so that it
    can later stop at this position. This is primarily intended for binary
    serialisation formats which include size headers: when encountering a
    size header the reader pushes a stop hint; during deserialisation the
    reader peeks at the recorded stop hint (via [peek_stop]); after reaching the
    stop hint the reader pops the recorded stop hint (via [pop_stop]).

    Note that the position [o] is relative to the current position in the
    [state].

    Returns an [Error] if the pushed stop hint is beyond the
    [maximum_size] limit of the [state].

    Returns an [Error] if the pushed stop hint is beyond an already placed
    stop hint. This means that the stops must be nested correctly like
    delimiters.
 *)
val push_stop : state -> int -> (state, string) result

(** [peek_stop state] is the next stop hint offset (if any have been set).
    In practical terms, it means there are [peek_stop state - state.readed]
    bytes left to read before reaching the stop hint. And as a special case,
    the stop hint is reached when [peek_stop state = state.readed].

    Returns [None] if there are no stop hints. *)
val peek_stop : state -> int option

(** [pop_stop state] removes and returns the next stop hint.

    Returns an [Error] if there are no stop hints. *)
val pop_stop : state -> (int * state, string) result

(** [bring_first_stop_forward]

    @raise Invalid_argument *)
val bring_first_stop_forward : state -> int -> state

(** [push_limit state o] adds a size-limit [o] bytes ahead in the reading buffer. *)
val push_limit : state -> int -> (state, string) result

(** [remove_limit state] removes the inner-most size-limit in the reading buffer. *)
val remove_limit : state -> (state, string) result

(** A [readed] is a value returned by [readk] in order to indicate the status of
    the deserialisation operation.

    The past-participle of {e to read} is {e read}. However, this is ambiguous
    with other forms of the verb. To avoid this ambiguity, the past-participle
    is misspelt into {e readed}. *)
type 'a readed =
  | Readed of
      { state : state
      ; value : 'a
          (** The deserialisation was successful and complete. The [value] is
                available. *)
      }
  | Failed of
      { state : state
      ; error : string
          (** [error] carries a human-readable message indicating the reason
                for the failure. *)
      }
  | Suspended of
      { state : state
      ; cont : Src.t -> 'a readed
          (** The deserialisation is suspeneded because it ran out of bytes to
                read from. Use [cont src] to provide one more source that the
                deserialisation can read from. *)
      }

(** {2: Simple reading functions} *)

(* TODO? type [('a, 'b) reader = 'a -> state -> 'b readed] *)

(** [readf state reading f] is for reading [reading] bytes from [state]. If
    enough bytes are available, it calls [f blob offset] allowing the actual read
    to take place.

    Returns [Failed] if reading exceeds the [maximum_size] of the [state].

    Returns [Failed] if the next size limit is exceeded. (See [push_limit]
    and [remove_limit].)

    Returns [Failed] if the next stop hint is exceeded. (See [push_stop],
    [peak_stop] and [pop_stop].)

    If there are not enough bytes available in [state], then
    [readf state reading f] will allocate a buffer of [reading] bytes which it
    uses to copy the bytes remaining in the current state and proceed with the
    reading when more bytes are given to the suspension.

    It is recommended to use [readf] with values of [reading] which are small.
    One of the reason being this allocation which might be performed here.
    Check out chunk readers (below) if you need to read large values. *)
val readf : state -> int -> (Src.t -> 'a) -> 'a readed

(** [readcopy buffer state] reads [Bytes.length buffer] bytes from state,
    copying them into [buffer]. *)
val readcopy : bytes -> state -> unit readed

(** {2: Chunked writing functions} *)

(** [chunkreader] is the type of readers that can be used to read a single value
    spread over multiple chunks. See the documentation of [readchunked] below. *)
type 'a chunkreader = Src.t -> 'a chunkreaded

and 'a chunkreaded =
  | CReaded of
      { readed : int
      ; value : 'a
      }
  | CFailed of
      { readed : int
      ; error : string
      }
  | CSuspended of
      { readed : int
      ; cont : 'a chunkreader
      }

(** [readchunked state r] interleaves calls to [r] within the suspend-resume
    mechanism of the [state], allowing the user to read more bytes than is
    available in a single blob, or to spread the reading into multiple chunks.

    [readchunked state r] calls [r b o l] where [b] is the underlying blob of
    [state], [o] is the offset [r] should read at, and [l] is the maximum
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
val readchunked : state -> 'a chunkreader -> 'a readed

(** {2: OCaml base-type readers} *)

val read_string : state -> int -> string readed
val read_bytes : state -> int -> bytes readed
val read_char : state -> char readed
val read_utf8_uchar : state -> Uchar.t readed
val read_uint8 : state -> int readed
val read_int8 : state -> int readed
val read_uint16_be : state -> int readed
val read_uint16_le : state -> int readed
val read_int16_be : state -> int readed
val read_int16_le : state -> int readed
val read_int32_be : state -> int32 readed
val read_int32_le : state -> int32 readed
val read_int64_be : state -> int64 readed
val read_int64_le : state -> int64 readed

(** {2: Composing reading functions} *)

(** [let*] is a binding operator for sequencing calls to different reading
    functions. It handles failures and suspensions. E.g.,

{[
let state = mk_state … in
let* c, state = read_char state in
let n = Char.code c in
let* s, state = read_string state n in
…
]} *)
val ( let* ) : 'a readed -> ('a * state -> 'b readed) -> 'b readed

(** {2: Wrapping reading functions} *)

(** [of_string read s] performs [read] on the bytes carried by [s].

    @raise Invalid_argument if [s] does not contain enough bytes to perform
    [read].

    @raise Invalid_argument if [s] contains more bytes than needed to perform
    [read]. *)
val of_string : (state -> 'a readed) -> string -> ('a, string) result

(** [of_string_seq read s] performs [read] on the bytes carried by the sequence
    of strings [s].

    @raise Invalid_argument if [s] does not contain enough bytes to perform
    [read].

    @raise Invalid_argument if [s] contains more bytes than needed to perform
    [read]. *)
val of_string_seq : (state -> 'a readed) -> string Seq.t -> ('a, string) result
