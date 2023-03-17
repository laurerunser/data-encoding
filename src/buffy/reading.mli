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
type state =
  { source : Src.t
  ; readed : int (* [read] is ambiguous so we make it unambiguously past as [readed] *)
  ; stop_hints : int list
      (* this list is grown when there is a size-header in the encoded binary data *)
  ; maximum_length : int
  }

(** [mk_state source] is a reading [state]. With such a state, the [readk]
    function can read from the bytes of [source].

    @param ?maximum_length is a limit on the maximum number of bytes
    read. [maximum_length] is a limit for a whole deserialisation procedure
    which might use multiple blobs (see the documentation of [readk]).

    @raise Failure if [offset] and [length] do not form a valid slice of
    [blob]. Specifically if
    [ offset<0 || length<0 || offset+length>String.length blob ]. *)
val mk_state : ?maximum_length:int -> Src.t -> state

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
    [maximum_length] limit of the [state].

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

(** [set_maximum_length state maximum_length] is a state identical
    to [state] but with the [maximum_length] field set to
    [maximum_length].

    @raise Invalid_argument if [maximum_length > state.maximum_length].
    I.e., if this function is used to increase the limit.

    @raise Invalid_argument if [maximum_length < 0].

    @raise Invalid_argument if [maximum_length] is before any of the pushed stop
    hints (see [push_stop]). *)
val set_maximum_length : state -> int -> state

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
                read from. Use [cont blob offset length] to provide one more
                slice that the deserialisation can read from. *)
      }

(** {2: Simple reading functions} *)

(* [readf state reading f] is for reading [reading] bytes from [state]. If
   enough bytes are available, it calls [f blob offset] allowing the actual read
   to take place.

   Returns [Failed] if reading exceeds the [maximum_length] of the [state].

   Returns [Failed] if the next stop hint is exceeded. (See [push_stop],
   [peak_stop] and [pop_stop].)

   If there are not enough bytes available in [state], then
   [readf state reading f] will allocate a buffer of [reading] bytes which it
   uses to copy the bytes remaining in the current state and proceed with the
   reading when more bytes are given to the suspension.

   It is recommended to use [readf] with values of [reading] which are small.
   One of the reason being this allocation which might be performed here.
   Check out chunk readers (below) if you need to read large values. *)
val readf : state -> int -> (Src.t -> int -> 'a) -> 'a readed
val read_copy : bytes -> state -> unit readed

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

(* TODO? place those in their own module? *)
(* TODO? type [('a, 'b) reader = 'a -> state -> 'b readed] *)

val read_string : state -> int -> string readed
val read_bytes : state -> int -> bytes readed
val read_char : state -> char readed
val read_utf8_uchar : state -> Uchar.t readed
val read_uint8 : state -> int readed
(* TODO? list/array/seq writing combinator? other combinators? *)
(* TODO? uint8, uint16, etc. reading functions (wrapping [Bytes.*]) *)

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
