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
  ; stop_at_readed : int list
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
  -> ?stop_at_readed:int list
  -> string
  -> int
  -> int
  -> source

(** TODO: document stops *)
val push_stop : source -> int -> (source, string) result

(** TODO: document stops *)
val pop_stop : source -> int * source

(** [set_maximum_length source maximum_length] is a source identical
    to [source] but with the [maximum_length] field set to
    [maximum_length].

    @raise Invalid_argument if [maximum_length > source.maximum_length].
    I.e., if this function is used to increase the limit.

    @raise Invalid_argument if [maximum_length < 0]. *)
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

(** {2: Simple reading functions} *)

(* TODO: documentation *)
val readf : source -> int -> (string -> int -> 'a) -> 'a readed

(** {2: Chunked writing functions} *)

(* TODO: documentation *)
type 'a chunkreader = string -> int -> int -> 'a chunkreaded

(* TODO: documentation *)
and 'a chunkreaded =
  | K of int * 'a chunkreader
  | Finish of 'a * int

(* TODO: documentation *)
val readchunked : source -> 'a chunkreader -> 'a readed

(** {2: OCaml base-type readers} *)

(* TODO? place those in their own module? *)

val read_small_string : source -> int -> string readed
val read_large_string : source -> int -> string readed
val read_large_bytes : source -> int -> bytes readed
val read_char : source -> char readed
(* TODO? list/array/seq writing combinator? other combinators? *)
(* TODO? uint8, uint16, etc. reading functions (wrapping [Bytes.*]) *)

(** {2: Composing reading functions} *)

val ( let* ) : 'a readed -> ('a * source -> 'b readed) -> 'b readed

(* TODO? [suspend] *)

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
