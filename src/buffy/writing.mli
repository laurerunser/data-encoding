(** {1: Suspendable writing buffers}

    This module exports types and values to allow users to write serialisers
    which manage suspending when more space is needed and resuming when more
    space is made available.

    Specifically, with this module, a user can write serialisation functions
    whilst retaining full control over the allocation and use of buffers. For
    example, a user can decide to allocate a single buffer based on information
    about the maximum size that may be required, or a series of small buffers
    each for a different chunk of the serialisation, or a single small buffer
    which is used in between two uses. A user can also decide to use the
    suspension/resumption of the serialisation process to insert concurrency
    cooperation points. *)

(** {2: Destinations} *)

(** A [destination] is a value that the [writek] function uses to write bytes
    to. It is a stateful wrapper around a [bytes] buffer. *)
type destination = private
  { buffer : bytes (** [buffer] is the actual bytes that the data is serialised onto *)
  ; offset : int
        (** [offset] is the start of the area of [buffer] that the data is
                     serialised onto *)
  ; length : int
        (** [length] is the length of the area of [buffer] that the data is
                     serialised onto *)
  ; written : int
        (** [written] is the number of bytes that have been serialised; it
                      starts at [0] and is updated when writing *)
  ; maximum_length : int
        (** [maximum_length] is the total number of bytes that
                             can ever be written, on this buffer and all the
                             subsequent buffers that may be needed after a
                             suspend/resume. *)
  }

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

(** [set_maximum_length destination maximum_length] is a destination identical
    to [destination] but with the [maximum_length] field set to
    [maximum_length].

    Beware, the [destination] returned by a call to [set_maximum_length] shares
    its buffer with the original. The returned buffer is intended to replace the
    original one for the caller.

    @raise Invalid_argument if [maximum_length > destination.maximum_length].
    I.e., if this function is used to increase the limit.

    @raise Invalid_argument if [maximum_length < 0]. *)
val set_maximum_length : destination -> int -> destination

(** [destination_too_small_to_continue_message] is an error message used when a
    suspended destination is provided with a new buffer which is too small to
    resume the suspended write.

    There are two approaches to avoiding this error.

    {ul
      {li You can provide larger and larger buffers until the suspension is
          successfully resumed.

{[
let rec go cont len =
  let buffer = Bytes.make len ' ' in
  match cont buffer 0 len with
  | Failed {error; _}
      when error = destination_too_small_to_continue_message ->
      go cont (len * 2)
  | …
in
]} }
      {li You can make sure that you never call [write] with sizes larger than
          the provided buffers. When writing a large value on the buffer, use
          [writechunked] instead. }
    }

    *)
val destination_too_small_to_continue_message : string

(** [slice_of_destination destination] is a tuple [(buffer, offset, written)]
    where [buffer] and [offset] are as passed by [mk_destination] and where
    [written] is the number of bytes that have been written onto the buffer.

    In other words, [slice_of_destination destination] is a triplet which
    indicate the part of the destination which contains serialised data. *)
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
            (** [destination] may contain some serialised data (data up until
                the error). Depending on your application you may be able to
                restart based on this [destination]. *)
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
                slice that the serialisation can write on.

                You can pass the same buffer as before (after using the data
                therein) or pass a new buffer. You can also delay the resumption
                of the serialisation until a later point in your program. *)
      }

(** {2: Simple writing functions} *)

(** [writef destination writing f] writes some content on the destination's
    buffer. More specifically, [writef]

    - assumes that exactly [writing] bytes are going to be written (it may
      suspend the serialisation based on that information), and
    - calls [f buffer off] so that this function does the writing (it may use
      [destination.buffer] or a different buffer if it delays the call until
      after a suspension).

    As a caller it is your responsibility to ensure that [f] writes exactly
    [writing] bytes on the provided buffer starting at exactly the provided
    offset.

    It is recommended to use this function to write data that is at least an
    order of magnitude smaller than the destination's buffer size. (Or,
    equivalently, it is recommended to provide buffers that are at least an
    order of magnitude larger than the largest value of [writing] in your
    serialisation application.) If [writing] is too large (or equivalently if
    the buffer is too small) then the suspension mechanism is more likely to
    be engaged when the buffer utilisation is small.

    If you need to write large values, consider using [writechunked] below. *)
val writef : destination -> int -> (bytes -> int -> unit) -> written

(** {2: Chunked writing functions} *)

(** [chunkwriter] is the type of functions that can be used to write a single
    value in multiple chunks. See the documentation of [writechunked] below for
    details on usage. *)
type chunkwriter = bytes -> int -> int -> chunkwritten

and chunkwritten =
  | K of int * chunkwriter
      (** [K (l,cw)] indicates the chunkwriter has written
                               [l] bytes and has more bytes to write still. *)
  | Finish of int (** [Finish l] indicates the chunkwriter has finished writing *)

(** [writechunked destination w] interleaves calls to [w] within the
    suspend-resume mechanism of the [destination] allowing the user to write
    more bytes than are available in the buffer.

    [writechunked destination w] calls [w b o l] with [b] the underlying buffer
    that [w] should write on, [o] the offset that [w] should write at, [l] the
    maximum number of bytes that [w] should write.

    - If [w] needs to write [ll] bytes with [ll<=l], then it should write [ll]
      bytes and return [Finished ll].
    - If [w] needs to write [ll] bytes with [ll>l], then it should write as many
      bytes as possible within the limit of [l] and return [K (lll,w)] where
      [lll] is the number of bytes written and [w] is a [chunkwriter] for the
      bytes that remain.

    As a caller it is your responsibility to ensure that [w] follows exactly the
    discipline described above. *)
val writechunked : destination -> chunkwriter -> written

(** {2: OCaml base-type writers} *)

(* TODO? place these functions in a module of its own *)
(* TODO? [type 'a writer = 'a -> destination -> written] *)

(** [write_small_string destination s] is equivalent to
    [writef destination (String.length s) (fun b o -> Bytes.blit_string s 0 b o (String.length s)].
    I.e., it writes the whole of the string [s] onto [destination]. The warning
    about large writes detailed for [writef] apply too. For large strings
    consider using [writechunked] or [write_large_string]. *)
val write_small_string : destination -> string -> written

(** [write_char destination c] is equivalent to
    [writed destination 1 (fun b o -> Bytes.set b o c)]. *)
val write_char : destination -> char -> written

(** [write_utf8_uchar destination c] writes the UTF-8 encoding of the unicode
    point [c]. *)
val write_utf8_uchar : destination -> Uchar.t -> written

(** [write_large_string destination s] uses the chunkwritter mechanism
    above to write the string [s] onto the destination. You should use this
    function when you need to write a string that is on the same order of
    magnitude of size as the buffer. In such a case, the chunkwritter will
    ensure that no space is wasted on the buffer. *)
val write_large_string : destination -> string -> written

(** [write_large_bytes] is similar to [write_large_string] but for bytes. Beware
    that in case of suspension, time can elapse in between multiple uses of the
    argument: avoid performing side-effects on the bytes during that time. *)
val write_large_bytes : destination -> bytes -> written

(* TODO? list/array/seq writing combinator? other combinators? *)
(* TODO? uint8, uint16, etc. writing functions (wrapping [Bytes.*]) *)

(** {2: Composing writing functions} *)

(** [let*] is a binding operator for sequencing calls to different writing
    functions. It handles failures and suspension. E.g.,

{[
let destination = mk_destination … in
let* destination = write_large_string destination "…" in
let* destination = write_char destination '!' in
…
]} *)
val ( let* ) : written -> (destination -> written) -> written

(** {2 Wrapping writing functions} *)

(** [to_string writer v] is a string, the content of which is the bytes produced
    by the serialisation of [v] by the [writer].

    @param ?buffer_size (default: [1024]) controls the size of the internal
    buffers. Multiple such buffers are allocated until the serialisation ends,
    at which point their content is concatenated. If the
    [destination_too_small_to_continue_message] error is encountered when
    resuming, the buffer size is doubled on retry. *)
val to_string : ?buffer_size:int -> (destination -> written) -> (string, string) result

(** [blit_instructions ~buffer writer v] is a sequence of triplets [(b,o,l)].
    For each triplet, the [l] bytes in [b] starting at offset [o] is a part of
    the output of the serialisation process.

    The serialisation process uses only the provided [buffer] and no other such
    [buffer] is allocated. Forcing the next element of the sequence will cause
    the content of the buffer to be overwritten.

    The intended use for this function is to produce a sequence of triplets,
    each of which is blitted onto some I/O interface before the next one is
    forced. E.g.,

{[
let fd : Unix.file_descr = … in
let blits = blit_instructions ~buffer:(Bytes.make 2048) writer v in
Seq.iter
  (fun (b, offset, len) ->
    (* TODO: handle partial sends *)
    Unix.send fd b offset length [])
  blits
]}

    It is even possible to interleave some concurrency synchronisation
    primitives in between to bouts of serialisation. E.g.,

{[
let blits = blit_instructions ~buffer:(Bytes.make 2048) writer v in
let open Lwt.Syntax in
let* chunks =
  Lwt_seq.fold_left_s
    (fun acc (b, offset, len) ->
      let acc = (Bytes.sub_string b offset len) :: acc in
      let* () = (* let other promises progress *) Lwt.pause () in
      Lwt.return acc)
    []
    blits
in
Lwt.return (String.concat "" (List.rev chunks))
]}

    @raise Failure When forcing any of the elements of the returned sequence,
    the exception [Failure] may be raised. *)
val blit_instructions
  :  buffer:bytes
  -> (destination -> written)
  -> (bytes * int * int) Seq.t
