(** {1 Suspendable writing buffers}

    This module exports types and values to allow users to write serialisers
    which manage suspending when more space is needed and resuming when more
    space is made available.

    Specifically, with this module, a user can write serialisation functions
    whilst retaining full control over the allocation and use of buffers. For
    example, a user can decide to allocate a single buffer based on information
    about the maximum size that may be required, or a series of small buffers
    each for a different chunk of the serialisation, or a single small buffer
    which is copied out in between two uses. A user can also decide to use the
    suspension/resumption of the serialisation process to insert concurrency
    cooperation points. *)

(** {2 State} *)

(** A [state] is a value that the [writef] function uses to write bytes
    to. It is a stateful wrapper around a [Dst.t] buffer. *)
type state = private
  { destination : Dst.t (** The destination the writing puts its bytes into. *)
  ; maximum_size : int
      (** [maximum_size] is the total number of bytes that
          can ever be written, on this buffer and all the subsequent buffers
          that may be needed after a suspend/resume. *)
  ; size_limits : int list
      (** A stack of reading limits. Each of those is a
        limit for a part of the writing process. *)
  }

(** [written state] is the number of bytes which have been written. Note that
    [written] is reset (to 0) every time the serialisation process is resumed
    after a suspension. In other words, [written state] is the number of bytes
    put into the [state]'s [destination], which is changed with every
    resumption. *)
val written : state -> int

(** [mk_state destination] is a state. With such a
    state, the [writef] function can write on the [destination].

    @param ?maximum_size is a limit on the maximum number of bytes
    written. [maximum_size] is a limit for a whole serialisation procedure
    which might use multiple destination (see the documentation of [writef]). *)
val mk_state : ?maximum_size:int -> Dst.t -> state

(** [push_limit state o] pushes a size limit [o] bytes ahead in the writing
    buffer. *)
val push_limit : state -> int -> (state, string) result

(** [remove_limit state] removes the most recently-added size limit from the
    state. *)
val remove_limit : state -> (state, string) result

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

(** A [written] is a value returned by [writef] in order to indicate the status
    of the serialisation operation. *)
type written =
  | Written of
      { state : state (** The state after the successful and complete writing. *) }
  | Failed of
      { state : state
          (** The state at the point of failure (some bytes might have been written). *)
      ; error : string
          (** [error] carries a human-readable message indicating the reason
                for the failure. *)
      }
  | Suspended of
      { state : state
          (** The state at the point of suspension. Note that, some
      writing functions avoid splitting values over multiple destinations in which
      case there may be unused bytes at the end of the destinations; other
      writing functions split values over multiple destinations in which case
      there may be a partial value written out. *)
      ; cont : Dst.t -> written
          (** The serialisation is suspended because it ran out of bytes to
                write to. Use [cont dst] to provide one more destination
                that the serialisation can write on.

                You can pass a destination which uses the same underlying bytes
                (after using the data therein). *)
      }

(** {2 Simple writing functions} *)

(* TODO? [type 'a writer = 'a -> state -> written] *)

(** [writef state width f] writes some content on the state's destination at
    the appropriate offset. More specifically, [writef]

    - assumes that exactly [width] bytes are going to be written (it may
      suspend the serialisation based on that information), and
    - calls [f dst] so that this function [f] does the writing (it may use
      [state.destination] or a different destination).

    As a caller it is your responsibility to ensure that [f] writes exactly
    [width] bytes on the provided destination starting at exactly the provided
    offset. Exceptions may be raised otherwise.

    Returns [Failed] if writing [width] bytes would exceed the [maximum_size] of
    [state].

    Returns [Failed] if writing [width] bytes would exceed the next size limit
    of [state].

    It is recommended to use this function to write data that is at least an
    order of magnitude smaller than the destination's size. (Or,
    equivalently, it is recommended to provide buffers that are at least an
    order of magnitude larger than the largest value of [width] in your
    serialisation application.) If [width] is too large (or equivalently if
    the buffer is too small) then the suspension mechanism is more likely to
    be engaged when the buffer utilisation is small.

    If you need to write large values, consider using [writechunked] below. *)
val writef : state -> int -> (Dst.t -> unit) -> written

(** {2 Chunked writing functions} *)

(** [chunkwriter] is the type of writers that can be used to write a single
    value spread over multiple chunks. See the documentation of {!writechunked}
    below.

    [chunkwriter]/[chunkwritten] is similar to [writef]/[written]. The main
    difference is that it doesn't hanfle the state. Instead, it only knows about
    destinations and it only reports how many bytes it has written. The
    [chunkwriter] is then driven by {!writechunked} function which takes care of
    handling the state, the suspensions, etc. *)
type chunkwriter = Dst.t -> int -> chunkwritten

and chunkwritten =
  | CWritten of
      { written : int (** [written] indicates how many bytes have been written. *) }
  | CFailed of
      { written : int
      ; error : string
      }
  | CSuspended of
      { written : int
      ; cont : chunkwriter
      }

(** [writechunked state w] interleaves calls to [w] within the
    suspend-resume mechanism of the [state] allowing the user to write
    more bytes than are available in the destination, or to spread writing into
    multiple chunks.

    [writechunked state w] calls [w dst available] where [dst] is the destination
    that the writer should write on and [available] is the maximum number of
    bytes that the writer is allowed to write at this time.

    - If [w] needs to write [width] bytes with [width<=available], then it
      should write [width] bytes and return [CWritten {written=width}].
      Alternatively, it can read fewer than [width] bytes and return
      [CSuspended {written; cont=ww}] where [written] is the number of bytes
      written and [ww] is a [chunkwriter] ready to write the additional needed
      bytes.
    - If [w] needs to write [width] bytes with [width>available], then it should
      write no more than [available] bytes into [dst] and return
      [CSuspended {written; cont=ww}] where [written] is the number of bytes
      written and [ww] is a [chunkwriter] for the bytes that remain.

    As a caller it is your responsibility to ensure that [w] follows exactly the
    discipline described above. *)
val writechunked : state -> chunkwriter -> written

(** {2 OCaml base-type writers} *)

val write_string : state -> string -> written
val write_bytes : state -> bytes -> written
val write_char : state -> char -> written
val write_utf8_uchar : state -> Uchar.t -> written
val write_uint8 : state -> int -> written
val write_int8 : state -> int -> written
val write_uint16_be : state -> int -> written
val write_uint16_le : state -> int -> written
val write_int16_be : state -> int -> written
val write_int16_le : state -> int -> written
val write_int32_be : state -> int32 -> written
val write_int32_le : state -> int32 -> written
val write_int64_be : state -> int64 -> written
val write_int64_le : state -> int64 -> written

(** {2 Composing writing functions} *)

(** [let*] is a binding operator for sequencing calls to different writing
    functions. It handles failures and suspension. E.g.,

{[
let state = mk_state … in
let* state = write_string state "…" in
let* state = write_char state '!' in
…
]} *)
val ( let* ) : written -> (state -> written) -> written

(** {2 Wrapping writing functions} *)

(** [to_string writer v] is a string, the content of which is the bytes produced
    by the serialisation of [v] by the [writer].

    @param ?buffer_size (default: [1024]) controls the size of the internal
    buffers. Multiple such buffers are allocated until the serialisation ends,
    at which point their content is concatenated. If the
    [destination_too_small_to_continue_message] error is encountered when
    resuming, the buffer size is doubled on retry. *)
val to_string : ?buffer_size:int -> (state -> written) -> (string, string) result

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
val blit_instructions : buffer:bytes -> (state -> written) -> (bytes * int * int) Seq.t
