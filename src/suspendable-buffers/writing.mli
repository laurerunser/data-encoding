(** A [destination] is a value that the [writek] function uses to write bytes
    to. It is a stateful wrapper around a [bytes] buffer. *)
type destination = private
  { buffer : bytes
  ; offset : int
  ; length : int
  ; written : int
  ; maximum_length : int
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
    its buffer with the original.

    @raise Invalid_argument if [maximum_length > destination.maximum_length].
    I.e., if this function is used to increase the limit.

    @raise Invalid_argument if [maximum_length < 0]. *)
val set_maximum_length : destination -> int -> destination

(** [destination_too_small_to_continue_message] is an error message used when a
    suspended destination is provided with a new buffer which is too small to
    resume the suspending write.

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

val write1 : destination -> int -> (bytes -> int -> unit) -> written

type chunkwriter = bytes -> int -> int -> chunkwritten

and chunkwritten =
  | K of int * chunkwriter
  | Finish of int

val writechunked : destination -> chunkwriter -> written
val ( let* ) : written -> (destination -> written) -> written
