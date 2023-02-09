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

val pop_stop : source -> int * source

(** [set_maximum_length source maximum_length] is a source identical
    to [source] but with the [maximum_length] field set to
    [maximum_length].

    @raise Invalid_argument if [maximum_length > source.maximum_length].
    I.e., if this function is used to increase the limit.

    @raise Invalid_argument if [maximum_length < 0]. *)
val set_maximum_length : source -> int -> source

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

val read1 : source -> int -> (string -> int -> 'a) -> 'a readed

type 'a chunkreader = string -> int -> int -> 'a chunkreaded

and 'a chunkreaded =
  | K of int * 'a chunkreader
  | Finish of 'a * int

val readchunked : source -> 'a chunkreader -> 'a readed
val ( let* ) : 'a readed -> ('a * source -> 'b readed) -> 'b readed
