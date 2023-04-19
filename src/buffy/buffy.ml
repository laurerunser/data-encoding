(** {1 Buffy}

{v
> Thanks for the dadaist pep talk, I feel much more abstract now.

-- Buffy Summers
v}

    Buffy is a library for abstract de/serialisation buffers with support for
    suspending and resuming the de/serialisation based on buffer space
    availability.

    Buffers in this library are not allocated implicitly. Their allocation is
    controlled by the user. (There are wrappers for some common allocation
    patterns such as buffers doubling in size and a single buffer being re-used;
    but these wrappers are not compulsory.) The design of Buffy aims to avoid
    all major-heap allocations within the library.

    When a serialisation (or deserialisation) process has run out of available
    space (or available bytes), the process returns a continuation expecting
    further space (or further bytes). The user provides this space (or those
    bytes) to resume the process. *)

(** {2 Reading} *)

(** [Src] is for {e sources}. A source is a value that a reading function can
    get bytes (and other bits) from. *)
module Src = Src

(** [R] is for {e reading}. Reading is the process of extracting information
    from sources ([Src]). *)
module R = Reading

(** {2 Writing} *)

(** [Dst] is for {e destinations}. A destination is a value that a writing
    function can put bytes (and other bits) into. *)
module Dst = Dst

(** [W] is for {e writing}. Writing is the process of adding information into
    destinations ([Dst]). *)
module W = Writing
