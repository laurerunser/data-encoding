(** Buffy is a library for generic de/serialisation buffers with support for
    suspending and resuming the de/serialisation based on buffer space
    availability.

    Buffers in this library are not allocated implicitly. Their allocation is
    controlled by the user. (There are wrappers for some common allocation
    patterns such as buffers doubling in size and a single buffer being re-used;
    but these wrappers are not compulsory.) The design of Buffy aims to avoid
    all major-heap allocations within the library. *)

module Src = Src
module R = Reading
module Dst = Dst
module W = Writing
