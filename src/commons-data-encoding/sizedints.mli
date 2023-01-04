module Uint62 : sig
  type t = private Optint.Int63.t

  val min_int : t
  val max_int : t
  val of_int64 : int64 -> t option
  val set_be : bytes -> int -> t -> unit
  val set_le : bytes -> int -> t -> unit
  val get_be : string -> int -> t
  val get_le : string -> int -> t
end

module Uint30 : sig
  type t = private int

  val min_int : t
  val max_int : t
  val of_int : int -> t option
  val to_uint62 : t -> Uint62.t
  val set_be : bytes -> int -> t -> unit
  val set_le : bytes -> int -> t -> unit
  val get_be : string -> int -> t
  val get_le : string -> int -> t
end

module Uint16 : sig
  type t = private int

  val min_int : t
  val max_int : t
  val of_int : int -> t option
  val to_uint62 : t -> Uint62.t
  val set_be : bytes -> int -> t -> unit
  val set_le : bytes -> int -> t -> unit
  val get_be : string -> int -> t
  val get_le : string -> int -> t
end

module Uint8 : sig
  type t = private int

  val min_int : t
  val zero : t
  val one : t
  val max_int : t
  val of_int : int -> t option
  val to_uint62 : t -> Uint62.t
  val get : string -> int -> t
end
