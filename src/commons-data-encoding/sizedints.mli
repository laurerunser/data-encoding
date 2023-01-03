module Uint30 : sig
  type t = private int

  val min_int : t
  val max_int : t
  val of_int : int -> t option
  val set_be : bytes -> int -> t -> unit
  val get_be : bytes -> int -> t
end

module Uint16 : sig
  type t = private int

  val min_int : t
  val max_int : t
  val of_int : int -> t option
  val to_uint30 : t -> Uint30.t
  val set_be : bytes -> int -> t -> unit
  val get_be : bytes -> int -> t
end

module Uint8 : sig
  type t = private int

  val min_int : t
  val zero : t
  val one : t
  val max_int : t
  val of_int : int -> t option
  val to_uint30 : t -> Uint30.t
  val set : bytes -> int -> t -> unit
  val get : bytes -> int -> t
end
