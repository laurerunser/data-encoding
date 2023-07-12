module Uint62 : sig
  type t = private Optint.Int63.t

  val min_int : t
  val zero : t
  val max_int : t
  val of_int64 : int64 -> t option
  val of_int : int -> t option
  val set_be : bytes -> int -> t -> unit
  val set_le : bytes -> int -> t -> unit
  val get_be : string -> int -> t
  val get_le : string -> int -> t
  val mul : t -> t -> t
  val add : t -> t -> t
  val to_int : t -> int
end

module Uint30 : sig
  type t = private int

  val min_int : t
  val zero : t
  val max_int : t
  val of_int : int -> t option
  val unsafe_of_int : int -> t
  val of_int32 : int32 -> t option
  val to_uint62 : t -> Uint62.t
  val set_be : bytes -> int -> t -> unit
  val set_le : bytes -> int -> t -> unit
  val get_be : string -> int -> t
  val get_le : string -> int -> t
end

module Uint16 : sig
  type t = private int

  val min_int : t
  val zero : t
  val max_int : t
  val of_int : int -> t option
  val unsafe_of_int : int -> t
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
  val unsafe_of_int : int -> t
  val to_uint62 : t -> Uint62.t
  val get : string -> int -> t
end

type 'a numeral =
  | Uint8 : Uint8.t numeral
  | Uint16 : Uint16.t numeral
  | Uint30 : Uint30.t numeral
  | Uint62 : Uint62.t numeral
  | Int32 : int32 numeral
  | Int64 : int64 numeral
