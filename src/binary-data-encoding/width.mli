(** {1 Width}

    The width of something is the number of bytes it takes to represent it in
    binary. E.g., int64 are represented on 8 bytes. *)

(** {2 Width as [int]} *)

(**)
val int64 : int
val int32 : int
val uint62 : int
val uint30 : int
val uint16 : int
val uint8 : int
val bool : int
val unit : int

(** {2 Width as uint62} *)
module As_uint62 : sig
  val int64 : Commons.Sizedints.Uint62.t
  val int32 : Commons.Sizedints.Uint62.t
  val uint62 : Commons.Sizedints.Uint62.t
  val uint30 : Commons.Sizedints.Uint62.t
  val uint16 : Commons.Sizedints.Uint62.t
  val uint8 : Commons.Sizedints.Uint62.t
  val bool : Commons.Sizedints.Uint62.t
  val unit : Commons.Sizedints.Uint62.t
end
