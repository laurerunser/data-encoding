type t = int (* TODO? unsigned?? *)

val int64 : t
val int32 : t
val uint62 : t
val uint30 : t
val uint16 : t
val uint8 : t
val bool : t
val unit : t

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
