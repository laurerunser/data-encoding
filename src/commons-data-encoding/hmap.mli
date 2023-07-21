type 'v k

val k : unit -> 'v k

type t

val empty : t
val find : t -> 'v k -> 'v option
val add : t -> 'v k -> 'v -> t
