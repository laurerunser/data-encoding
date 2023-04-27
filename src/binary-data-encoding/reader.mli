(* TODO: documentation of @raise *)
(* TODO: documentation of string/bytes ownership and mutation discipline *)
(* TODO: more variants of read *)

(** {1 Low-level reader} *)

(** [readk descr state] reads from [state] a value as described by [descr]. *)
val readk : ('s, 'a) Descr.t -> Buffy.R.state -> 'a Buffy.R.readed

(** {1 High-level readers} *)

(** [read ~src ~offset ~length descr] reads from the string [src] starting at
    [offset] and for a maximum of [length] bytes a value as described by
    [descr]. *)
val read
  :  ('s, 'a) Descr.t
  -> src:string
  -> offset:int
  -> length:int
  -> ('a, string) result

(** [read_string descr src] is
    [read descr ~src ~offset:0 ~length:(String.length src)] *)
val read_string : ('s, 'a) Descr.t -> string -> ('a, string) result

(** [read_strings descr ss] reads from the successive string-slices of [ss] a
    value as described by [descr]. Slices are used as required by the
    suspend/resume mechanism of [Buffy].

    [read_strings descr ss] is equivalent to
{[
let ss = Seq.map (fun (s, o, l) -> String.sub s o l) ss in
let s = String.concat "" (List.of_seq ss) in
read_string descr s
]}
    but it avoids having to hold the full-sized string in memory.
    *)
val read_strings : ('s, 'a) Descr.t -> (string * int * int) Seq.t -> ('a, string) result
