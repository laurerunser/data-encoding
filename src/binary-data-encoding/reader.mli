(* TODO: documentation of @raise *)
(* TODO: documentation of string/bytes ownership and mutation discipline *)
(* TODO: more variants of read *)

(** {1 Low-level reader} *)

(** [readk state descr] reads from [state] a value as described by [descr]. *)
val readk : Buffy.R.state -> ('s, 'a) Descr.t -> 'a Buffy.R.readed

(** {1 High-level readers} *)

(** [read ~src ~offset ~length descr] reads from the string [src] starting at
    [offset] and for a maximum of [length] bytes a value as described by
    [descr]. *)
val read
  :  src:string
  -> offset:int
  -> length:int
  -> ('s, 'a) Descr.t
  -> ('a, string) result

(** [read_string src descr] is
    [read ~src ~offset:0 ~length:(String.length src) descr] *)
val read_string : string -> ('s, 'a) Descr.t -> ('a, string) result

(** [read_strings ss descr] reads from the successive string-slices of [ss] a
    value as described by [descr]. Slices are used as required by the
    suspend/resume mechanism of [Buffy].

    [read_strings ss descr] is equivalent to
{[
let ss = Seq.map (fun (s, o, l) -> String.sub s o l) ss in
let s = String.concat "" (List.of_seq ss) in
read_string s descr
]}
    but it avoids having to hold the full-sized string in memory.
    *)
val read_strings : (string * int * int) Seq.t -> ('s, 'a) Descr.t -> ('a, string) result
