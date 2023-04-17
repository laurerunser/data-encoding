module Encoding = Encoding_public

module Query = struct
  include Query

  let size_of encoding a =
    let (Descr.E encoding) = Encoding.introspect encoding in
    Query.size_of encoding a
  ;;

  let maximum_size_of encoding =
    let (Descr.E encoding) = Encoding.introspect encoding in
    match sizability encoding with
    | Intrinsic _ -> Query.maximum_size_of encoding
    | Extrinsic ->
      failwith
        "Binary_data_encoding.Query.maximum_size_of: extrinsically sizable encoding"
  ;;

  let equal_of encoding a b =
    let (Descr.E encoding) = Encoding.introspect encoding in
    Query.equal_of encoding a b
  ;;

  let pp_of encoding fmt a =
    let (Descr.E encoding) = Encoding.introspect encoding in
    Query.pp_of encoding fmt a
  ;;

  let sizability encoding =
    let (Descr.E encoding) = Encoding.introspect encoding in
    Sizability.S (Query.sizability encoding)
  ;;
end

module Reader = struct
  let readk s encoding =
    let (Descr.E encoding) = Encoding.introspect encoding in
    Reader.readk s encoding
  ;;

  let read ~src ~offset ~length encoding =
    let (Descr.E encoding) = Encoding.introspect encoding in
    Reader.read ~src ~offset ~length encoding
  ;;

  let read_strings s encoding =
    let (Descr.E encoding) = Encoding.introspect encoding in
    Reader.read_strings s encoding
  ;;

  let read_string s encoding =
    let (Descr.E encoding) = Encoding.introspect encoding in
    Reader.read_string s encoding
  ;;
end

module Writer = struct
  let writek : type a. Buffy.W.state -> a Encoding.t -> a -> Buffy.W.written =
   fun state encoding v ->
    let (Descr.E encoding) = Encoding.introspect encoding in
    Writer.writek state encoding v
 ;;

  let write
    : type a.
      dst:bytes
      -> offset:int
      -> length:int
      -> a Encoding.t
      -> a
      -> (int, int * string) result
    =
   fun ~dst ~offset ~length encoding v ->
    let (Descr.E encoding) = Encoding.introspect encoding in
    Writer.write ~dst ~offset ~length encoding v
 ;;

  let string_of : type a. ?buffer_size:int -> a Encoding.t -> a -> (string, string) result
    =
   fun ?buffer_size encoding v ->
    let (Descr.E encoding) = Encoding.introspect encoding in
    Writer.string_of ?buffer_size encoding v
 ;;
end
