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
  let writek : type a. a Encoding.t -> Buffy.W.state -> a -> Buffy.W.written =
   fun encoding ->
    let (Descr.E descr) = Encoding.introspect encoding in
    Writer.writek descr
 ;;

  let write
    : type a.
      a Encoding.t
      -> dst:bytes
      -> offset:int
      -> length:int
      -> a
      -> (int, int * string) result
    =
   fun encoding ->
    let (Descr.E descr) = Encoding.introspect encoding in
    Writer.write descr
 ;;

  let string_of : type a. ?buffer_size:int -> a Encoding.t -> a -> (string, string) result
    =
   fun ?buffer_size encoding v ->
    let (Descr.E encoding) = Encoding.introspect encoding in
    Writer.string_of ?buffer_size encoding v
 ;;
end
