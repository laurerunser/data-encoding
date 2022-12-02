let rec generator_of_encoding
    : type t. t Binary_data_encoding.Encoding.t -> t QCheck2.Gen.t
  =
 fun encoding ->
  match encoding with
  | Unit -> QCheck2.Gen.unit
  | Int64 -> QCheck2.Gen.int64
  | UInt64 -> QCheck2.Gen.(map Unsigned.UInt64.of_int64 int64)
  | Int32 -> QCheck2.Gen.int32
  | UInt32 -> QCheck2.Gen.(map Unsigned.UInt32.of_int32 int32)
  | UInt16 -> QCheck2.Gen.(map Unsigned.UInt16.of_int (0 -- 0xff_ff))
  | String n ->
    let n = Unsigned.UInt32.to_int n in
    QCheck2.Gen.(string_size (pure n))
  | Bytes n ->
    let n = Unsigned.UInt32.to_int n in
    QCheck2.Gen.(map Bytes.unsafe_of_string (string_size (pure n)))
  | Option t ->
    let t = generator_of_encoding t in
    QCheck2.Gen.option t
  | Headered _ -> failwith "TODO"
  | [] -> QCheck2.Gen.pure Commons.Hlist.[]
  | head :: tail ->
    let head = generator_of_encoding head in
    let tail = generator_of_encoding tail in
    QCheck2.Gen.map2 (fun h t -> Commons.Hlist.( :: ) (h, t)) head tail
;;

let rec equal_of_encoding : type t. t Binary_data_encoding.Encoding.t -> t -> t -> bool =
 fun encoding ->
  match encoding with
  | Unit -> Unit.equal
  | Int64 -> Int64.equal
  | UInt64 -> Unsigned.UInt64.equal
  | Int32 -> Int32.equal
  | UInt32 -> Unsigned.UInt32.equal
  | UInt16 -> Unsigned.UInt16.equal
  | String _ -> String.equal
  | Bytes _ -> Bytes.equal
  | Option t ->
    let t = equal_of_encoding t in
    Option.equal t
  | Headered _ -> failwith "TODO"
  | [] -> fun [] [] -> true
  | head :: tail ->
    let head = equal_of_encoding head in
    let tail = equal_of_encoding tail in
    fun (ah :: at) (bh :: bt) -> head ah bh && tail at bt
;;

let ( let* ) x f =
  match x with
  | Error _ -> false
  | Ok v -> f v
;;

let to_test : type t. t Binary_data_encoding.Encoding.t -> QCheck2.Test.t =
 fun encoding ->
  let generator = generator_of_encoding encoding in
  let equal = equal_of_encoding encoding in
  let dst = Bytes.make 100 '\x00' in
  (* TODO: adapt length to encoding *)
  let offset = 0 in
  let maximum_length = 100 in
  (* TODO: adapt maximum_length to encoding *)
  QCheck2.Test.make generator (fun v ->
      let* written_length =
        Binary_data_encoding.Backend.write ~dst ~offset ~maximum_length encoding v
      in
      let src = Bytes.to_string dst in
      let* vv, _ =
        Binary_data_encoding.Backend.read
          ~src
          ~offset
          ~maximum_length:written_length
          encoding
      in
      equal v vv)
;;
