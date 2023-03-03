let rec generator_of_encoding : type t. t Json_data_encoding.Encoding.t -> t QCheck2.Gen.t
  =
 fun encoding ->
  match encoding with
  | Unit -> QCheck2.Gen.unit
  | Bool -> QCheck2.Gen.bool
  | Int64 -> QCheck2.Gen.int64
  | String -> QCheck2.Gen.(string_size (0 -- 10))
  | Seq t -> QCheck2.Gen.map List.to_seq (QCheck2.Gen.list (generator_of_encoding t))
  | Tuple t -> generator_of_encoding_tuple t
  | Object t -> generator_of_encoding_object t
  | Conv { serialisation = _; deserialisation; encoding } ->
    (*TODO: support errors*)
    QCheck2.Gen.map
      (fun w -> Result.get_ok @@ deserialisation w)
      (generator_of_encoding encoding)

and generator_of_encoding_tuple
  : type t. t Json_data_encoding.Encoding.tuple -> t QCheck2.Gen.t
  =
 fun t ->
  match t with
  | [] -> QCheck2.Gen.pure Commons.Hlist.[]
  | head :: tail ->
    let head = generator_of_encoding head in
    let tail = generator_of_encoding_tuple tail in
    QCheck2.Gen.map2 (fun h t -> Commons.Hlist.( :: ) (h, t)) head tail

and generator_of_encoding_object
  : type t. t Json_data_encoding.Encoding.obj -> t QCheck2.Gen.t
  =
 fun t ->
  match t with
  | [] -> QCheck2.Gen.pure Commons.Hlist.[]
  | Req { encoding = head; name = _ } :: tail ->
    let head = generator_of_encoding head in
    let tail = generator_of_encoding_object tail in
    QCheck2.Gen.map2 (fun h t -> Commons.Hlist.( :: ) (h, t)) head tail
  | Opt { encoding = head; name = _ } :: tail ->
    let head = generator_of_encoding head in
    let head = QCheck2.Gen.option head in
    let tail = generator_of_encoding_object tail in
    QCheck2.Gen.map2 (fun h t -> Commons.Hlist.( :: ) (h, t)) head tail
;;

let rec equal_of_encoding : type t. t Json_data_encoding.Encoding.t -> t -> t -> bool =
 fun encoding ->
  match encoding with
  | Unit -> Unit.equal
  | Bool -> Bool.equal
  | Int64 -> Int64.equal
  | String -> String.equal
  | Seq t -> Seq.for_all2 (equal_of_encoding t)
  | Tuple t -> equal_of_encoding_tuple t
  | Object t -> equal_of_encoding_object t
  | Conv { serialisation; deserialisation = _; encoding } ->
    fun a b -> (equal_of_encoding encoding) (serialisation a) (serialisation b)

and equal_of_encoding_tuple
  : type t. t Json_data_encoding.Encoding.tuple -> t -> t -> bool
  =
 fun t ->
  match t with
  | [] -> fun [] [] -> true
  | head :: tail ->
    let head = equal_of_encoding head in
    let tail = equal_of_encoding_tuple tail in
    fun (ah :: at) (bh :: bt) -> head ah bh && tail at bt

and equal_of_encoding_object : type t. t Json_data_encoding.Encoding.obj -> t -> t -> bool
  =
 fun t ->
  match t with
  | [] -> fun [] [] -> true
  | Req { encoding = head; name = _ } :: tail ->
    let head = equal_of_encoding head in
    let tail = equal_of_encoding_object tail in
    fun (ah :: at) (bh :: bt) -> head ah bh && tail at bt
  | Opt { encoding = head; name = _ } :: tail ->
    let head = equal_of_encoding head in
    let head = Option.equal head in
    let tail = equal_of_encoding_object tail in
    fun (ah :: at) (bh :: bt) -> head ah bh && tail at bt
;;

let ( let* ) x f =
  match x with
  | Error _ -> false
  | Ok v -> f v
;;

let to_test : type t. t Json_data_encoding.Encoding.t -> QCheck2.Test.t =
 fun encoding ->
  let generator = generator_of_encoding encoding in
  let equal = equal_of_encoding encoding in
  QCheck2.Test.make generator (fun v ->
    let* j = Json_data_encoding.Construct.construct encoding v in
    let lxms = Json_data_encoding.JSON.lexemify j in
    let* jj = Json_data_encoding.JSON.parse lxms in
    let* vv = Json_data_encoding.Destruct.destruct encoding jj in
    equal v vv)
;;
