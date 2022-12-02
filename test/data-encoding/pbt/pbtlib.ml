let rec generator_of_encoding : type t. t Data_encoding.Encoding.t -> t QCheck2.Gen.t
  = function
  | Unit -> QCheck2.Gen.unit
  | Int64 -> QCheck2.Gen.int64
  | String ->
    (* TODO: length *)
    QCheck2.Gen.(string_size (0 -- 10))
  | Bytes -> QCheck2.Gen.(map Bytes.unsafe_of_string (string_size (0 -- 10)))
  | Tuple t -> generator_of_encoding_tuple t
  | Object t -> generator_of_encoding_object t
  | Split _ -> failwith "TODO"

and generator_of_encoding_tuple
    : type t. t Data_encoding.Encoding.tuple -> t QCheck2.Gen.t
  =
 fun t ->
  match t with
  | [] -> QCheck2.Gen.pure Commons.Hlist.[]
  | head :: tail ->
    let head = generator_of_encoding head in
    let tail = generator_of_encoding_tuple tail in
    QCheck2.Gen.map2 (fun h t -> Commons.Hlist.( :: ) (h, t)) head tail

and generator_of_encoding_object : type t. t Data_encoding.Encoding.obj -> t QCheck2.Gen.t
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

let rec equal_of_encoding : type t. t Data_encoding.Encoding.t -> t -> t -> bool =
 fun encoding ->
  match encoding with
  | Unit -> Unit.equal
  | Int64 -> Int64.equal
  | String -> String.equal
  | Bytes -> Bytes.equal
  | Tuple t -> equal_of_encoding_tuple t
  | Object t -> equal_of_encoding_object t
  | Split _ -> failwith "TODO"

and equal_of_encoding_tuple : type t. t Data_encoding.Encoding.tuple -> t -> t -> bool =
 fun t ->
  match t with
  | [] -> fun [] [] -> true
  | head :: tail ->
    let head = equal_of_encoding head in
    let tail = equal_of_encoding_tuple tail in
    fun (ah :: at) (bh :: bt) -> head ah bh && tail at bt

and equal_of_encoding_object : type t. t Data_encoding.Encoding.obj -> t -> t -> bool =
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

let to_test_binary : type t. t Data_encoding.Encoding.t -> QCheck2.Test.t =
 fun encoding ->
  let generator = generator_of_encoding encoding in
  let equal = equal_of_encoding encoding in
  let e_b = Data_encoding.Encoding.to_binary encoding in
  let dst = Bytes.make 100 '\x00' in
  (* TODO: adapt length to encoding *)
  let offset = 0 in
  let maximum_length = 100 in
  (* TODO: adapt maximum_length to encoding *)
  QCheck2.Test.make generator (fun v ->
      let* written_length =
        Binary_data_encoding.Backend.write ~dst ~offset ~maximum_length e_b v
      in
      let src = Bytes.to_string dst in
      let* vv, _ =
        Binary_data_encoding.Backend.read ~src ~offset ~maximum_length:written_length e_b
      in
      equal v vv)
;;

let to_test_json : type t. t Data_encoding.Encoding.t -> QCheck2.Test.t =
 fun encoding ->
  let generator = generator_of_encoding encoding in
  let equal = equal_of_encoding encoding in
  (* JSON *)
  let e_j = Data_encoding.Encoding.to_json encoding in
  QCheck2.Test.make generator (fun v ->
      let* j = Json_data_encoding.Backend.construct e_j v in
      let* vv = Json_data_encoding.Backend.destruct e_j j in
      equal v vv)
;;
