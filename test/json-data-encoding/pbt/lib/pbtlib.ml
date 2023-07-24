let rec generator_of_encoding : type t. t Json_data_encoding.Encoding.t -> t QCheck2.Gen.t
  =
 fun encoding ->
  match encoding with
  | Unit -> QCheck2.Gen.unit
  | Null -> QCheck2.Gen.unit
  | Bool -> QCheck2.Gen.bool
  | Int64 -> QCheck2.Gen.int64
  | String -> QCheck2.Gen.(string_size ?gen:(Some printable) (0 -- 10))
  | Seq t -> QCheck2.Gen.map List.to_seq (QCheck2.Gen.list (generator_of_encoding t))
  | Tuple t -> generator_of_encoding_tuple t
  | Object t -> generator_of_encoding_object t
  | Conv { serialisation = _; deserialisation; encoding } ->
    (*TODO: support errors*)
    QCheck2.Gen.map
      (fun w -> Result.get_ok @@ deserialisation w)
      (generator_of_encoding encoding)
  | Union { cases; serialisation = _; deserialisation = _ } ->
    QCheck2.Gen.bind
      (QCheck2.Gen.oneofl cases)
      (fun (Json_data_encoding.Encoding.AnyC { tag = _; encoding; inject }) ->
      QCheck2.Gen.map inject (generator_of_encoding encoding))

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
  | Null -> Unit.equal
  | Bool -> Bool.equal
  | Int64 -> Int64.equal
  | String -> String.equal
  | Seq t -> Seq.for_all2 (equal_of_encoding t)
  | Tuple t -> equal_of_encoding_tuple t
  | Object t -> equal_of_encoding_object t
  | Conv { serialisation; deserialisation = _; encoding } ->
    fun a b -> (equal_of_encoding encoding) (serialisation a) (serialisation b)
  | Union { cases = _; serialisation; deserialisation = _ } ->
    fun a b ->
      let (AnyP (ca, pa)) = serialisation a in
      let (AnyP (cb, pb)) = serialisation b in
      if ca.tag = cb.tag
      then
        (* TODO: extract an equality witness from the tag comparison *)
        equal_of_encoding ca.encoding pa (Obj.magic pb)
      else false

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

let to_test : type t. string -> t Json_data_encoding.Encoding.t -> QCheck2.Test.t =
 fun name encoding ->
  let generator = generator_of_encoding encoding in
  let equal = equal_of_encoding encoding in
  QCheck2.Test.make ~name generator (fun v ->
    let* j = Json_data_encoding.Construct.construct encoding v in
    let str = Json_data_encoding.JSON.to_string j in
    let buffy = Buffy.Src.of_string (str ^ " ") in
    let vv =
      Json_data_encoding.Destruct_incremental.destruct_incremental encoding buffy
    in
    match vv with
    | Await _ -> failwith "await"
    | Error e ->
      print_string (Json_data_encoding.Encoding.to_string encoding);
      print_newline ();
      print_string (Json_data_encoding.Encoding.value_to_string encoding v);
      print_newline ();
      failwith (str ^ " " ^ e)
    | Ok vv -> equal v vv)
;;

let to_test2 : type t. string -> t Json_data_encoding.Encoding.t -> QCheck2.Test.t =
 fun name encoding ->
  let generator = generator_of_encoding encoding in
  let equal = equal_of_encoding encoding in
  QCheck2.Test.make ~name generator (fun v ->
    let* j = Json_data_encoding.Construct.construct encoding v in
    let str = Json_data_encoding.JSON.to_string j in
    let str_length = String.length str in
    let split_point =
      try Random.int (str_length - 2) with
      | _ -> 1
    in
    let str1 = String.sub str 0 split_point in
    let str2 = String.sub str split_point (str_length - split_point) in
    let buffy = Buffy.Src.of_string str1 in
    let v1 =
      Json_data_encoding.Destruct_incremental.destruct_incremental encoding buffy
    in
    match v1 with
    | Await f ->
      let buffy = Buffy.Src.of_string (str2 ^ " ") in
      let v2 = f buffy in
      (match v2 with
       | Await _ -> failwith "await"
       | Error e ->
         print_string (Json_data_encoding.Encoding.to_string encoding);
         print_newline ();
         print_string (Json_data_encoding.Encoding.value_to_string encoding v);
         print_newline ();
         failwith (str ^ " " ^ e)
       | Ok vv -> equal v vv)
    | Error e ->
      print_string (Json_data_encoding.Encoding.to_string encoding);
      print_newline ();
      print_string (Json_data_encoding.Encoding.value_to_string encoding v);
      print_newline ();
      failwith ("Incomplete input, should fail. Error was: " ^ str ^ " " ^ e)
    | Ok _ -> failwith "Incomplete input, should fail but returned ok")
;;

let to_test_old : type t. string -> t Json_data_encoding.Encoding.t -> QCheck2.Test.t =
 fun name encoding ->
  let generator = generator_of_encoding encoding in
  let equal = equal_of_encoding encoding in
  QCheck2.Test.make ~name generator (fun v ->
    let* j = Json_data_encoding.Construct.construct encoding v in
    let vv = Json_data_encoding.Destruct.destruct encoding j in
    match vv with
    | Error e -> failwith e
    | Ok vv -> equal v vv)
;;
