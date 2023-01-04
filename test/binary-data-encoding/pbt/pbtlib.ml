let ( let* ) x f =
  match x with
  | Error msg -> failwith msg
  | Ok x -> f x
;;

let rec generator_of_encoding
    : type t. t Binary_data_encoding.Encoding.t -> t QCheck2.Gen.t
  =
 fun encoding ->
  match encoding with
  | Unit -> QCheck2.Gen.unit
  | Bool -> QCheck2.Gen.bool
  | Numeral { numeral = Int64; endianness = _ } -> QCheck2.Gen.int64
  | Numeral { numeral = Int32; endianness = _ } -> QCheck2.Gen.int32
  | Numeral { numeral = UInt30; endianness = _ } ->
    QCheck2.Gen.(
      map
        (fun v -> Option.get @@ Commons.Sizedints.Uint30.of_int v)
        (0 -- (Commons.Sizedints.Uint30.max_int :> int)))
  | Numeral { numeral = UInt62; endianness = _ } ->
    QCheck2.Gen.(
      map
        (fun v -> Option.get @@ Commons.Sizedints.Uint62.of_int64 v)
        (make_primitive
           ~gen:(fun prng ->
             let incl_bound =
               Optint.Int63.to_int64 (Commons.Sizedints.Uint62.max_int :> Optint.Int63.t)
             in
             let excl_bound = Int64.succ incl_bound in
             Random.State.int64 prng excl_bound)
           ~shrink:
             (Seq.unfold (fun i64 ->
                  if i64 = 0L
                  then None
                  else (
                    let shrunk = Int64.shift_right i64 1 in
                    Some (shrunk, shrunk))))))
  | Numeral { numeral = UInt16; endianness = _ } ->
    QCheck2.Gen.(
      map
        (fun v -> Option.get @@ Commons.Sizedints.Uint16.of_int v)
        (0 -- (Commons.Sizedints.Uint16.max_int :> int)))
  | Numeral { numeral = UInt8; endianness = _ } ->
    QCheck2.Gen.(
      map
        (fun v -> Option.get @@ Commons.Sizedints.Uint8.of_int v)
        (0 -- (Commons.Sizedints.Uint8.max_int :> int)))
  | String n ->
    QCheck2.Gen.(string_size (pure (Optint.Int63.to_int (n :> Optint.Int63.t))))
  | Bytes n ->
    QCheck2.Gen.(
      map
        Bytes.unsafe_of_string
        (string_size (pure (Optint.Int63.to_int (n :> Optint.Int63.t)))))
  | Seq { encoding; length } ->
    QCheck2.Gen.map
      (fun l -> { Binary_data_encoding.Encoding.seq = List.to_seq l; len = None })
      (QCheck2.Gen.list_repeat
         (Optint.Int63.to_int (length :> Optint.Int63.t))
         (generator_of_encoding encoding))
  | Option t ->
    let t = generator_of_encoding t in
    QCheck2.Gen.option t
  | Headered { mkheader; headerencoding; mkencoding; equal = _; maximum_size = _ } ->
    let headert = generator_of_encoding headerencoding in
    QCheck2.Gen.bind headert (fun header ->
        let* payloadencoding = mkencoding header in
        QCheck2.Gen.map
          (fun payload ->
            QCheck2.assume (Result.is_ok (mkheader payload));
            payload)
          (generator_of_encoding payloadencoding))
  | Fold _ ->
    if Obj.magic encoding == Binary_data_encoding.Encoding.ellastic_uint30
    then Obj.magic (generator_of_encoding Binary_data_encoding.Encoding.uint30)
    else failwith "TODO"
  | Conv { serialisation = _; deserialisation; encoding } ->
    let t = generator_of_encoding encoding in
    QCheck2.Gen.map
      (fun v ->
        let* v = deserialisation v in
        v)
      t
  | [] -> QCheck2.Gen.pure Commons.Hlist.[]
  | head :: tail ->
    let head = generator_of_encoding head in
    let tail = generator_of_encoding tail in
    QCheck2.Gen.map2 (fun h t -> Commons.Hlist.( :: ) (h, t)) head tail
;;

let print_of_encoding : type t. t Binary_data_encoding.Encoding.t -> t -> string =
 fun encoding v -> Format.asprintf "%a" (Binary_data_encoding.Query.pp_of encoding) v
;;

let ( let* ) x f =
  match x with
  | Error _ -> false
  | Ok v -> f v
;;

let to_test
    : type t.
      string
      -> ?gen:t QCheck2.Gen.t
      -> t Binary_data_encoding.Encoding.t
      -> QCheck2.Test.t
  =
 fun name ?gen encoding ->
  let generator =
    match gen with
    | Some g -> g
    | None -> generator_of_encoding encoding
  in
  let equal = Binary_data_encoding.Query.equal_of encoding in
  let print = print_of_encoding encoding in
  let offset = 0 in
  (* TODO: adapt length to encoding *)
  let length = 1024 in
  let dst = Bytes.make length '\x00' in
  QCheck2.Test.make ~name ~print generator (fun v ->
      let* written_length =
        Binary_data_encoding.Backend.write ~dst ~offset ~length encoding v
      in
      let src = Bytes.to_string dst in
      let* vv =
        Binary_data_encoding.Backend.read ~src ~offset ~length:written_length encoding
      in
      equal v vv)
;;
