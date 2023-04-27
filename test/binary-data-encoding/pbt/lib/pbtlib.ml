let ( let* ) x f =
  match x with
  | Error msg -> failwith msg
  | Ok x -> f x
;;

let rec generator_of_descr
  : type s t. (s, t) Binary_data_encoding__Descr.t -> t QCheck2.Gen.t
  = function
  | Unit -> QCheck2.Gen.unit
  | Bool -> QCheck2.Gen.bool
  | Numeral { numeral = Int64; endianness = _ } -> QCheck2.Gen.int64
  | Numeral { numeral = Int32; endianness = _ } -> QCheck2.Gen.int32
  | Numeral { numeral = Uint30; endianness = _ } ->
    QCheck2.Gen.(
      map
        (fun v -> Option.get @@ Commons.Sizedints.Uint30.of_int v)
        (0 -- (Commons.Sizedints.Uint30.max_int :> int)))
  | Numeral { numeral = Uint62; endianness = _ } ->
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
  | Numeral { numeral = Uint16; endianness = _ } ->
    QCheck2.Gen.(
      map
        (fun v -> Option.get @@ Commons.Sizedints.Uint16.of_int v)
        (0 -- (Commons.Sizedints.Uint16.max_int :> int)))
  | Numeral { numeral = Uint8; endianness = _ } ->
    QCheck2.Gen.(
      map
        (fun v -> Option.get @@ Commons.Sizedints.Uint8.of_int v)
        (0 -- (Commons.Sizedints.Uint8.max_int :> int)))
  | String n ->
    QCheck2.Gen.map
      (String.make (Optint.Int63.to_int (n :> Optint.Int63.t)))
      QCheck2.Gen.printable
  | Bytes n ->
    QCheck2.Gen.map
      (Bytes.make (Optint.Int63.to_int (n :> Optint.Int63.t)))
      QCheck2.Gen.printable
  | LSeq { length; descr } ->
    QCheck2.Gen.map
      (fun l -> { Binary_data_encoding.Descr.seq = List.to_seq l; length = lazy length })
      QCheck2.Gen.(
        list_size
          (let length = Optint.Int63.to_int (length :> Optint.Int63.t) in
           pure length)
          (generator_of_descr descr))
  | USeq { descr } ->
    QCheck2.Gen.map
      (fun l -> List.to_seq l)
      QCheck2.Gen.(list_size (QCheck2.Gen.int_range 0 4) (generator_of_descr descr))
  | Array { length; descr } ->
    let length = Optint.Int63.to_int (length :> Optint.Int63.t) in
    QCheck2.Gen.array_size (QCheck2.Gen.pure length) (generator_of_descr descr)
  | Option { optioner = _; descr } ->
    let g = generator_of_descr descr in
    QCheck2.Gen.option g
  | Headered
      { mkheader
      ; headerdescr
      ; writers = _
      ; readers = _
      ; sizers = _
      ; descr_of_header
      ; equal = _
      ; maximum_size = _
      } ->
    let headert =
      match headerdescr with
      | Numeral { numeral; endianness = _ } ->
        (* large int-sizes are generally for large collections which take too long
         to test in PBT *)
        QCheck2.Gen.map
          (Binary_data_encoding.Query.numeral_of_int numeral)
          (QCheck2.Gen.int_range 0 20)
      | headerdescr -> generator_of_descr headerdescr
    in
    QCheck2.Gen.bind headert (fun header ->
      let* payloaddescr = descr_of_header header in
      let payloadgenerator =
        match payloaddescr with
        | EStatic payloaddescr -> generator_of_descr payloaddescr
        | EDynamic payloaddescr -> generator_of_descr payloaddescr
      in
      QCheck2.Gen.map
        (fun payload ->
          QCheck2.assume (Result.is_ok (mkheader payload));
          payload)
        payloadgenerator)
  | Fold _ -> failwith "TODO"
  | Conv { serialisation = _; deserialisation; descr } ->
    let t = generator_of_descr descr in
    QCheck2.Gen.map
      (fun v ->
        let* v = deserialisation v in
        v)
      t
  | Size_headered { size = _; descr } ->
    (* TODO: check for overflow against size *)
    generator_of_descr descr
  | Size_limit { at_most; descr } ->
    ignore at_most;
    ignore descr;
    failwith "TODO_"
  | Union { tag = _; serialisation = _; deserialisation = _; cases } ->
    QCheck2.Gen.bind
      (QCheck2.Gen.oneofl cases)
      (fun (AnyC { tag = _; descr; write = _; read = _; size = _; inject }) ->
      QCheck2.Gen.map inject (generator_of_descr descr))
  | TupNil -> QCheck2.Gen.pure Commons.Hlist.[]
  | TupCons { tupler = _; head; tail } ->
    let head = generator_of_descr head in
    let tail = generator_of_descr tail in
    QCheck2.Gen.map2 (fun h t -> Commons.Hlist.( :: ) (h, t)) head tail

and generator_of_encoding : type t. t Binary_data_encoding.Encoding.t -> t QCheck2.Gen.t =
 fun encoding ->
  let (E descr) = Binary_data_encoding.Encoding.introspect encoding in
  generator_of_descr descr
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
    string -> ?gen:t QCheck2.Gen.t -> t Binary_data_encoding.Encoding.t -> QCheck2.Test.t
  =
 fun name ?gen encoding ->
  let generator =
    match gen with
    | Some g -> g
    | None -> generator_of_encoding encoding
  in
  let equal = Binary_data_encoding.Query.equal_of encoding in
  let print = print_of_encoding encoding in
  QCheck2.Test.make ~name ~print generator (fun v ->
    let* s = Binary_data_encoding.Writer.string_of encoding v in
    let* queried_size = Binary_data_encoding.Query.size_of encoding v in
    let queried_size = Optint.Int63.to_int queried_size in
    if String.length s <> queried_size
    then failwith "Computed size inconsistent with written size";
    let* vv = Binary_data_encoding.Reader.read_string encoding s in
    if not (equal v vv)
    then
      Format.kasprintf
        failwith
        "roundtrip broken; started with %s; found %s"
        (print v)
        (print vv);
    true)
;;
