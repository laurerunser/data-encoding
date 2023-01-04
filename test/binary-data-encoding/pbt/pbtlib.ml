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

let rec equal_of_encoding : type t. t Binary_data_encoding.Encoding.t -> t -> t -> bool =
 fun encoding ->
  match encoding with
  | Unit -> Unit.equal
  | Bool -> Bool.equal
  | Numeral { numeral = Int64; endianness = _ } -> Int64.equal
  | Numeral { numeral = Int32; endianness = _ } -> Int32.equal
  | Numeral { numeral = UInt62; endianness = _ } ->
    fun a b -> Optint.Int63.equal (a :> Optint.Int63.t) (b :> Optint.Int63.t)
  | Numeral { numeral = UInt30; endianness = _ } ->
    fun a b -> Int.equal (a :> int) (b :> int)
  | Numeral { numeral = UInt16; endianness = _ } ->
    fun a b -> Int.equal (a :> int) (b :> int)
  | Numeral { numeral = UInt8; endianness = _ } ->
    fun a b -> Int.equal (a :> int) (b :> int)
  | String _ -> String.equal
  | Bytes _ -> Bytes.equal
  | Option t ->
    let t = equal_of_encoding t in
    Option.equal t
  | Headered { mkheader = _; headerencoding = _; mkencoding = _; equal; maximum_size = _ }
    -> equal
  | Fold
      { chunkencoding = _
      ; chunkify = _
      ; readinit = _
      ; reducer = _
      ; equal
      ; maximum_size = _
      } -> equal
  | Conv { serialisation; deserialisation = _; encoding } ->
    fun x y -> (equal_of_encoding encoding) (serialisation x) (serialisation y)
  | [] -> fun [] [] -> true
  | head :: tail ->
    let head = equal_of_encoding head in
    let tail = equal_of_encoding tail in
    fun (ah :: at) (bh :: bt) -> head ah bh && tail at bt
;;

let rec pp_of_encoding
    : type t. t Binary_data_encoding.Encoding.t -> Format.formatter -> t -> unit
  =
 fun encoding fmt v ->
  match encoding with
  | Unit -> Format.fprintf fmt "()"
  | Bool -> Format.fprintf fmt "%b" v
  | Numeral { numeral = Int64; endianness = _ } -> Format.fprintf fmt "%Ld" v
  | Numeral { numeral = Int32; endianness = _ } -> Format.fprintf fmt "%ld" v
  | Numeral { numeral = UInt62; endianness = _ } ->
    Format.fprintf fmt "%Ld" (Optint.Int63.to_int64 (v :> Optint.Int63.t))
  | Numeral { numeral = UInt30; endianness = _ } -> Format.fprintf fmt "%d" (v :> int)
  | Numeral { numeral = UInt16; endianness = _ } -> Format.fprintf fmt "%d" (v :> int)
  | Numeral { numeral = UInt8; endianness = _ } -> Format.fprintf fmt "%d" (v :> int)
  | String _ -> Format.fprintf fmt "%s" v
  | Bytes _ -> Format.fprintf fmt "%s" (Bytes.unsafe_to_string v)
  | Option t ->
    (match v with
    | None -> Format.fprintf fmt "None"
    | Some v ->
      let pp = pp_of_encoding t in
      Format.fprintf fmt "Some(%a)" pp v)
  | Headered { mkheader; headerencoding = _; mkencoding; equal = _; maximum_size = _ } ->
    let ( let* ) = Result.bind in
    (match
       let* header = mkheader v in
       let* encoding = mkencoding header in
       Ok encoding
     with
    | Ok encoding ->
      let pp = pp_of_encoding encoding in
      Format.fprintf fmt "%a" pp v
    | Error msg -> Format.fprintf fmt "Error: %s" msg)
  | Fold
      { chunkencoding; chunkify; readinit = _; reducer = _; equal = _; maximum_size = _ }
    ->
    Format.fprintf
      fmt
      "chunked(%a)"
      (Format.pp_print_seq
         ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ',')
         (pp_of_encoding chunkencoding))
      (chunkify v)
  | Conv { serialisation; deserialisation = _; encoding } ->
    let pp fmt v = pp_of_encoding encoding fmt (serialisation v) in
    Format.fprintf fmt "conved(%a)" pp v
  | [] -> ()
  | [ head ] ->
    let [ v ] = v in
    let pp = pp_of_encoding head in
    Format.fprintf fmt "%a" pp v
  | head :: tail ->
    let (v :: vs) = v in
    let pph = pp_of_encoding head in
    let ppt = pp_of_encoding tail in
    Format.fprintf fmt "%a;%a" pph v ppt vs
;;

let print_of_encoding : type t. t Binary_data_encoding.Encoding.t -> t -> string =
 fun encoding v -> Format.asprintf "%a" (fun fmt v -> pp_of_encoding encoding fmt v) v
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
  let equal = equal_of_encoding encoding in
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
