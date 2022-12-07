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

let rec pp_of_encoding
    : type t. t Binary_data_encoding.Encoding.t -> Format.formatter -> t -> unit
  =
 fun encoding fmt v ->
  match encoding with
  | Unit -> Format.fprintf fmt "()"
  | Int64 -> Format.fprintf fmt "%Ld" v
  | UInt64 -> Unsigned.UInt64.pp fmt v
  | Int32 -> Format.fprintf fmt "%ld" v
  | UInt32 -> Unsigned.UInt32.pp fmt v
  | UInt16 -> Unsigned.UInt16.pp fmt v
  | String _ -> Format.fprintf fmt "%s" v
  | Bytes _ -> Format.fprintf fmt "%s" (Bytes.unsafe_to_string v)
  | Option t ->
    (match v with
    | None -> Format.fprintf fmt "None"
    | Some v ->
      let pp = pp_of_encoding t in
      Format.fprintf fmt "Some(%a)" pp v)
  | Headered { mkheader; headerencoding = _; encoding } ->
    let pp = pp_of_encoding (encoding (mkheader v)) in
    Format.fprintf fmt "%a" pp v
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

let to_test : type t. string -> t Binary_data_encoding.Encoding.t -> QCheck2.Test.t =
 fun name encoding ->
  let generator = generator_of_encoding encoding in
  let equal = equal_of_encoding encoding in
  let print = print_of_encoding encoding in
  let offset = 0 in
  (* TODO: adapt length to encoding *)
  let length = 100 in
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
