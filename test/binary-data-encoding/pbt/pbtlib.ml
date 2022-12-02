module Hlist = Commons.Hlist

type 'a testable =
  { encoding : 'a Binary_data_encoding.Encoding.t
  ; generator : 'a QCheck2.Gen.t
  ; equal : 'a -> 'a -> bool
  }

let rec testable_of_encoding : type t. t Binary_data_encoding.Encoding.t -> t testable =
 fun encoding ->
  match encoding with
  | Unit -> { encoding; generator = QCheck2.Gen.unit; equal = Unit.equal }
  | Int64 -> { encoding; generator = QCheck2.Gen.int64; equal = Int64.equal }
  | UInt64 ->
    { encoding
    ; generator = QCheck2.Gen.(map Unsigned.UInt64.of_int64 int64)
    ; equal = Unsigned.UInt64.equal
    }
  | Int32 -> { encoding; generator = QCheck2.Gen.int32; equal = Int32.equal }
  | UInt32 ->
    { encoding
    ; generator = QCheck2.Gen.(map Unsigned.UInt32.of_int32 int32)
    ; equal = Unsigned.UInt32.equal
    }
  | UInt16 ->
    { encoding
    ; generator = QCheck2.Gen.(map Unsigned.UInt16.of_int (0 -- 0xff_ff))
    ; equal = Unsigned.UInt16.equal
    }
  | String n ->
    let n = Unsigned.UInt32.to_int n in
    { encoding; generator = QCheck2.Gen.(string_size (pure n)); equal = String.equal }
  | Bytes n ->
    let n = Unsigned.UInt32.to_int n in
    { encoding
    ; generator = QCheck2.Gen.(map Bytes.unsafe_of_string (string_size (pure n)))
    ; equal = Bytes.equal
    }
  | Option t ->
    let t = testable_of_encoding t in
    { encoding
    ; generator = QCheck2.Gen.(option t.generator)
    ; equal = Option.equal t.equal
    }
  | Headered _ -> failwith "TODO"
  | [] -> { encoding; generator = QCheck2.Gen.pure Hlist.[]; equal = (fun [] [] -> true) }
  | head :: tail ->
    let head = testable_of_encoding head in
    let tail = testable_of_encoding tail in
    { encoding = head.encoding :: tail.encoding
    ; generator =
        QCheck2.Gen.map2 (fun h t -> Hlist.( :: ) (h, t)) head.generator tail.generator
    ; equal = (fun (ah :: at) (bh :: bt) -> head.equal ah bh && tail.equal at bt)
    }
;;

type any_testable = AnyTestable : _ testable -> any_testable

let mk e = AnyTestable (testable_of_encoding e)

let ( let* ) x f =
  match x with
  | Error _ -> false
  | Ok v -> f v
;;

let to_test (AnyTestable { encoding; generator; equal }) =
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
