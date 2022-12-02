module Hlist = Commons.Hlist

type 'a testable =
  { encoding : 'a Data_encoding.Encoding.t
  ; generator : 'a QCheck2.Gen.t
  ; equal : 'a -> 'a -> bool
  }

let rec testable_of_encoding : type t. t Data_encoding.Encoding.t -> t testable = function
  | Unit ->
    { encoding = Data_encoding.Encoding.unit
    ; generator = QCheck2.Gen.unit
    ; equal = Unit.equal
    }
  | Int64 ->
    { encoding = Data_encoding.Encoding.int64
    ; generator = QCheck2.Gen.int64
    ; equal = Int64.equal
    }
  | String ->
    (* TODO: length *)
    { encoding = Data_encoding.Encoding.string
    ; generator = QCheck2.Gen.(string_size (0 -- 10))
    ; equal = String.equal
    }
  | Bytes ->
    { encoding = Data_encoding.Encoding.bytes
    ; generator = QCheck2.Gen.(map Bytes.unsafe_of_string (string_size (0 -- 10)))
    ; equal = Bytes.equal
    }
  | Tuple t -> testable_of_encoding_tuple t
  | Object t -> testable_of_encoding_object t
  | Split _ -> failwith "TODO"

and testable_of_encoding_tuple : type t. t Data_encoding.Encoding.tuple -> t testable
  = function
  | [] ->
    { encoding = Data_encoding.Encoding.tuple []
    ; generator = QCheck2.Gen.pure Hlist.[]
    ; equal = (fun [] [] -> true)
    }
  | head :: tail ->
    let head = testable_of_encoding head in
    let tail = testable_of_encoding_tuple tail in
    (match tail.encoding with
    | Tuple tail_encoding ->
      { encoding = Data_encoding.Encoding.Tuple (head.encoding :: tail_encoding)
      ; generator =
          QCheck2.Gen.map2 (fun h t -> Hlist.( :: ) (h, t)) head.generator tail.generator
      ; equal = (fun (ah :: at) (bh :: bt) -> head.equal ah bh && tail.equal at bt)
      }
    | Object _ -> assert false
    | Split _ -> assert false)

and testable_of_encoding_object : type t. t Data_encoding.Encoding.obj -> t testable
  = function
  | [] ->
    { encoding = Data_encoding.Encoding.obj []
    ; generator = QCheck2.Gen.pure Hlist.[]
    ; equal = (fun [] [] -> true)
    }
  | Req { encoding = head; name } :: tail ->
    let head = testable_of_encoding head in
    let tail = testable_of_encoding_object tail in
    (match tail.encoding with
    | Object tail_encoding ->
      { encoding =
          Data_encoding.Encoding.Object
            (Req { encoding = head.encoding; name } :: tail_encoding)
      ; generator =
          QCheck2.Gen.map2 (fun h t -> Hlist.( :: ) (h, t)) head.generator tail.generator
      ; equal = (fun (ah :: at) (bh :: bt) -> head.equal ah bh && tail.equal at bt)
      }
    | Tuple _ -> assert false
    | Split _ -> assert false)
  | Opt { encoding = head; name } :: tail ->
    let head = testable_of_encoding head in
    let tail = testable_of_encoding_object tail in
    (match tail.encoding with
    | Object tail_encoding ->
      { encoding =
          Data_encoding.Encoding.Object
            (Opt { encoding = head.encoding; name } :: tail_encoding)
      ; generator =
          QCheck2.Gen.map2
            (fun h t -> Hlist.( :: ) (h, t))
            (QCheck2.Gen.option head.generator)
            tail.generator
      ; equal =
          (fun (ah :: at) (bh :: bt) -> Option.equal head.equal ah bh && tail.equal at bt)
      }
    | Tuple _ -> assert false
    | Split _ -> assert false)
;;

type any_testable = AnyTestable : _ testable -> any_testable

let mk e = AnyTestable (testable_of_encoding e)

let ( let* ) x f =
  match x with
  | Error _ -> false
  | Ok v -> f v
;;

let to_test_binary (AnyTestable { encoding; generator; equal }) =
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

let to_test_json (AnyTestable { encoding; generator; equal }) =
  (* JSON *)
  let e_j = Data_encoding.Encoding.to_json encoding in
  QCheck2.Test.make generator (fun v ->
      let* j = Json_data_encoding.Backend.construct e_j v in
      let* vv = Json_data_encoding.Backend.destruct e_j j in
      equal v vv)
;;
