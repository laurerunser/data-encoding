module Hlist = Commons.Hlist

type 'a testable =
  { encoding : 'a Json_data_encoding.Encoding.t
  ; generator : 'a QCheck2.Gen.t
  ; equal : 'a -> 'a -> bool
  }

let rec testable_of_encoding : type t. t Json_data_encoding.Encoding.t -> t testable
  = function
  | Unit ->
    { encoding = Json_data_encoding.Encoding.unit
    ; generator = QCheck2.Gen.unit
    ; equal = Unit.equal
    }
  | Int64 ->
    { encoding = Json_data_encoding.Encoding.int64
    ; generator = QCheck2.Gen.int64
    ; equal = Int64.equal
    }
  | String ->
    { encoding = Json_data_encoding.Encoding.string
    ; generator = QCheck2.Gen.(string_size (0 -- 10))
    ; equal = String.equal
    }
  | Tuple t -> testable_of_encoding_tuple t
  | Object t -> testable_of_encoding_object t
  | Conv _ -> failwith "TODO"

and testable_of_encoding_tuple : type t. t Json_data_encoding.Encoding.tuple -> t testable
  = function
  | [] ->
    { encoding = Json_data_encoding.Encoding.tuple []
    ; generator = QCheck2.Gen.pure Hlist.[]
    ; equal = (fun [] [] -> true)
    }
  | head :: tail ->
    let head = testable_of_encoding head in
    let tail = testable_of_encoding_tuple tail in
    (match tail.encoding with
    | Tuple tail_encoding ->
      { encoding = Json_data_encoding.Encoding.Tuple (head.encoding :: tail_encoding)
      ; generator =
          QCheck2.Gen.map2 (fun h t -> Hlist.( :: ) (h, t)) head.generator tail.generator
      ; equal = (fun (ah :: at) (bh :: bt) -> head.equal ah bh && tail.equal at bt)
      }
    | Object _ -> assert false
    | Conv _ -> assert false)

and testable_of_encoding_object : type t. t Json_data_encoding.Encoding.obj -> t testable
  = function
  | [] ->
    { encoding = Json_data_encoding.Encoding.obj []
    ; generator = QCheck2.Gen.pure Hlist.[]
    ; equal = (fun [] [] -> true)
    }
  | Req { encoding = head; name } :: tail ->
    let head = testable_of_encoding head in
    let tail = testable_of_encoding_object tail in
    (match tail.encoding with
    | Object tail_encoding ->
      { encoding =
          Json_data_encoding.Encoding.Object
            (Req { encoding = head.encoding; name } :: tail_encoding)
      ; generator =
          QCheck2.Gen.map2 (fun h t -> Hlist.( :: ) (h, t)) head.generator tail.generator
      ; equal = (fun (ah :: at) (bh :: bt) -> head.equal ah bh && tail.equal at bt)
      }
    | Tuple _ -> assert false
    | Conv _ -> assert false)
  | Opt { encoding = head; name } :: tail ->
    let head = testable_of_encoding head in
    let tail = testable_of_encoding_object tail in
    (match tail.encoding with
    | Object tail_encoding ->
      { encoding =
          Json_data_encoding.Encoding.Object
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
    | Conv _ -> assert false)
;;

type any_testable = AnyTestable : _ testable -> any_testable

let mk e = AnyTestable (testable_of_encoding e)

let ( let* ) x f =
  match x with
  | Error _ -> false
  | Ok v -> f v
;;

let to_test (AnyTestable { encoding; generator; equal }) =
  QCheck2.Test.make generator (fun v ->
      let* j = Json_data_encoding.Backend.construct encoding v in
      let* vv = Json_data_encoding.Backend.destruct encoding j in
      equal v vv)
;;
