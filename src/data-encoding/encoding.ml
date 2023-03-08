module Hlist = Commons.Hlist

[@@@warning "-30"]

type 'a t =
  { json : 'a Json_data_encoding.Encoding.t
  ; binary : 'a Binary_data_encoding.Encoding.t
  }

type _ tuple =
  | [] : unit Hlist.t tuple
  | ( :: ) : 'a t * 'b Hlist.t tuple -> ('a * 'b) Hlist.t tuple

type _ obj =
  | [] : unit Hlist.t obj
  | ( :: ) : 'a field * 'b Hlist.t obj -> ('a * 'b) Hlist.t obj

and _ field =
  | Req :
      { encoding : 'a t
      ; name : string
      }
      -> 'a field
  | Opt :
      { encoding : 'a t
      ; name : string
      }
      -> 'a option field

[@@@warning "+30"]

let to_json { json; binary = _ } = json
let to_binary { json = _; binary } = binary

let unit =
  { json = Json_data_encoding.Encoding.unit; binary = Binary_data_encoding.Encoding.unit }
;;

let bool =
  { json = Json_data_encoding.Encoding.bool; binary = Binary_data_encoding.Encoding.bool }
;;

let int64 =
  { json = Json_data_encoding.Encoding.int64
  ; binary = Binary_data_encoding.Encoding.int64
  }
;;

let string =
  { json = Json_data_encoding.Encoding.string
  ; binary = Binary_data_encoding.Encoding.string `UInt30
  }
;;

let bytes =
  { json =
      Json_data_encoding.Encoding.(
        conv
          ~serialisation:Bytes.to_string
          ~deserialisation:(fun s -> Ok (Bytes.of_string s))
          string)
  ; binary = Binary_data_encoding.Encoding.bytes `UInt30
  }
;;

let conv ~serialisation ~deserialisation encoding =
  { json =
      Json_data_encoding.Encoding.conv ~serialisation ~deserialisation (to_json encoding)
  ; binary =
      Binary_data_encoding.Encoding.conv
        ~serialisation
        ~deserialisation
        (to_binary encoding)
  }
;;

let rec to_json_tuple : type a. a tuple -> a Json_data_encoding.Encoding.tuple = function
  | [] -> Json_data_encoding.Encoding.[]
  | t :: ts ->
    let t = to_json t in
    let ts = to_json_tuple ts in
    Json_data_encoding.Encoding.( :: ) (t, ts)
;;

let rec to_binary_tuple : type a. a tuple -> a Binary_data_encoding.Encoding.t = function
  | [] -> Binary_data_encoding.Encoding.[]
  | t :: ts ->
    let t = to_binary t in
    let ts = to_binary_tuple ts in
    Binary_data_encoding.Encoding.( :: ) (t, ts)
;;

let tuple p =
  { json = Json_data_encoding.Encoding.tuple (to_json_tuple p)
  ; binary = to_binary_tuple p
  }
;;

let rec to_json_obj : type a. a obj -> a Json_data_encoding.Encoding.obj = function
  | [] -> Json_data_encoding.Encoding.[]
  | Req { encoding = t; name } :: ts ->
    let t = to_json t in
    let ts = to_json_obj ts in
    Json_data_encoding.Encoding.( :: ) (Req { encoding = t; name }, ts)
  | Opt { encoding = t; name } :: ts ->
    let t = to_json t in
    let ts = to_json_obj ts in
    Json_data_encoding.Encoding.( :: ) (Opt { encoding = t; name }, ts)
;;

let rec to_binary_obj : type a. a obj -> a Binary_data_encoding.Encoding.t = function
  | [] -> Binary_data_encoding.Encoding.[]
  | Req { encoding = t; name = _ } :: ts ->
    let t = to_binary t in
    let ts = to_binary_obj ts in
    Binary_data_encoding.Encoding.( :: ) (t, ts)
  | Opt { encoding = t; name = _ } :: ts ->
    let t = to_binary t in
    let ts = to_binary_obj ts in
    Binary_data_encoding.Encoding.( :: ) (Binary_data_encoding.Encoding.option t, ts)
;;

let obj p =
  { json = Json_data_encoding.Encoding.obj (to_json_obj p); binary = to_binary_obj p }
;;

let req name encoding = Req { encoding; name }
let opt name encoding = Opt { encoding; name }
let split json binary = { json; binary }

module Union = struct
  type ('tag, 'payload, 'union) case_descr =
    { json : ('payload, 'union) Json_data_encoding.Encoding.case_descr
    ; binary : ('tag, 'payload, 'union) Binary_data_encoding.Encoding.case_descr
    }

  type ('tag, 'p, 'a) case_and_payload = ('tag, 'p, 'a) case_descr * 'p
  type ('tag, 'a) anycase = AnyC : ('tag, _, 'a) case_descr -> ('tag, 'a) anycase

  type ('tag, 'a) anycaseandpayload =
    | AnyP : ('tag, _, 'a) case_and_payload -> ('tag, 'a) anycaseandpayload

  let case binary_tag json_tag (encoding : _ t) inject =
    { json = Json_data_encoding.Encoding.Union.case json_tag encoding.json inject
    ; binary = Binary_data_encoding.Encoding.Union.case binary_tag encoding.binary inject
    }
  ;;

  let case_unit binary_tag json_tag inject = case binary_tag json_tag unit inject

  let union tag_encoding cases serialisation tag_selection string_selection =
    let json =
      Json_data_encoding.Encoding.Union.union
        (List.map
           (fun (AnyC { json; binary = _ }) -> Json_data_encoding.Encoding.AnyC json)
           cases)
        (fun x ->
          let (AnyP ({ json; binary = _ }, p)) = serialisation x in
          Json_data_encoding.Encoding.AnyP (json, p))
        (fun tag ->
          match string_selection tag with
          | Ok (AnyC { json; binary = _ }) -> Ok (Json_data_encoding.Encoding.AnyC json)
          | Error _ as error -> error)
    in
    let binary =
      Binary_data_encoding.Encoding.Union.union
        tag_encoding
        (List.map
           (fun (AnyC { json = _; binary }) -> Binary_data_encoding.Encoding.AnyC binary)
           cases)
        (fun x ->
          let (AnyP ({ json = _; binary }, p)) = serialisation x in
          Binary_data_encoding.Encoding.AnyP (binary, p))
        (fun tag ->
          match tag_selection tag with
          | Ok (AnyC { json = _; binary }) ->
            Ok (Binary_data_encoding.Encoding.AnyC binary)
          | Error _ as error -> error)
    in
    ({ json; binary } : _ t)
  ;;

  let either (l : 'a t) (r : 'b t) : ('a, 'b) Either.t t =
    { json = Json_data_encoding.Encoding.Union.either l.json r.json
    ; binary = Binary_data_encoding.Encoding.Union.either l.binary r.binary
    }
  ;;
end

let%expect_test _ =
  let w e v =
    let s = Binary_data_encoding.Writer.string_of e.binary v in
    let s = Result.get_ok s in
    Format.printf
      "%a\n"
      (Format.pp_print_seq
         ~pp_sep:(fun _ () -> ())
         (fun fmt c -> Format.fprintf fmt "%02x" (Char.code c)))
      (String.to_seq s);
    let j =
      Buffy.W.to_string (fun dst -> Json_data_encoding.Construct.write dst e.json v)
    in
    let j = Result.get_ok j in
    Format.printf "%s\n" j
  in
  w unit ();
  [%expect {| {} |}];
  w (Union.either unit bool) (Either.Left ());
  [%expect {|
    ff
    "Left" |}];
  w (Union.either unit bool) (Either.Right false);
  [%expect {|
    0000
    {"Right":false} |}];
  w (Union.either unit bool) (Either.Right true);
  [%expect {|
    00ff
    {"Right":true} |}]
;;
