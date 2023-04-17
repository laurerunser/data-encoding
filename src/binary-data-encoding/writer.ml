(* TODO: more documentation *)
(* TODO: more assertion checks *)
(* TODO: benchmark and optimise (later, after we add more features) *)

let ( let* ) = Buffy.W.( let* )

let rec writek : type s a. Buffy.W.state -> (s, a) Descr.t -> a -> Buffy.W.written =
 fun state encoding v ->
  match encoding with
  | Unit -> Written { state }
  | Bool ->
    if v
    then Buffy.W.write_uint8 state (Magic.bool_true :> int)
    else Buffy.W.write_uint8 state (Magic.bool_false :> int)
  | Numeral { numeral; endianness } -> write_numeral state numeral endianness v
  | String n ->
    (* TODO: support chunk writing of strings so that it's possible to serialise
     a big blob onto several small buffers *)
    (* TODO: encoding-specific failure modes could be defined in the Encoding
       module maybe? They are related to encodings more than the serialisation
       process. *)
    (* TODO: support for 32-bit machines: don't use `Int63.to_int`, maybe writef
       should take an int63? *)
    if Optint.Int63.compare (Optint.Int63.of_int (String.length v)) (n :> Optint.Int63.t)
       <> 0
    then Failed { state; error = "inconsistent length of string" }
    else Buffy.W.write_string state v
  | Bytes n ->
    (* TODO: support chunk writing of bytes so that it's possible to serialise
     a big blob onto several small buffers *)
    (* TODO: encoding-specific failure modes could be defined in the Encoding
       module maybe? They are related to encodings more than the serialisation
       process. *)
    (* TODO: support for 32-bit machines: don't use `Int63.to_int`, maybe writef
       should take an int63? *)
    if Optint.Int63.compare (Optint.Int63.of_int (Bytes.length v)) (n :> Optint.Int63.t)
       <> 0
    then Failed { state; error = "inconsistent length of bytes" }
    else Buffy.W.write_bytes state v
  | LSeq { length; elementencoding } ->
    let length = Optint.Int63.to_int (length :> Optint.Int63.t) in
    let rec fold state s length =
      match s () with
      | Seq.Nil ->
        if length = 0
        then Buffy.W.Written { state }
        else Buffy.W.Failed { state; error = "inconsistent length of seq" }
      | Seq.Cons (elt, s) ->
        if length <= 0
        then Buffy.W.Failed { state; error = "inconsistent length of seq" }
        else
          let* state = writek state elementencoding elt in
          fold state s (length - 1)
    in
    fold state v.seq length
  | USeq { elementencoding } ->
    let rec fold state s =
      match s () with
      | Seq.Nil -> Buffy.W.Written { state }
      | Seq.Cons (elt, s) ->
        let* state = writek state elementencoding elt in
        fold state s
    in
    fold state v
  | Array { length; elementencoding } ->
    if Option.get @@ Commons.Sizedints.Uint62.of_int64 (Int64.of_int (Array.length v))
       <> length
    then Buffy.W.Failed { state; error = "inconsistent array length" }
    else (
      let length = Optint.Int63.to_int (length :> Optint.Int63.t) in
      let rec fold state index =
        if index >= length
        then Buffy.W.Written { state }
        else
          let* state = writek state elementencoding (Array.get v index) in
          fold state (index + 1)
      in
      fold state 0)
  | Option (_, t) ->
    (match v with
     | None ->
       let* state = Buffy.W.write_uint8 state (Magic.option_none_tag :> int) in
       Buffy.W.Written { state }
     | Some v ->
       let* state = Buffy.W.write_uint8 state (Magic.option_some_tag :> int) in
       writek state t v)
  | Headered { mkheader; headerencoding; mkencoding; equal = _; maximum_size = _ } ->
    (match mkheader v with
     | Error msg ->
       let error = "error in user-provided mkheader function: " ^ msg in
       Failed { state; error }
     | Ok header ->
       let* state = writek state headerencoding header in
       (match mkencoding header with
        | Error msg ->
          let error = "error in user-provided encoding function: " ^ msg in
          Failed { state; error }
        | Ok (EDynamic encoding) -> writek state encoding v
        | Ok (EStatic encoding) -> writek state encoding v))
  | Fold
      { chunkencoding; chunkify; readinit = _; reducer = _; equal = _; maximum_size = _ }
    ->
    let rec fold state chunks =
      match chunks () with
      | Seq.Nil -> Buffy.W.Written { state }
      | Seq.Cons (chunk, chunks) ->
        let* state = writek state chunkencoding chunk in
        fold state chunks
    in
    fold state (chunkify v)
  | Conv { serialisation; deserialisation = _; encoding } ->
    writek state encoding (serialisation v)
  | Size_headered { size = size_numeral; encoding } ->
    let size_size_limit =
      (* a size-header implies a size-limit; e.g., uint8 implies 256 *)
      (Query.max_int_of size_numeral :> Optint.Int63.t)
    in
    let other_size_limit = state.maximum_size - Buffy.W.written state in
    let other_size_limit =
      match state.size_limits with
      | [] -> other_size_limit
      | previous_limit :: _ ->
        min (previous_limit - Buffy.W.written state) other_size_limit
    in
    let other_size_limit =
      (* TODO: 32bit compat *)
      Optint.Int63.of_int other_size_limit
    in
    let size_limit =
      if Optint.Int63.compare size_size_limit other_size_limit <= 0
      then size_size_limit
      else other_size_limit
    in
    (* TODO: have [Query.size_of_with_limit] to fail earlier *)
    (match Query.size_of encoding v with
     | Error error -> Failed { error; state }
     | Ok size ->
       if Optint.Int63.compare size size_limit > 0
       then Failed { error = "size-limit exceeded"; state }
       else (
         let size =
           (* TODO: 32bit machines *)
           Query.numeral_of_int size_numeral (Optint.Int63.to_int size)
         in
         let* state =
           write_numeral state size_numeral Encoding_public.default_endianness size
         in
         writek state encoding v))
  | Size_limit { at_most; encoding } ->
    let requested_size_limit =
      (* TODO: support 32bit plateforms *)
      Optint.Int63.to_int (at_most :> Optint.Int63.t)
    in
    (* Because the constructors are nested, the effective size-limit may be
       lower than that provided in the constructor. We compute the largest
       possible size-limit. *)
    let possible_size_limit = state.maximum_size - Buffy.W.written state in
    let possible_size_limit =
      match state.size_limits with
      | [] -> possible_size_limit
      | previous_limit :: _ ->
        min (previous_limit - Buffy.W.written state) possible_size_limit
    in
    if requested_size_limit < possible_size_limit
    then (
      match Buffy.W.push_limit state requested_size_limit with
      | Error _ -> assert false (* the [min]s above prevent this *)
      | Ok state ->
        let* state = writek state encoding v in
        (match Buffy.W.remove_limit state with
         | Error _ ->
           assert false (* the only remove is here and it is paired with the push *)
         | Ok state -> Written { state }))
    else writek state encoding v
  | Union { tag = tag_encoding; serialisation; deserialisation = _; cases = _ } ->
    let (AnyP ({ Descr.tag; encoding; inject = _ }, payload)) = serialisation v in
    let* state = writek state tag_encoding tag in
    writek state encoding payload
  | TupNil -> Written { state }
  | TupCons (_, t, ts) ->
    (match v with
     | v :: vs ->
       let* state = writek state t v in
       writek state ts vs)

and write_numeral
  : type a. Buffy.W.state -> a Descr.numeral -> Descr.endianness -> a -> Buffy.W.written
  =
 fun state numeral endianness v ->
  match numeral, endianness with
  | Int64, Big_endian -> Buffy.W.write_int64_be state v
  | Int64, Little_endian -> Buffy.W.write_int64_le state v
  | Int32, Big_endian -> Buffy.W.write_int32_be state v
  | Int32, Little_endian -> Buffy.W.write_int32_le state v
  | Uint62, Big_endian ->
    Buffy.W.write_int64_be state (Optint.Int63.to_int64 (v :> Optint.Int63.t))
  | Uint62, Little_endian ->
    Buffy.W.write_int64_le state (Optint.Int63.to_int64 (v :> Optint.Int63.t))
  | Uint30, Big_endian -> Buffy.W.write_int32_be state (Int32.of_int (v :> int))
  | Uint30, Little_endian -> Buffy.W.write_int32_le state (Int32.of_int (v :> int))
  | Uint16, Big_endian -> Buffy.W.write_uint16_be state (v :> int)
  | Uint16, Little_endian -> Buffy.W.write_uint16_le state (v :> int)
  | Uint8, _ -> Buffy.W.write_uint8 state (v :> int)
;;

let write
  : type s a.
    dst:bytes
    -> offset:int
    -> length:int
    -> (s, a) Descr.t
    -> a
    -> (int, int * string) result
  =
 fun ~dst:buffer ~offset ~length encoding v ->
  if offset + length > Bytes.length buffer
  then Error (0, "length exceeds buffer size")
  else (
    let destination = Buffy.Dst.of_bytes buffer ~offset ~length in
    let state = Buffy.W.mk_state ~maximum_size:length destination in
    match writek state encoding v with
    | Suspended _ -> assert false (* the buffer is bigger than length *)
    | Failed { state; error } -> Error (Buffy.W.written state, error)
    | Written { state } -> Ok (Buffy.W.written state))
;;

let string_of
  : type s a. ?buffer_size:int -> (s, a) Descr.t -> a -> (string, string) result
  =
 fun ?buffer_size encoding v ->
  Buffy.W.to_string ?buffer_size (fun state -> writek state encoding v)
;;
