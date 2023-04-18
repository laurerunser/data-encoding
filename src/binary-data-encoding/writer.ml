[@@@landmark "auto"]

(* TODO: more documentation *)
(* TODO: more assertion checks *)
(* TODO? a reckless mode which doesn't check for consistency? (string/bytes
   length, seq length, etc.) *)
(* TODO? replace partial application by hashconsing? *)

let ( let* ) = Buffy.W.( let* )

let rec writek : type s a. (s, a) Descr.t -> Buffy.W.state -> a -> Buffy.W.written
  = function
  | Unit -> fun state () -> Written { state }
  | Bool ->
    fun state v ->
      if v
      then Buffy.W.write_uint8 state (Magic.bool_true :> int)
      else Buffy.W.write_uint8 state (Magic.bool_false :> int)
  | Numeral { numeral; endianness } -> write_numeral numeral endianness
  | String n ->
    fun state v ->
      (* TODO? a reckless mode which doesn't check for consistency? *)
      (* TODO: descr-specific failure modes could be defined in the Encoding
       module maybe? They are related to descr more than the serialisation
       process. *)
      (* TODO: support for 32-bit machines: don't use `Int63.to_int`, maybe writef
       should take an int63? *)
      if Optint.Int63.compare
           (Optint.Int63.of_int (String.length v))
           (n :> Optint.Int63.t)
         <> 0
      then Failed { state; error = "inconsistent length of string" }
      else Buffy.W.write_string state v
  | Bytes n ->
    fun state v ->
      (* TODO: descr-specific failure modes could be defined in the Encoding
       module maybe? They are related to descr more than the serialisation
       process. *)
      (* TODO: support for 32-bit machines: don't use `Int63.to_int`, maybe writef
       should take an int63? *)
      if Optint.Int63.compare (Optint.Int63.of_int (Bytes.length v)) (n :> Optint.Int63.t)
         <> 0
      then Failed { state; error = "inconsistent length of bytes" }
      else Buffy.W.write_bytes state v
  | LSeq { length; descr } ->
    let writeelt = writek descr in
    let length = Optint.Int63.to_int (length :> Optint.Int63.t) in
    fun state v ->
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
            let* state = writeelt state elt in
            fold state s (length - 1)
      in
      fold state v.seq length
  | USeq { descr } ->
    let writeelt = writek descr in
    fun state v ->
      let rec fold state s =
        match s () with
        | Seq.Nil -> Buffy.W.Written { state }
        | Seq.Cons (elt, s) ->
          let* state = writeelt state elt in
          fold state s
      in
      fold state v
  | Array { length; descr } ->
    let writeelt = writek descr in
    fun state v ->
      if Option.get @@ Commons.Sizedints.Uint62.of_int64 (Int64.of_int (Array.length v))
         <> length
      then Buffy.W.Failed { state; error = "inconsistent array length" }
      else (
        let length = Optint.Int63.to_int (length :> Optint.Int63.t) in
        let rec fold state index =
          if index >= length
          then Buffy.W.Written { state }
          else
            let* state = writeelt state (Array.get v index) in
            fold state (index + 1)
        in
        fold state 0)
  | Option { optioner = _; descr } ->
    let writet = writek descr in
    fun state v ->
      (match v with
       | None ->
         let* state = Buffy.W.write_uint8 state (Magic.option_none_tag :> int) in
         Buffy.W.Written { state }
       | Some v ->
         let* state = Buffy.W.write_uint8 state (Magic.option_some_tag :> int) in
         writet state v)
  | Headered
      { mkheader
      ; headerdescr
      ; writers
      ; readers = _
      ; descr_of_header
      ; equal = _
      ; maximum_size = _
      } ->
    let writeheader = writek headerdescr in
    fun state v ->
      (match mkheader v with
       | Error msg ->
         let error = "error in user-provided mkheader function: " ^ msg in
         Failed { state; error }
       | Ok header ->
         let* state = writeheader state header in
         let writet =
           match Commons.BoundedCache.find writers header with
           | Some writet -> writet
           | None ->
             let writet =
               match descr_of_header header with
               | Error msg ->
                 let error = "error in user-provided encoding function: " ^ msg in
                 fun state _ -> Buffy.W.Failed { state; error }
               | Ok (EDynamic encoding) -> writek encoding
               | Ok (EStatic encoding) -> writek encoding
             in
             Commons.BoundedCache.put writers header writet;
             writet
         in
         writet state v)
  | Fold { chunkdescr; chunkify; readinit = _; reducer = _; equal = _; maximum_size = _ }
    ->
    let writechunk = writek chunkdescr in
    fun state v ->
      let rec fold state chunks =
        match chunks () with
        | Seq.Nil -> Buffy.W.Written { state }
        | Seq.Cons (chunk, chunks) ->
          let* state = writechunk state chunk in
          fold state chunks
      in
      fold state (chunkify v)
  | Conv { serialisation; deserialisation = _; descr } ->
    let writet = writek descr in
    fun state v -> writet state (serialisation v)
  | Size_headered { size = size_numeral; descr } ->
    let writesize = write_numeral size_numeral Encoding.default_endianness in
    let writet = writek descr in
    let size_size_limit =
      (* a size-header implies a size-limit; e.g., uint8 implies 256 *)
      (Query.max_int_of size_numeral :> Optint.Int63.t)
    in
    fun state v ->
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
      (* TODO: preapplication in Query.size_of to avoid interpretation here *)
      (match Query.size_of descr v with
       | Error error -> Failed { error; state }
       | Ok size ->
         if Optint.Int63.compare size size_limit > 0
         then Failed { error = "size-limit exceeded"; state }
         else (
           let size =
             (* TODO: 32bit machines *)
             Query.numeral_of_int size_numeral (Optint.Int63.to_int size)
           in
           let* state = writesize state size in
           writet state v))
  | Size_limit { at_most; descr } ->
    let requested_size_limit =
      (* TODO: support 32bit plateforms *)
      Optint.Int63.to_int (at_most :> Optint.Int63.t)
    in
    let writet = writek descr in
    fun state v ->
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
          let* state = writet state v in
          (match Buffy.W.remove_limit state with
           | Error _ ->
             assert false (* the only remove is here and it is paired with the push *)
           | Ok state -> Written { state }))
      else writet state v
  | Union { tag; serialisation; deserialisation = _; cases = _ } ->
    let writetag = writek tag in
    fun state v ->
      let (AnyP (({ Descr.tag; descr; write; read = _; inject = _ } as case), payload)) =
        serialisation v
      in
      let* state = writetag state tag in
      let writepayload =
        match write with
        | Some writepayload -> writepayload
        | None ->
          let writepayload = writek descr in
          case.write <- Some writepayload;
          writepayload
      in
      writepayload state payload
  | TupNil -> fun state [] -> Written { state }
  | TupCons { tupler = _; head; tail } ->
    let writehead = writek head in
    let writetail = writek tail in
    fun state (v :: vs) ->
      let* state = writehead state v in
      writetail state vs

and write_numeral
  : type a. a Descr.numeral -> Descr.endianness -> Buffy.W.state -> a -> Buffy.W.written
  =
 fun numeral endianness ->
  match numeral, endianness with
  | Int64, Big_endian -> Buffy.W.write_int64_be
  | Int64, Little_endian -> Buffy.W.write_int64_le
  | Int32, Big_endian -> Buffy.W.write_int32_be
  | Int32, Little_endian -> Buffy.W.write_int32_le
  | Uint62, Big_endian ->
    fun state v ->
      (* TODO: avoid conversion to int64 *)
      Buffy.W.write_int64_be state (Optint.Int63.to_int64 (v :> Optint.Int63.t))
  | Uint62, Little_endian ->
    fun state v ->
      (* TODO: avoid conversion to int64 *)
      Buffy.W.write_int64_le state (Optint.Int63.to_int64 (v :> Optint.Int63.t))
  | Uint30, Big_endian ->
    (* TODO: avoid conversion to int32 *)
    fun state v -> Buffy.W.write_int32_be state (Int32.of_int (v :> int))
  | Uint30, Little_endian ->
    (* TODO: avoid conversion to int32 *)
    fun state v -> Buffy.W.write_int32_le state (Int32.of_int (v :> int))
  | Uint16, Big_endian -> fun state v -> Buffy.W.write_uint16_be state (v :> int)
  | Uint16, Little_endian -> fun state v -> Buffy.W.write_uint16_le state (v :> int)
  | Uint8, _ -> fun state v -> Buffy.W.write_uint8 state (v :> int)
;;

let write
  : type s a.
    (s, a) Descr.t
    -> dst:bytes
    -> offset:int
    -> length:int
    -> a
    -> (int, int * string) result
  =
 fun descr ->
  let write = writek descr in
  fun ~dst:buffer ~offset ~length v ->
    if offset + length > Bytes.length buffer
    then Error (0, "length exceeds buffer size")
    else (
      let destination = Buffy.Dst.of_bytes buffer ~offset ~length in
      let state = Buffy.W.mk_state ~maximum_size:length destination in
      match write state v with
      | Suspended _ -> assert false (* the buffer is bigger than length *)
      | Failed { state; error } -> Error (Buffy.W.written state, error)
      | Written { state } -> Ok (Buffy.W.written state))
;;

let string_of
  : type s a. ?buffer_size:int -> (s, a) Descr.t -> a -> (string, string) result
  =
 fun ?buffer_size descr ->
  let write = writek descr in
  fun v -> Buffy.W.to_string ?buffer_size (fun state -> write state v)
;;
