let repeats =
  match Sys.getenv_opt "BENCHREPEAT" with
  | None -> 5
  | Some s -> int_of_string s
;;

let buffer_size =
  match Sys.getenv_opt "BENCHBUFFSIZE" with
  | None -> 1_000_000
  | Some s -> int_of_string s
;;

let json_buffer_size =
  match Sys.getenv_opt "BENCHBUFFSIZEJSON" with
  | None -> 1_000_000_000
  | Some s -> int_of_string s
;;

let sizes =
  match Sys.getenv_opt "BENCHDATASIZES" with
  | None -> [ 1_000; 5_000; 25_000; 125_000; 625_000; 3_125_000 ]
  | Some s ->
    let ss = String.split_on_char ',' s in
    List.map int_of_string ss
;;

let json_sizes =
  match Sys.getenv_opt "BENCHDATASIZES" with
  | None -> [ 1_000; 5_000; 25_000; 125_000; 625_000; 3_125_000 ]
  | Some s ->
    let ss = String.split_on_char ',' s in
    List.map int_of_string ss
;;

let quiet =
  match Sys.getenv_opt "BENCHQUIET" with
  | None -> false
  | Some "yes" -> true
  | Some _ -> false
;;

let payload_file_name name size =
  Format.asprintf "payload-%08x-%d" (Stdlib.Hashtbl.hash name) size
;;

let payload_file_name_json name size = payload_file_name name size ^ "_json"

let blits_seq_of_file fname buffer : (bytes * int * int) Seq.t =
  let ic = open_in fname in
  let rec go () =
    match input ic buffer 0 (Bytes.length buffer) with
    | 0 -> Seq.Nil
    | n -> Seq.Cons ((buffer, 0, n), go)
  in
  fun () -> go ()
;;

let src_seq_of_file fname buffer : Buffy.Src.t Seq.t =
  Seq.map
    (fun (b, offset, length) -> Buffy.Src.of_bytes ~offset ~length b)
    (blits_seq_of_file fname buffer)
;;

let strs_seq_of_file fname buffer : string Seq.t =
  Seq.memoize
    (Seq.map
       (fun (b, offset, length) -> Bytes.sub_string b offset length)
       (blits_seq_of_file fname buffer))
;;

type result =
  { chunks : Mtime.Span.t list
  ; totals : Mtime.Span.t list
  }

let ww f buffer =
  let rec run f =
    let dst = Buffy.Dst.of_bytes buffer in
    match f dst with
    | Buffy.W.Written _ -> Ok ()
    | Buffy.W.Failed { error; _ } -> Error error
    | Buffy.W.Suspended { cont; _ } -> run cont
  in
  run f
;;

let timew f buffer =
  let rec run tacc f =
    let dst = Buffy.Dst.of_bytes buffer in
    match f dst with
    | Buffy.W.Written _ | Buffy.W.Failed _ -> tacc
    | Buffy.W.Suspended { cont; _ } ->
      let t = Mtime_clock.now () in
      run (t :: tacc) cont
  in
  let t0 = Mtime_clock.now () in
  let ts = run [ t0 ] f in
  match ts with
  | [] -> assert false
  | [ t0 ] ->
    let totals = [ Mtime.span (Mtime_clock.now ()) t0 ] in
    let chunks = totals in
    { chunks; totals }
  | tend :: ts ->
    let chunks, _ =
      (* we ignore the last chunk because it is for a smaller part of the buffer *)
      List.fold_left (fun (dacc, tprev) t -> Mtime.span t tprev :: dacc, t) ([], tend) ts
    in
    let totals = [ Mtime.span tend t0 ] in
    { chunks; totals }
;;

let null = { chunks = []; totals = [] }

let measurew repeats f buffer =
  let measures = Array.make repeats null in
  Gc.full_major ();
  for i = 0 to pred repeats do
    Array.set measures i (timew f buffer)
  done;
  measures
;;

let rr (f : Buffy.Src.t -> _ Buffy.R.readed) (ss : Buffy.Src.t Seq.t) =
  let rec run f ss =
    match ss () with
    | Seq.Nil -> assert false
    | Seq.Cons (src, ss) ->
      (match f src with
       | Buffy.R.Readed { value; _ } -> Ok value
       | Buffy.R.Failed { error; _ } -> Error error
       | Buffy.R.Suspended { cont; _ } -> run cont ss)
  in
  run f ss
;;

let timer (f : Buffy.Src.t -> _ Buffy.R.readed) (ss : string Seq.t) =
  let t0 = Mtime_clock.now () in
  let rec run tacc f ss =
    match ss () with
    | Seq.Nil -> assert false
    | Seq.Cons (s, ss) ->
      let src = Buffy.Src.of_string s in
      (match f src with
       | Buffy.R.Readed _ | Buffy.R.Failed _ -> tacc
       | Buffy.R.Suspended { cont; _ } ->
         let t = Mtime_clock.now () in
         run (t :: tacc) cont ss)
  in
  let ts = run [ t0 ] f ss in
  match ts with
  | [] -> assert false
  | [ t0 ] ->
    let totals = [ Mtime.span (Mtime_clock.now ()) t0 ] in
    let chunks = totals in
    { chunks; totals }
  | tend :: ts ->
    let chunks, _ =
      (* we ignore the last chunk because it is for a smaller part of the buffer *)
      List.fold_left (fun (dacc, tprev) t -> Mtime.span t tprev :: dacc, t) ([], tend) ts
    in
    let totals = [ Mtime.span tend t0 ] in
    { chunks; totals }
;;

let measurer repeats f ss =
  let measures = Array.make repeats null in
  Gc.full_major ();
  for i = 0 to pred repeats do
    Array.set measures i (timer f ss)
  done;
  measures
;;

(******************)

let timer2
  (f : Buffy.Src.t -> _ Json_data_encoding.Destruct_incremental.result)
  (ss : string Seq.t)
  =
  let t0 = Mtime_clock.now () in
  let rec run tacc f ss =
    match ss () with
    | Seq.Nil -> tacc
    | Seq.Cons (s, ss) ->
      let src = Buffy.Src.of_string s in
      (match (f src : _ Json_data_encoding.Destruct_incremental.result) with
       | Ok _ -> tacc
       | Error e ->
         print_string e;
         print_newline ();
         tacc
       | Await f ->
         let t = Mtime_clock.now () in
         run (t :: tacc) f ss)
  in
  let ts = run [ t0 ] f ss in
  match ts with
  | [] -> assert false
  | [ t0 ] ->
    let totals = [ Mtime.span (Mtime_clock.now ()) t0 ] in
    let chunks = totals in
    { chunks; totals }
  | tend :: ts ->
    let chunks, _ =
      (* we ignore the last chunk because it is for a smaller part of the buffer *)
      List.fold_left (fun (dacc, tprev) t -> Mtime.span t tprev :: dacc, t) ([], tend) ts
    in
    let totals = [ Mtime.span tend t0 ] in
    { chunks; totals }
;;

let measurer2 repeats f ss =
  let measures = Array.make repeats null in
  Gc.full_major ();
  for i = 0 to pred repeats do
    Array.set measures i (timer2 f ss)
  done;
  measures
;;

(************************)

(******************)

let timer3
  (f : Json_data_encoding.JSON.flex -> ('a, string) Stdlib.result)
  (ss : string Seq.t)
  =
  let t0 = Mtime_clock.now () in
  let rec run tacc ss =
    match ss () with
    | Seq.Nil -> tacc
    | Seq.Cons (s, ss) ->
      let src = Ezjsonm.from_string s in
      (match f (src : Json_data_encoding.JSON.compat :> Json_data_encoding.JSON.flex) with
       | Ok _ ->
         let t = Mtime_clock.now () in
         run (t :: tacc) ss
       | Error e ->
         print_string e;
         print_newline ();
         let t = Mtime_clock.now () in
         run (t :: tacc) ss)
  in
  let ts = run [ t0 ] ss in
  match ts with
  | [] -> assert false
  | [ t0 ] ->
    let totals = [ Mtime.span (Mtime_clock.now ()) t0 ] in
    let chunks = totals in
    { chunks; totals }
  | tend :: ts ->
    let chunks, _ =
      (* we ignore the last chunk because it is for a smaller part of the buffer *)
      List.fold_left (fun (dacc, tprev) t -> Mtime.span t tprev :: dacc, t) ([], tend) ts
    in
    let totals = [ Mtime.span tend t0 ] in
    { chunks; totals }
;;

let measurer3 repeats f ss =
  let measures = Array.make repeats null in
  Gc.full_major ();
  for i = 0 to pred repeats do
    Array.set measures i (timer3 f ss)
  done;
  measures
;;

(************************)

let flatten arr =
  Array.fold_left
    (fun { chunks; totals } { chunks = c; totals = t } ->
      { chunks = List.rev_append chunks c; totals = List.rev_append totals t })
    null
    arr
;;

let average_ns arr =
  let sum =
    List.fold_left (fun acc d -> Int64.add acc (Mtime.Span.to_uint64_ns d)) 0L arr
  in
  Int64.div sum (Int64.of_int (List.length arr))
;;

let max_ns arr =
  List.fold_left (fun acc d -> Int64.max acc (Mtime.Span.to_uint64_ns d)) (-1L) arr
;;

let pp_ns fmt ns =
  let s = Int64.div ns Mtime.Span.(to_uint64_ns s) in
  let ns = Int64.rem ns Mtime.Span.(to_uint64_ns s) in
  Format.fprintf fmt "%5Ld.%0.9Ld" s ns
;;

let print_summary size buffer_size { chunks; totals } =
  if quiet
  then ()
  else
    Format.printf
      "size(abstract) %12d  -  buffer(bytes) %07d  -  average chunk %a  -  max chunk %a  \
       -  average total %a  -  max total %a\n"
      size
      buffer_size
      pp_ns
      (average_ns chunks)
      pp_ns
      (max_ns chunks)
      pp_ns
      (average_ns totals)
      pp_ns
      (max_ns totals)
;;

let log s = if quiet then () else print_endline s

include Benchable
