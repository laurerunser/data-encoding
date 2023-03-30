type result =
  { chunks : Mtime.Span.t list
  ; totals : Mtime.Span.t list
  }

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

let timer f ss =
  let t0 = Mtime_clock.now () in
  let rec run tacc f ss =
    match ss with
    | [] -> assert false
    | blob :: ss ->
      let src = Buffy.Src.of_string blob in
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
  Format.printf
    "size %12d  -  buffer %07d  -  average chunk %a  -  max chunk %a  -  average total \
     %a  -  max total %a\n"
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

let sizes =
  match Sys.getenv_opt "BENCHDATASIZES" with
  | None -> [ 1_000; 5_000; 25_000; 125_000; 625_000; 3_125_000 ]
  | Some s ->
    let ss = String.split_on_char ',' s in
    List.map int_of_string ss
;;
