let serialise encoding data dst =
  let state = Buffy.W.mk_state dst in
  Json_data_encoding.Construct.write state encoding data
;;

let run name encoding make_data sizes buffer_size repeats quiet =
  let log s = Benchlib.log s quiet in
  Format.kasprintf log "%s.to_JSON_string (%d samples)\n" name repeats;
  let buffer = Bytes.make buffer_size '0' in
  List.iter
    (fun size ->
      let data = make_data size in
      let serialisations = Benchlib.measurew repeats (serialise encoding data) buffer in
      let serialisations = Benchlib.flatten serialisations in
      Benchlib.print_summary size buffer_size serialisations quiet)
    sizes;
  ()
;;

module M = struct
  type datum =
    { x : int64
    ; y : int64
    }

  type data = datum array list

  let encoding : data Json_data_encoding.Encoding.t =
    let open Json_data_encoding.Encoding in
    list
      (array
         Record.(
           record
             (fun x y -> { x; y })
             [ field "x" (fun { x; _ } -> x) int64; field "y" (fun { y; _ } -> y) int64 ]))
  ;;

  (* produce a data of size [( (8+8)*10 + 1 ) * sz + 4] *)
  let make_data sz =
    let datum = { x = 0L; y = 1L } in
    let arr n = Array.make n datum in
    let rec mk count acc =
      if count <= 0
      then acc
      else if count = 1
      then arr 10 :: acc
      else (
        assert (count >= 2);
        let delta = count mod 5 in
        mk (count - 2) (arr (10 - delta) :: arr (10 + delta) :: acc))
    in
    mk sz []
  ;;

  let main = run "list(array(record(i64,i64)))" encoding make_data

  let main_t =
    let open Benchlib in
    Cmdliner.Term.(const main $ sizes_t $ buffer_size_t $ repeats_t $ quiet_t)
  ;;

  let main_cmd =
    let open Cmdliner in
    let doc = "Runs the json writing benches repeatedly and prints report" in
    let man = [] in
    let info = Cmd.info "json writing bench" ~version:"0.1" ~doc ~man in
    Cmd.v info main_t
  ;;

  let () = exit (Cmdliner.Cmd.eval main_cmd)
end
