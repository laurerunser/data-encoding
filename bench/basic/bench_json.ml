let serialise encoding data dst =
  let state = Buffy.W.mk_state dst in
  Json_data_encoding.Construct.write state encoding data
;;

(* TODO: deserialise *)

let run name encoding make_data =
  Format.printf "%s.to_JSON_string (%d samples)\n" name Benchlib.repeats;
  let buffer = Bytes.make Benchlib.buffer_size '0' in
  List.iter
    (fun size ->
      let data = make_data size in
      let serialisations =
        Benchlib.measurew Benchlib.repeats (serialise encoding data) buffer
      in
      let serialisations = Benchlib.flatten serialisations in
      Benchlib.print_summary size Benchlib.buffer_size serialisations)
    Benchlib.sizes;
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

  let () = run "list(array(record(i64,i64)))" encoding make_data
end
