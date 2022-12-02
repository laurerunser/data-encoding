(* setup *)
let e = Data_encoding.Encoding.unit
let v = ()

(* binary *)
let e_b = Data_encoding.Encoding.to_binary e

let () =
  let dst = Bytes.make 100 '\x00' in
  let offset = 0 in
  let maximum_length = 0 in
  let () =
    match Binary_data_encoding.Backend.write ~dst ~offset ~maximum_length e_b v with
    | Error (_, _) -> assert false
    | Ok _ -> ()
  in
  let src = Bytes.to_string dst in
  let vv =
    match Binary_data_encoding.Backend.read ~src ~offset ~maximum_length e_b with
    | Error _ -> assert false
    | Ok (vv, _) -> vv
  in
  assert (v = vv)
;;

(* JSON *)
let e_j = Data_encoding.Encoding.to_json e

let () =
  let j =
    match Json_data_encoding.Backend.construct e_j v with
    | Error _ -> assert false
    | Ok j -> j
  in
  let vv =
    match Json_data_encoding.Backend.destruct e_j j with
    | Error _ -> assert false
    | Ok vv -> vv
  in
  assert (v = vv)
;;
