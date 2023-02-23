(* TODO: expect tests *)

type source =
  { blob : string
  ; offset : int
  ; length : int
  ; readed : int (* [read] is ambiguous so we make it unambiguously past as [readed] *)
  ; stop_hints : int list
        (* this list is grown when there is a size-header in the encoded binary data *)
  ; maximum_length : int
  }

let rec check_stop_hints base stops maximum_length =
  match stops with
  | [] -> true
  | stop :: stops ->
    base <= stop && stop <= maximum_length && check_stop_hints stop stops maximum_length
;;

let mk_source ?(maximum_length = max_int) ?(stop_hints = []) blob offset length =
  if offset < 0 then failwith "Buffy.R.mk_source: negative offset";
  if length < 0 then failwith "Buffy.R.mk_source: negative length";
  if offset + length > String.length blob
  then failwith "Buffy.R.mk_source: offset+length overflow";
  if not (check_stop_hints 0 stop_hints maximum_length)
  then failwith "Buffy.R.mk_source: stops out of bounds or out of order";
  { blob; offset; length; readed = 0; stop_hints; maximum_length }
;;

let bump_readed source reading =
  let readed = source.readed + reading in
  assert (readed <= source.length);
  assert (readed <= source.maximum_length);
  { source with readed }
;;

let rec check_last_stop stops maximum_length =
  match stops with
  | [] -> true
  | [ stop ] -> stop < maximum_length
  | stop :: stops ->
    assert (stop >= 0);
    assert (stop < maximum_length);
    assert (stop <= List.hd stops);
    check_last_stop stops maximum_length
;;

let set_maximum_length source maximum_length =
  if maximum_length < 0
  then raise (Invalid_argument "Buffy.R.set_maximum_length: negative length");
  if maximum_length > source.maximum_length
  then
    raise (Invalid_argument "Buffy.R.set_maximum_length: cannot increase maximum length");
  if not (check_last_stop source.stop_hints maximum_length)
  then
    raise
      (Invalid_argument
         "Buffy.R.set_maximum_length: cannot set maximum length lower than expected stop");
  { source with maximum_length }
;;

(* TODO: there needs to be more tests for stop hints in conjunction with
   suspend-resume *)

let push_stop source length =
  assert (length >= 0);
  if source.readed + length > source.maximum_length
  then Error "expected-stop exceeds maximum-length"
  else (
    let requested_stop = source.readed + length in
    match source.stop_hints with
    | [] -> Ok { source with stop_hints = [ requested_stop ] }
    | previously_requested_stop :: _ ->
      if requested_stop > previously_requested_stop
      then Error "expected-stop exceeds previously requested stop"
      else Ok { source with stop_hints = requested_stop :: source.stop_hints })
;;

let peak_stop source =
  match source.stop_hints with
  | [] -> None
  | stop :: _ -> Some stop
;;

(* TODO? return a [unit readed] instead? With a failed if the stop is different
   from the readed? *)
let pop_stop source =
  match source.stop_hints with
  | [] -> Error "expected an expected-stop but found none"
  | stop :: stop_hints -> Ok (stop, { source with stop_hints })
;;

type 'a readed =
  | Readed of
      { source : source
      ; value : 'a
      }
  | Failed of
      { source : source
      ; error : string
      }
  | Suspended of
      { source : source
            (* TODO? add a field to indicate number of chars read from previous buffer *)
      ; cont : string -> int -> int -> 'a readed
      }

let source_too_small_to_continue_message = "new source blob is too small to continue"

let readf source reading read =
  assert (reading >= 0);
  if source.readed + reading > source.maximum_length
  then Failed { source; error = "maximum-length exceeded" }
  else if match source.stop_hints with
          | [] -> false
          | stop :: _ -> source.readed + reading > stop
  then Failed { source; error = "expected-stop point exceeded" }
  else if source.readed + reading <= source.length
  then (
    (* full reading can happen immediately, just do it *)
    let value = read source.blob (source.offset + source.readed) in
    let source = bump_readed source reading in
    Readed { source; value })
  else if source.readed = source.length
  then (
    (* the source was fully consumed, we do a simple continuation *)
    (* To avoid the continuation capturing the [source] we compute some values immediately *)
    let maximum_length = source.maximum_length - source.readed in
    let stop_hints = List.map (fun stop -> stop - source.readed) source.stop_hints in
    Suspended
      { source
      ; cont =
          (fun blob offset length ->
            let source = mk_source ~maximum_length ~stop_hints blob offset length in
            if reading > length
            then Failed { source; error = source_too_small_to_continue_message }
            else (
              let value = read source.blob source.offset in
              let source = bump_readed source reading in
              Readed { source; value }))
      })
  else (
    (* the source has left-over bytes, do a two-step continuation to use those *)
    (* TODO? provide a [copy_threshold] to let the user control when
         the partial read of the remaining of the previous source is
         copied and when is it kept a reference of. *)
    (* TODO? provide a copy_limit and return [Failed] if going over *)
    let split_reading_buffer = Bytes.make reading '\x00' in
    let split_reading_left_length = source.length - source.readed in
    assert (split_reading_left_length > 0);
    Bytes.blit_string
      source.blob
      (source.offset + source.readed)
      split_reading_buffer
      0
      split_reading_left_length;
    let split_reading_right_length = reading - split_reading_left_length in
    assert (split_reading_right_length > 0);
    let maximum_length = source.maximum_length - source.readed in
    let stop_hints = List.map (fun stop -> stop - source.readed) source.stop_hints in
    Suspended
      { source
      ; cont =
          (fun blob offset length ->
            (* First check that the current here readf has enough data *)
            if reading > split_reading_left_length + length
            then (
              (* TODO: what source should we return here? *)
              let source = mk_source "DUMMYTODO" 0 0 in
              Failed { source; error = source_too_small_to_continue_message })
            else (
              (* prepare for this small here readf *)
              Bytes.blit_string
                blob
                offset
                split_reading_buffer
                split_reading_left_length
                split_reading_right_length;
              let source =
                mk_source
                  ~maximum_length
                  ~stop_hints
                  (Bytes.unsafe_to_string split_reading_buffer)
                  0
                  reading
              in
              (* actually do this small here read *)
              let value = read source.blob source.offset in
              (* Second prepare the source for giving back *)
              let maximum_length = source.maximum_length - split_reading_left_length in
              let stop_hints =
                List.map (fun stop -> stop - split_reading_left_length) source.stop_hints
              in
              let source = mk_source ~maximum_length ~stop_hints blob offset length in
              let source = bump_readed source split_reading_right_length in
              Readed { source; value }))
      })
;;

let%expect_test _ =
  let pp_source_state fmt source =
    Format.fprintf
      fmt
      "blob: %S, offset: %d, length: %d, readed: %d, stops: [%a], maxlen: %d"
      source.blob
      source.offset
      source.length
      source.readed
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ',')
         Format.pp_print_int)
      source.stop_hints
      source.maximum_length
  in
  let w source ls =
    Format.printf "Source: %a\n" pp_source_state source;
    match
      List.fold_left
        (fun source l ->
          match readf source l (fun b o -> String.sub b o l) with
          | Suspended _ ->
            Format.printf "Suspended!\n";
            raise Exit
          | Failed { error; source } ->
            Format.printf "Error: %S\nSource: %a\n" error pp_source_state source;
            source
          | Readed { value; source } ->
            Format.printf "Ok: %S\nSource: %a\n" value pp_source_state source;
            source)
        source
        ls
    with
    | exception Exit -> ()
    | _ -> ()
  in
  let source = mk_source "foobarbaz" 0 9 in
  w source [ 3; 0; 6; 1 ];
  [%expect
    {|
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 0, stops: [], maxlen: 4611686018427387903
    Ok: "foo"
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 3, stops: [], maxlen: 4611686018427387903
    Ok: ""
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 3, stops: [], maxlen: 4611686018427387903
    Ok: "barbaz"
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 9, stops: [], maxlen: 4611686018427387903
    Suspended! |}];
  let source = mk_source "foobarbaz" ~maximum_length:6 0 9 in
  w source [ 3; 0; 6; 3; 1 ];
  [%expect
    {|
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 0, stops: [], maxlen: 6
    Ok: "foo"
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 3, stops: [], maxlen: 6
    Ok: ""
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 3, stops: [], maxlen: 6
    Error: "maximum-length exceeded"
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 3, stops: [], maxlen: 6
    Ok: "bar"
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 6, stops: [], maxlen: 6
    Error: "maximum-length exceeded"
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 6, stops: [], maxlen: 6 |}];
  ()
;;

let read_small_string source len =
  readf source len (fun src off -> String.sub src off len)
;;

let%expect_test _ =
  let pp_source_state fmt source =
    Format.fprintf
      fmt
      "blob: %S, offset: %d, length: %d, readed: %d, stops: [%a], maxlen: %d"
      source.blob
      source.offset
      source.length
      source.readed
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ',')
         Format.pp_print_int)
      source.stop_hints
      source.maximum_length
  in
  let w source ls =
    Format.printf "Source: %a\n" pp_source_state source;
    match
      List.fold_left
        (fun source l ->
          match read_small_string source l with
          | Suspended _ ->
            Format.printf "Suspended!\n";
            raise Exit
          | Failed { error; source } ->
            Format.printf "Error: %S\nSource: %a\n" error pp_source_state source;
            source
          | Readed { value; source } ->
            Format.printf "Ok: %S\nSource: %a\n" value pp_source_state source;
            source)
        source
        ls
    with
    | exception Exit -> ()
    | _ -> ()
  in
  let source = mk_source "foobarbaz" 0 9 in
  w source [ 3; 0; 6; 1 ];
  [%expect
    {|
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 0, stops: [], maxlen: 4611686018427387903
    Ok: "foo"
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 3, stops: [], maxlen: 4611686018427387903
    Ok: ""
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 3, stops: [], maxlen: 4611686018427387903
    Ok: "barbaz"
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 9, stops: [], maxlen: 4611686018427387903
    Suspended! |}];
  let source = mk_source "foobarbaz" ~maximum_length:6 0 9 in
  w source [ 3; 0; 6; 3; 1 ];
  [%expect
    {|
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 0, stops: [], maxlen: 6
    Ok: "foo"
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 3, stops: [], maxlen: 6
    Ok: ""
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 3, stops: [], maxlen: 6
    Error: "maximum-length exceeded"
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 3, stops: [], maxlen: 6
    Ok: "bar"
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 6, stops: [], maxlen: 6
    Error: "maximum-length exceeded"
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 6, stops: [], maxlen: 6 |}];
  ()
;;

let read_char source =
  if source.readed + 1 > source.maximum_length
  then Failed { source; error = "maximum-length exceeded" }
  else if match source.stop_hints with
          | [] -> false
          | stop :: _ -> source.readed + 1 > stop
  then Failed { source; error = "expected-stop point exceeded" }
  else if source.readed + 1 > source.length
  then
    Suspended
      { source
      ; cont =
          (fun blob offset length ->
            assert (source.readed <= source.length);
            assert (source.readed = source.length);
            (* we are reading 1 char, so we can't have left over from before *)
            let source =
              mk_source
                ~maximum_length:(source.maximum_length - 1)
                ~stop_hints:(List.map (fun stop -> stop - 1) source.stop_hints)
                blob
                offset
                length
            in
            assert (length > 0);
            let value = String.get source.blob source.offset in
            let source = bump_readed source 1 in
            Readed { source; value })
      }
  else (
    let value = String.get source.blob (source.offset + source.readed) in
    let source = bump_readed source 1 in
    Readed { source; value })
;;

let%expect_test _ =
  let pp_source_state fmt source =
    Format.fprintf
      fmt
      "blob: %S, offset: %d, length: %d, readed: %d, stops: [%a], maxlen: %d"
      source.blob
      source.offset
      source.length
      source.readed
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ',')
         Format.pp_print_int)
      source.stop_hints
      source.maximum_length
  in
  let w source =
    Format.printf "Source: %a\n" pp_source_state source;
    let rec go source =
      match read_char source with
      | Suspended _ ->
        Format.printf "Suspended!\n";
        ()
      | Failed { error; source } ->
        Format.printf "Error: %S\nSource: %a\n" error pp_source_state source;
        ()
      | Readed { value; source } ->
        Format.printf "Ok: %c\nSource: %a\n" value pp_source_state source;
        go source
    in
    go source
  in
  let source = mk_source "foobarbaz" 3 3 in
  w source;
  [%expect
    {|
    Source: blob: "foobarbaz", offset: 3, length: 3, readed: 0, stops: [], maxlen: 4611686018427387903
    Ok: b
    Source: blob: "foobarbaz", offset: 3, length: 3, readed: 1, stops: [], maxlen: 4611686018427387903
    Ok: a
    Source: blob: "foobarbaz", offset: 3, length: 3, readed: 2, stops: [], maxlen: 4611686018427387903
    Ok: r
    Source: blob: "foobarbaz", offset: 3, length: 3, readed: 3, stops: [], maxlen: 4611686018427387903
    Suspended! |}];
  let source = mk_source "foobarbaz" ~maximum_length:3 0 9 in
  w source;
  [%expect
    {|
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 0, stops: [], maxlen: 3
    Ok: f
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 1, stops: [], maxlen: 3
    Ok: o
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 2, stops: [], maxlen: 3
    Ok: o
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 3, stops: [], maxlen: 3
    Error: "maximum-length exceeded"
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 3, stops: [], maxlen: 3 |}];
  ()
;;

type 'a chunkreader = string -> int -> int -> 'a chunkreaded

and 'a chunkreaded =
  | K of int * 'a chunkreader
  | Finish of 'a * int

let rec readchunked : type a. source -> a chunkreader -> a readed =
 fun source read ->
  let hard_limit = source.maximum_length - source.readed in
  let hard_limit =
    match source.stop_hints with
    | [] -> hard_limit
    | stop :: _ -> min hard_limit (stop - source.readed)
  in
  let suspendable_limit = source.length - source.readed in
  let reading = min hard_limit suspendable_limit in
  assert (reading <= hard_limit);
  assert (reading >= 0);
  match read source.blob (source.offset + source.readed) reading with
  | Finish (value, readed) ->
    assert (readed <= reading);
    let source = bump_readed source readed in
    Readed { source; value }
  | K (readed, read) ->
    assert (readed <= reading);
    if readed = hard_limit
    then (
      let error = "chunkreader requires more bytes but hard limit was reached" in
      Failed { error; source })
    else (
      let source = bump_readed source readed in
      Suspended
        { source
        ; cont =
            (fun blob offset length ->
              let source =
                mk_source
                  ~maximum_length:(source.maximum_length - source.readed)
                  ~stop_hints:
                    (List.map (fun stop -> stop - source.readed) source.stop_hints)
                  blob
                  offset
                  length
              in
              readchunked source read)
        })
;;

(* TODO: have the user pass the buffer for [read_large_bytes] to avoid major
   allocation. *)

let read_large_bytes source len =
  let dest = Bytes.make len '\000' in
  let rec chunkreader dest_offset blob offset maxreadsize =
    let needsreading = len - dest_offset in
    if needsreading = 0
    then Finish (dest, 0)
    else if needsreading <= maxreadsize
    then (
      Bytes.blit_string blob offset dest dest_offset needsreading;
      Finish (dest, needsreading))
    else (
      Bytes.blit_string blob offset dest dest_offset maxreadsize;
      K (maxreadsize, chunkreader maxreadsize))
  in
  readchunked source (chunkreader 0)
;;

(* TODO: have a large-string variant which returns [(string*int*int)list] to
   avoid allocations. *)
(* TODO: have a large-string variant which takes [(string*int*int)->unit] to let
   the user control the copying/use. *)

let read_large_string source len =
  let dest = Bytes.make len '\000' in
  let rec chunkreader dest_offset blob offset maxreadsize =
    let needsreading = len - dest_offset in
    if needsreading = 0
    then Finish (Bytes.unsafe_to_string dest, 0)
    else if needsreading <= maxreadsize
    then (
      Bytes.blit_string blob offset dest dest_offset needsreading;
      Finish (Bytes.unsafe_to_string dest, needsreading))
    else (
      Bytes.blit_string blob offset dest dest_offset maxreadsize;
      K (maxreadsize, chunkreader maxreadsize))
  in
  readchunked source (chunkreader 0)
;;

let%expect_test _ =
  let pp_source_state fmt source =
    Format.fprintf
      fmt
      "blob: %S, offset: %d, length: %d, readed: %d, stops: [%a], maxlen: %d"
      source.blob
      source.offset
      source.length
      source.readed
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ',')
         Format.pp_print_int)
      source.stop_hints
      source.maximum_length
  in
  let w source ls =
    Format.printf "Source: %a\n" pp_source_state source;
    match
      List.fold_left
        (fun source l ->
          match read_large_string source l with
          | Suspended _ ->
            Format.printf "Suspended!\n";
            raise Exit
          | Failed { error; source } ->
            Format.printf "Error: %S\nSource: %a\n" error pp_source_state source;
            source
          | Readed { value; source } ->
            Format.printf "Ok: %S\nSource: %a\n" value pp_source_state source;
            source)
        source
        ls
    with
    | exception Exit -> ()
    | _ -> ()
  in
  let source = mk_source "foobarbaz" 0 9 in
  w source [ 3; 0; 6; 1 ];
  [%expect
    {|
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 0, stops: [], maxlen: 4611686018427387903
    Ok: "foo"
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 3, stops: [], maxlen: 4611686018427387903
    Ok: ""
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 3, stops: [], maxlen: 4611686018427387903
    Ok: "barbaz"
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 9, stops: [], maxlen: 4611686018427387903
    Suspended! |}];
  let source = mk_source "foobarbaz" ~maximum_length:6 0 9 in
  w source [ 3; 0; 6; 3; 1 ];
  [%expect
    {|
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 0, stops: [], maxlen: 6
    Ok: "foo"
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 3, stops: [], maxlen: 6
    Ok: ""
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 3, stops: [], maxlen: 6
    Error: "chunkreader requires more bytes but hard limit was reached"
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 3, stops: [], maxlen: 6
    Ok: "bar"
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 6, stops: [], maxlen: 6
    Error: "chunkreader requires more bytes but hard limit was reached"
    Source: blob: "foobarbaz", offset: 0, length: 9, readed: 6, stops: [], maxlen: 6 |}];
  ();
  let w ss ls =
    let rec go ss cont =
      match cont () with
      | Suspended { source; cont } ->
        Format.printf "Suspended!\nSource: %a\n" pp_source_state source;
        let s = List.hd ss in
        let ss = List.tl ss in
        go ss (fun () -> cont s 0 (String.length s))
      | Failed { error; source } as failed ->
        Format.printf "Error: %S\nSource: %a\n" error pp_source_state source;
        failed, source, ss
      | Readed { value; source } as readed ->
        Format.printf "Ok: %S\nSource: %a\n" value pp_source_state source;
        readed, source, ss
    in
    let rec reads source ss ls =
      match ls with
      | [] -> ()
      | l :: ls ->
        (match go ss (fun () -> read_large_string source l) with
        | Suspended _, _, _ -> assert false
        | (Failed _ | Readed _), source, ss -> reads source ss ls)
    in
    let s = List.hd ss in
    let ss = List.tl ss in
    let source = mk_source s 0 (String.length s) in
    Format.printf "Start!\nSource: %a\n" pp_source_state source;
    reads source ss ls
  in
  w [ "foo"; "bar"; "baz" ] [ 3; 0; 6 ];
  [%expect
    {|
    Start!
    Source: blob: "foo", offset: 0, length: 3, readed: 0, stops: [], maxlen: 4611686018427387903
    Ok: "foo"
    Source: blob: "foo", offset: 0, length: 3, readed: 3, stops: [], maxlen: 4611686018427387903
    Ok: ""
    Source: blob: "foo", offset: 0, length: 3, readed: 3, stops: [], maxlen: 4611686018427387903
    Suspended!
    Source: blob: "foo", offset: 0, length: 3, readed: 3, stops: [], maxlen: 4611686018427387903
    Suspended!
    Source: blob: "bar", offset: 0, length: 3, readed: 3, stops: [], maxlen: 4611686018427387900
    Ok: "barbaz"
    Source: blob: "baz", offset: 0, length: 3, readed: 3, stops: [], maxlen: 4611686018427387897 |}];
  w
    (let rec xs = "xyz" :: xs in
     xs)
    [ 4; 4; 4 ];
  [%expect
    {|
    Start!
    Source: blob: "xyz", offset: 0, length: 3, readed: 0, stops: [], maxlen: 4611686018427387903
    Suspended!
    Source: blob: "xyz", offset: 0, length: 3, readed: 3, stops: [], maxlen: 4611686018427387903
    Ok: "xyzx"
    Source: blob: "xyz", offset: 0, length: 3, readed: 1, stops: [], maxlen: 4611686018427387900
    Suspended!
    Source: blob: "xyz", offset: 0, length: 3, readed: 3, stops: [], maxlen: 4611686018427387900
    Ok: "yzxy"
    Source: blob: "xyz", offset: 0, length: 3, readed: 2, stops: [], maxlen: 4611686018427387897
    Suspended!
    Source: blob: "xyz", offset: 0, length: 3, readed: 3, stops: [], maxlen: 4611686018427387897
    Ok: "zxyz"
    Source: blob: "xyz", offset: 0, length: 3, readed: 3, stops: [], maxlen: 4611686018427387894 |}];
  ()
;;

let rec ( let* ) x f =
  match x with
  | Readed { source; value } -> f (value, source)
  | Failed { source; error } -> Failed { source; error }
  | Suspended { source; cont } ->
    let cont blob offset length =
      let* x = cont blob offset length in
      f x
    in
    Suspended { source; cont }
;;

let read_utf8_uchar source =
  let* c, source = read_char source in
  let c = Char.code c in
  if c land 0b1000_0000 = 0b0000_0000
  then (
    (* length 1 *)
    let value = Uchar.of_int c in
    Readed { source; value })
  else if c land 0b1110_0000 = 0b1100_0000
  then
    (* length 2 *)
    let* c1, source = read_char source in
    let c1 = Char.code c1 in
    if c1 land 0b1100_0000 = 0b1000_0000
    then (
      let c = (c land 0b0001_1111) lsl 6 in
      let c1 = c1 land 0b0011_1111 in
      let point = c lor c1 in
      let value = Uchar.of_int point in
      Readed { source; value })
    else (
      let error = "Invalid UTF8 byte" in
      Failed { source; error })
  else if c land 0b1111_0000 = 0b1110_0000
  then
    (* length 3 *)
    let* c1, source = read_char source in
    let c1 = Char.code c1 in
    let* c2, source = read_char source in
    let c2 = Char.code c2 in
    if c1 land 0b1100_0000 = 0b1000_0000 && c2 land 0b1100_0000 = 0b1000_0000
    then (
      let c = (c land 0b0000_1111) lsl 12 in
      let c1 = (c1 land 0b0011_1111) lsl 6 in
      let c2 = c2 land 0b0011_1111 in
      let point = c lor c1 lor c2 in
      let value = Uchar.of_int point in
      Readed { source; value })
    else (
      let error = "Invalid UTF8 byte" in
      Failed { source; error })
  else if c land 0b1111_1000 = 0b1111_0000
  then
    (* length 4 *)
    let* c1, source = read_char source in
    let c1 = Char.code c1 in
    let* c2, source = read_char source in
    let c2 = Char.code c2 in
    let* c3, source = read_char source in
    let c3 = Char.code c3 in
    if c1 land 0b1100_0000 = 0b1000_0000
       && c2 land 0b1100_0000 = 0b1000_0000
       && c3 land 0b1100_0000 = 0b1000_0000
    then (
      let c = (c land 0b0000_0111) lsl 18 in
      let c1 = (c1 land 0b0011_1111) lsl 12 in
      let c2 = (c2 land 0b0011_1111) lsl 6 in
      let c3 = c3 land 0b0011_1111 in
      let point = c lor c1 lor c2 lor c3 in
      let value = Uchar.of_int point in
      Readed { source; value })
    else (
      let error = "Invalid UTF8 byte" in
      Failed { source; error })
  else (
    let error = "Invalid UTF8 leading byte" in
    Failed { source; error })
;;

let%expect_test _ =
  let w s =
    let rec loop acc source =
      match read_utf8_uchar source with
      | Readed { value; source } -> loop (value :: acc) source
      | Failed { error; source = _ } -> Error error
      | Suspended { source = _; cont = _ } -> Ok (List.rev acc)
    in
    match loop [] (mk_source s 0 (String.length s)) with
    | Ok uchars ->
      Format.printf
        "%a\n"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_char fmt '.')
           (fun fmt u -> Format.fprintf fmt "x%x" (Uchar.to_int u)))
        uchars
    | Error error -> Format.printf "Error %s\n" error
  in
  w "";
  [%expect {| |}];
  w "abc";
  [%expect {| x61.x62.x63 |}];
  w "\xff";
  [%expect {| Error Invalid UTF8 leading byte |}];
  w "$";
  [%expect {| x24 |}];
  w "£";
  [%expect {| xa3 |}];
  w "€";
  [%expect {| x20ac |}];
  w "𐍈";
  [%expect {| x10348 |}];
  ()
;;

let of_string read s =
  let source = mk_source s 0 (String.length s) in
  match read source with
  | Readed { source; value } ->
    assert (source.readed <= source.length);
    if source.readed < source.length then Error "Too many bytes" else Ok value
  | Failed { source = _; error } -> Error error
  | Suspended { source = _; cont = _ } -> Error "Not enough bytes"
;;

let rec of_string_seq_loop seq cont =
  match seq () with
  | Seq.Nil -> Error "Not enough chunks or bytes"
  | Seq.Cons (s, seq) ->
    (match cont s 0 (String.length s) with
    | Readed { source; value } ->
      assert (source.readed <= source.length);
      if source.readed < source.length
      then Error "Too many bytes"
      else if Seq.is_empty seq
      then Ok value
      else Error "Too many chunks"
    | Failed { source = _; error } -> Error error
    | Suspended { source = _; cont } -> of_string_seq_loop seq cont)
;;

let of_string_seq read s =
  of_string_seq_loop s (fun s off len ->
      let source = mk_source s off len in
      read source)
;;
