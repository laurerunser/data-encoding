(* TODO: expect tests *)

type source =
  { blob : string
  ; offset : int
  ; length : int
  ; readed : int (* [read] is ambiguous so we make it unambiguously past as [readed] *)
  ; stop_at_readed : int list
        (* this list is grown when there is a size-header in the encoded binary data *)
  ; maximum_length : int
  }

let mk_source ?(maximum_length = max_int) ?(stop_at_readed = []) blob offset length =
  if offset < 0 then failwith "Suspendable_buffers.Reading.mk_source: negative offset";
  if length < 0 then failwith "Suspendable_buffers.Reading.mk_source: negative length";
  if offset + length > String.length blob
  then failwith "Suspendable_buffers.Reading.mk_source: offset+length overflow";
  { blob; offset; length; readed = 0; stop_at_readed; maximum_length }
;;

let bump_readed source reading = { source with readed = source.readed + reading }

let set_maximum_length source maximum_length =
  if maximum_length < 0
  then
    raise
      (Invalid_argument "Suspendable_buffers.Reading.set_maximum_length: negative length");
  if maximum_length > source.maximum_length
  then
    raise
      (Invalid_argument
         "Suspendable_buffers.Reading.set_maximum_length: cannot increase maximum length");
  { source with maximum_length }
;;

let push_stop source length =
  assert (length >= 0);
  if source.readed + length > source.maximum_length
  then Error "expected-stop exceeds maximum-length"
  else (
    let requested_stop = source.readed + length in
    match source.stop_at_readed with
    | [] -> Ok { source with stop_at_readed = [ requested_stop ] }
    | previously_requested_stop :: _ ->
      if requested_stop > previously_requested_stop
      then Error "expected-stop exceeds previously requested stop"
      else Ok { source with stop_at_readed = requested_stop :: source.stop_at_readed })
;;

let pop_stop source =
  assert (source.stop_at_readed <> []);
  match source.stop_at_readed with
  | [] -> assert false
  | stop :: stop_at_readed -> stop, { source with stop_at_readed }
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
      ; cont : string -> int -> int -> 'a readed
      }

let source_too_small_to_continue_message = "new source blob is too small to continue"

let readf source reading read =
  assert (reading >= 0);
  if source.readed + reading > source.maximum_length
  then Failed { source; error = "maximum-length exceeded" }
  else if match source.stop_at_readed with
          | [] -> false
          | stop :: _ -> source.readed + reading > stop
  then Failed { source; error = "expected-stop point exceeded" }
  else if source.readed + reading > source.length
  then
    Suspended
      { source
      ; cont =
          (fun blob offset length ->
            assert (source.readed <= source.length);
            if source.readed = source.length
            then (
              let source =
                mk_source
                  ~maximum_length:source.maximum_length
                  ~stop_at_readed:source.stop_at_readed
                  blob
                  offset
                  length
              in
              if reading > source.length
                 (* TODO: instead of failing here (and below), allow to continue
                 after more feeding, possibly by concatenating bigger and bigger
                 blobs until the value is readable *)
              then Failed { source; error = source_too_small_to_continue_message }
              else (
                let value = read source.blob source.offset in
                let source = bump_readed source reading in
                Readed { source; value }))
            else (
              assert (source.readed < source.length);
              (* First check that the current here small readf has enough data *)
              let available_length = source.length - source.readed + length in
              if reading > available_length
              then Failed { source; error = source_too_small_to_continue_message }
              else (
                (* prepare for this small here readf *)
                let source =
                  let blob =
                    String.sub
                      source.blob
                      (source.offset + source.readed)
                      (source.length - source.readed)
                    ^ String.sub blob offset (reading - (source.length - source.readed))
                  in
                  let offset = 0 in
                  let length = reading in
                  mk_source
                    ~maximum_length:source.maximum_length
                    ~stop_at_readed:source.stop_at_readed
                    blob
                    offset
                    length
                in
                (* actually do this small here read *)
                let value = read source.blob source.offset in
                let source = bump_readed source reading in
                assert (source.readed = source.length);
                (* Second prepare the source for giving back *)
                let source =
                  mk_source
                    ~maximum_length:source.maximum_length
                    ~stop_at_readed:source.stop_at_readed
                    blob
                    offset
                    length
                in
                (* delta is the part of the new blob that has already been
                         read by the actual small read above *)
                let delta = reading - (source.length - source.readed) in
                assert (source.readed = 0);
                let source = { source with readed = delta } in
                Readed { source; value })))
      }
  else (
    let value = read source.blob (source.offset + source.readed) in
    let source = bump_readed source reading in
    Readed { source; value })
;;

type 'a chunkreader = string -> int -> int -> 'a chunkreaded

and 'a chunkreaded =
  | K of int * 'a chunkreader
  | Finish of 'a * int

let rec readchunked source read =
  let reading =
    min (source.maximum_length - source.readed) (source.length - source.readed)
  in
  assert (reading >= 0);
  match read source.blob (source.offset + source.readed) reading with
  | Finish (value, readed) ->
    let source = bump_readed source readed in
    Readed { source; value }
  | K (readed, read) ->
    let source = bump_readed source readed in
    Suspended
      { source
      ; cont =
          (fun blob offset length ->
            let source =
              mk_source
                ~maximum_length:(source.maximum_length - source.readed)
                blob
                offset
                length
            in
            readchunked source read)
      }
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
