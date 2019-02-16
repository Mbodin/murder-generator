(** Module main.
 * This file is the one compiled to JavaScript, then fetched and executed
 * to run the whole program. **)

(** The translations needed to print error messages. **)
let errorTranslations =
  ref ("An error occurred!", "Please report it", "there", "Error details:")

(** Whether the loading animation is running. **)
let loading = ref true

(** Stops the loading animation. **)
let stopLoading _ =
  assert%lwt !loading ;%lwt
  ignore (Js.Unsafe.fun_call (Js.Unsafe.js_expr "stopLoading") [||]) ;
  loading := false ;
  Lwt.return ()

(** Starts the loading animation. **)
let startLoading _ =
  assert%lwt !loading ;%lwt
  ignore (Js.Unsafe.fun_call (Js.Unsafe.js_expr "startLoading") [||]) ;
  loading := true ;
  Lwt.return ()

(** Given a map from key to their translation, finds the corresponding key. **)
let get_translation m key =
  let iso = PMap.find "iso639" m in
  try PMap.find key m
  with Not_found ->
    failwith ("No key “" ^ key ^ "” found for translation “" ^ iso ^ "”.")

(** The main script. **)
let _ =
  try%lwt
    InOut.clear_response () ;
    let%lwt translations =
      (** Getting the translations file. **)
      let translations_file = "web/translations.json" in
      let%lwt translations = InOut.get_file translations_file in
      match Yojson.Safe.from_string ~fname:translations_file translations with
      | `List l ->
        Lwt.return (
          List.mapi (fun i ->
            let current =
              "The " ^ string_of_int (i + 1) ^ "th element"
              ^ " of the file “" ^ translations_file ^ "”" in function
            | `Assoc l ->
              let m =
                PMap.of_enum (ExtList.List.enum (List.map (function
                  | key, `String str -> (key, str)
                  | (key, _) ->
                    failwith (current ^ " associates the field “" ^ key
                              ^ "” to something else than a string.")) l)) in
              if not (PMap.mem "iso639" m) then
                failwith (current ^ " has no key “iso639”.") ;
              m
            | _ ->
              failwith (current ^ " is not an object.")) l)
      | _ ->
        Lwt.fail (Invalid_argument
          ("The file “" ^ translations_file ^ "” is not a list.")) in
    (** Shuffling languages, but putting the user languages on top. **)
    let translations =
      let userLangs =
        let navigator = Dom_html.window##.navigator in
        let to_list o =
          match Js.Optdef.to_option o with
          | None -> []
          | Some a -> [Js.to_string a] in
        to_list navigator##.language @ to_list navigator##.userLanguage in
      let (matching, nonmatching) =
        List.partition (fun m -> List.mem (PMap.find "iso639" m) userLangs)
          translations in
      Utils.shuffle matching @ Utils.shuffle nonmatching in
    (** Showing to the user all available languages. **)
    let%lwt translations =
      let (res, w) = Lwt.task () in
      InOut.print_block (InOut.Div (List.map (fun m ->
        let get = get_translation m in
        InOut.CenterP [InOut.LinkContinuation (get "name", fun _ ->
          InOut.clear_response () ;
          errorTranslations :=
            (get "errorOccurred", get "report", get "there", get "errorDetails") ;
          Lwt.wakeup_later w m)]) translations)) ;
      stopLoading () ;%lwt
      res in
    let get_translation = get_translation translations in
    (* TODO *)
    InOut.print_block (InOut.P [InOut.Text (get_translation "underConstruction")]) ;
    Lwt.return ()
  with e ->
    let (errorOccurred, reportIt, there, errorDetails) = !errorTranslations in
    InOut.print_block (InOut.Div [
        InOut.P [
            InOut.Text errorOccurred ; InOut.Text reportIt ;
            InOut.Link (there, "https://github.com/Mbodin/murder-generator/issues")
          ] ;
        InOut.P [
            InOut.Text errorDetails ;
            InOut.Text (Printexc.to_string e)
          ]
      ]) ;
    if%lwt Lwt.return !loading then stopLoading () ;%lwt
    Lwt.return ()

