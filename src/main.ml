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

(** Getting and parsing the translations file. **)
let get_translations _ =
  let%lwt (translation, languages) =
    let translations_file = "web/translations.json" in
    let%lwt translations = InOut.get_file translations_file in
    Lwt.return (Translation.from_json translations_file translations) in
  (** Shuffling languages, but putting the user languages on top. **)
  let userLangs =
    let navigator = Dom_html.window##.navigator in
    let to_list o =
      match Js.Optdef.to_option o with
      | None -> []
      | Some a -> [Js.to_string a] in
    to_list navigator##.language @ to_list navigator##.userLanguage in
  let (matching, nonmatching) =
    List.partition (fun lg ->
      List.mem (Translation.iso639 lg) userLangs) languages in
  Lwt.return (translation, Utils.shuffle matching @ Utils.shuffle nonmatching)

(** Get and parse each data file. **)
let get_data _ =
  let intermediate = ref Driver.empty_intermediary in
  Lwt_list.iter_p (fun fileName ->
      let%lwt file = InOut.get_file fileName in
      let lexbuf = Lexing.from_string file in
      let file = Driver.parse_lexbuf fileName lexbuf in
      intermediate := Driver.prepare_declarations !intermediate file ;
      Lwt.return ())
    MurderFiles.files ;%lwt
  if not (Driver.is_intermediary_final !intermediate) then (
    let categories = Driver.categories_to_be_defined !intermediate in
    let (attributes, contacts) = Driver.attributes_to_be_defined !intermediate in
    let missing str s =
      " Missing " ^ str ^ ": " ^ String.concat ", " (Utils.PSet.to_list s) ^ "." in
    Lwt.fail (Invalid_argument
      ("Non final intermediary after parsing all files."
       ^ missing "categories" categories
       ^ missing "attributes" attributes
       ^ missing "contacts" contacts)))
  else Lwt.return (Driver.parse !intermediate)

(** The main script. **)
let _ =
  try%lwt
    InOut.clear_response () ;
    let%lwt (translation, languages) = get_translations () in
    let get_translation lg key =
      match Translation.translate translation key lg with
      | Some str -> str
      | None ->
        failwith ("No key “" ^ key ^ "” found for language “"
                  ^ (Translation.iso639 lg) ^ "”.") in
    (** We request the data without forcing it yet. **)
    let data = get_data () in
    (** Showing to the user all available languages. **)
    let%lwt language =
      let (res, w) = Lwt.task () in
      InOut.print_block (InOut.Div (List.map (fun lg ->
        let get = get_translation lg in
        InOut.CenterP [ InOut.LinkContinuation (get "name", fun _ ->
          InOut.clear_response () ;
          errorTranslations :=
            (get "error", get "report", get "there", get "errorDetails") ;
          Lwt.wakeup_later w lg) ]) languages)) ;
      stopLoading () ;%lwt
      res in
    let get_translation = get_translation language in
    (** Describing the project to the user. **)
    InOut.print_block (InOut.P [
      InOut.Text (get_translation "description") ;
      InOut.Text (get_translation "openSource") ;
      InOut.Link (get_translation "there",
                  "https://github.com/Mbodin/murder-generator")
        ]) ;
    (** Asking the first basic questions about the murder party. **)
    let%lwt (playerNumber, complexityDifficulty) =
      let playerNumber =
        Dom_html.createInput ~_type:(Js.string "number") InOut.document in
      ignore (playerNumber##setAttribute (Js.string "min") (Js.string "0")) ;
      ignore (playerNumber##setAttribute (Js.string "max")
                                         (Js.string (string_of_int max_int))) ;
      playerNumber##.value := Js.string "13" ;
      InOut.print_block (InOut.P [
          InOut.Text (get_translation "howManyPlayers") ;
          InOut.Node (playerNumber :> Dom_html.element Js.t)
        ]) ;
      let generalLevel =
        Dom_html.createInput ~_type:(Js.string "range") InOut.document in
      ignore (generalLevel##setAttribute (Js.string "min") (Js.string "0")) ;
      ignore (generalLevel##setAttribute (Js.string "max") (Js.string "100")) ;
      generalLevel##.value := Js.string "50" ;
      InOut.print_block (InOut.Div [
          InOut.P [ InOut.Text (get_translation "experience") ] ;
          InOut.CenterP [
            InOut.Text (get_translation "beginner") ;
            InOut.Node (generalLevel :> Dom_html.element Js.t) ;
            InOut.Text (get_translation "experienced")
          ]
        ]) ;
      let (res, w) = Lwt.task () in
      InOut.print_block (InOut.CenterP [
        InOut.LinkContinuation (get_translation "next", fun _ ->
          InOut.clear_response () ;
          let playerNumber =
            max 0 (int_of_string (Js.to_string playerNumber##.value)) in
          let generalLevel =
            max 0. (min 100.
              (float_of_string (Js.to_string generalLevel##.value))) in
          let generalLevel = generalLevel /. 100. in
          let generalLevel = generalLevel *. generalLevel in
          let complexityDifficulty =
            5 + int_of_float (generalLevel *. 45.) in
          Lwt.wakeup_later w (playerNumber, complexityDifficulty)) ]) ;
      res in
    (** Asking about categories. **)
    let%lwt data = data in
    let translate_categories = Driver.translates_category data in
    (* TODO *)
    InOut.print_block (InOut.P [
      InOut.Text ("This is just a test: " ^ string_of_int playerNumber
                  ^ ", " ^ string_of_int complexityDifficulty)]) ;
    InOut.print_block (InOut.P [
      InOut.Text ("Another test: " ^
                  String.concat ", " (List.map (fun c ->
                    match Translation.translate translate_categories c language with
                    | None -> "<none>"
                    | Some t -> t) (Driver.all_categories data)))]) ;
    InOut.print_block (InOut.P [
      InOut.Text (get_translation "underConstruction") ;
      InOut.Text (get_translation "participate") ;
      InOut.Link (get_translation "there",
                  "https://github.com/Mbodin/murder-generator")
        ]) ;
    Lwt.return ()
  (** Reporting errors. **)
  with e ->
    let issues = "https://github.com/Mbodin/murder-generator/issues" in
    try%lwt
      let (errorOccurred, reportIt, there, errorDetails) = !errorTranslations in
      InOut.print_block (InOut.Div [
          InOut.P [
              InOut.Text errorOccurred ; InOut.Text reportIt ;
              InOut.Link (there, issues)
            ] ;
          InOut.P [
              InOut.Text errorDetails ;
              InOut.Text (Printexc.to_string e)
            ]
        ]) ;
      if%lwt Lwt.return !loading then stopLoading () ;%lwt
      Lwt.return ()
    with e' -> (** If there have been an error when printing the error,
                * we failback to the console. **)
      Firebug.console##log "Unfortunately, a important error happened." ;
      Firebug.console##log ("Please report it to " ^ issues) ;
      Firebug.console##log ("Primary error details: " ^ Printexc.to_string e) ;
      Firebug.console##log ("Secondary error details: " ^ Printexc.to_string e') ;
      Lwt.return ()

