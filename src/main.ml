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
        InOut.CenterP [ InOut.LinkContinuation (get "name", fun _ ->
          InOut.clear_response () ;
          errorTranslations :=
            (get "error", get "report", get "there", get "errorDetails") ;
          Lwt.wakeup_later w m) ]) translations)) ;
      stopLoading () ;%lwt
      res in
    let get_translation = get_translation translations in
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
          InOut.P [ InOut.Text (get_translation "howManyPlayers") ] ;
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
          let playerNumber = int_of_string (Js.to_string playerNumber##.value) in
          if playerNumber < 0 then
            failwith "Please enter a positive number of players." ;
          let generalLevel = float_of_string (Js.to_string generalLevel##.value) in
          if generalLevel < 0. || generalLevel > 100. then
            failwith ("The general level should be a percentage, "
                      ^ "but is out of bounds.") ;
          let generalLevel = generalLevel /. 100. in
          let generalLevel = generalLevel *. generalLevel in
          let complexityDifficulty =
            5 + int_of_float (generalLevel *. 45.) in
          Lwt.wakeup_later w (playerNumber, complexityDifficulty)) ]) ;
      res in
    (* TODO *)
    InOut.print_block (InOut.P [
      InOut.Text ("This is just a test: " ^ string_of_int playerNumber
                  ^ ", " ^ string_of_int complexityDifficulty)]) ;
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

