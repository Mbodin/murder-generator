(** Module main.
 * This file is the one compiled to JavaScript, then fetched and executed
 * to run the whole program. **)

open Js_of_ocaml


(** The default error messages. **)
let errorTranslationsDefault =
  ("An error occurred!", "Please report it", "there", "Error details:")

(** The translations needed to print error messages. **)
let errorTranslations = ref errorTranslationsDefault

(** Whether the loading animation is running. **)
let loading = ref true

(** Stops the loading animation. **)
let stopLoading _ =
  if !loading then (
    ignore (Js.Unsafe.fun_call (Js.Unsafe.js_expr "stopLoading") [||]) ;
    loading := false) ;
  Lwt.return ()

(** Starts the loading animation. **)
let startLoading _ =
  if not !loading then (
    ignore (Js.Unsafe.fun_call (Js.Unsafe.js_expr "startLoading") [||]) ;
    loading := true) ;
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

(** Prints a list of strings, [andw] being the word for “and”
 * in the current language.
 * This function makes use of the Oxford comma. **)
let print_list andw = function
  | [] -> ""
  | a :: [] -> a
  | a :: b :: [] -> a ^ " " ^ andw ^ " " ^ b
  | l ->
    match Utils.list_match_right l with
    | None -> assert false
    | Some (l, r) ->
      String.concat ", " l ^ ", " ^ andw ^ " " ^ r

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
      " Missing " ^ str ^ ": " ^ print_list "and" (Utils.PSet.to_list s) ^ "." in
    Lwt.fail (Invalid_argument
      ("Non final intermediary after parsing all files."
       ^ missing "categories" categories
       ^ missing "attributes" attributes
       ^ missing "contacts" contacts)))
  else Lwt.return (Driver.parse !intermediate)

(** A type to store what each page of the menu provides. **)
type parameters = {
    language : Translation.language option ;
    player_number : int ;
    general_level : float ;
    general_complexity : float ;
    categories : Utils.Id.t Utils.PSet.t option ;
    player_information : (string (** Character name *)
                          * int (** Complexity **)
                          * int (** Difficulty **)
                          * unit (** Miscellaneous **)) list
  }

(** The main script. **)
let _ =
  try%lwt
    InOut.clear_response () ;
    let%lwt (translation, languages) = get_translations () in
    let get_translation_language lg key =
      match Translation.translate translation key lg with
      | Some str -> str
      | None ->
        failwith ("No key “" ^ key ^ "” found for language “"
                  ^ (Translation.iso639 lg) ^ "”.") in
    let get_language p =
      match p.language with
      | Some lg -> lg
      | None -> assert false in
    let get_translation p = get_translation_language (get_language p) in
    (** We request the data without forcing it yet. **)
    let data = get_data () in
    let rec ask_for_languages parameters =
      errorTranslations := errorTranslationsDefault ;
      (** Showing to the user all available languages. **)
      let%lwt language =
        let (res, w) = Lwt.task () in
        InOut.print_block (InOut.Div (InOut.Normal, List.map (fun lg ->
          let get_translation = get_translation_language lg in
          InOut.Div (InOut.Centered, [
            InOut.P [ InOut.LinkContinuation (true, get_translation "name",
              fun _ ->
                InOut.clear_response () ;
                errorTranslations :=
                  (get_translation "error", get_translation "report",
                   get_translation "there", get_translation "errorDetails") ;
                Lwt.wakeup_later w lg) ]])) languages)) ;
        stopLoading () ;%lwt
        res in
      ask_for_basic { parameters with language = Some language }
    and ask_for_basic parameters =
      let get_translation = get_translation parameters in
      (** Describing the project to the user. **)
      InOut.print_block (InOut.P [
          InOut.Text (get_translation "description") ;
          InOut.Text (get_translation "openSource") ;
          InOut.Link (get_translation "there",
                      "https://github.com/Mbodin/murder-generator")
        ]) ;
      (** Asking the first basic questions about the murder party. **)
      let (playerNumber, readPlayerNumber) =
        InOut.createNumberInput parameters.player_number in
      InOut.print_block (InOut.P [
          InOut.Text (get_translation "howManyPlayers") ;
          InOut.Node playerNumber
        ]) ;
      let (generalLevel, readGeneralLevel) =
        InOut.createPercentageInput parameters.general_level in
      InOut.print_block (InOut.Div (InOut.Normal, [
          InOut.P [ InOut.Text (get_translation "experience") ] ;
          InOut.Div (InOut.Centered, [
              InOut.Text (get_translation "beginner") ;
              InOut.Node generalLevel ;
              InOut.Text (get_translation "experienced")
            ])
        ])) ;
      let (generalComplexity, readGeneralComplexity) =
        InOut.createPercentageInput parameters.general_complexity in
      InOut.print_block (InOut.Div (InOut.Normal, [
          InOut.P [ InOut.Text (get_translation "lengthOfCharacterSheets") ] ;
          InOut.Div (InOut.Centered, [
              InOut.Text (get_translation "shortSheets") ;
              InOut.Node generalComplexity ;
              InOut.Text (get_translation "longSheets")
            ])
        ])) ;
      let%lwt cont =
        let (cont, w) = Lwt.task () in
        InOut.print_block (InOut.Div (InOut.Centered, [
          InOut.LinkContinuation (false, get_translation "previous", fun _ ->
            InOut.clear_response () ;
            Lwt.wakeup_later w (fun _ -> ask_for_languages parameters)) ;
          InOut.Space ;
          InOut.LinkContinuation (true, get_translation "next", fun _ ->
            InOut.clear_response () ;
            Lwt.wakeup_later w (fun _ ->
              ask_for_categories { parameters with
                player_number = readPlayerNumber () ;
                general_level = readGeneralLevel () ;
                general_complexity = readGeneralComplexity () }))])) ;
        cont in
      cont ()
    and ask_for_categories parameters =
      let get_translation = get_translation parameters in
      (** Forcing the data to be loaded. **)
      (if Lwt.state data = Lwt.Sleep then
        startLoading ()
      else Lwt.return ()) ;%lwt
      let%lwt data = data in
      stopLoading () ;%lwt
      (** Asking about categories. **)
      let translate_categories =
        let translate_categories = Driver.translates_category data in fun c ->
        match Translation.translate translate_categories c
                (get_language parameters) with
        | None ->
          failwith ("No translation found a category in language “"
                    ^ (Translation.iso639 (get_language parameters)) ^ "”.")
        | Some t -> t in
      let all_categories = Driver.all_categories data in
      let selected_categories =
        match parameters.categories with
        | None -> Utils.PSet.from_list all_categories
        | Some s -> s in
      let onCategoryClick = ref (fun _ -> ()) in
      let categoriesButtons =
        List.fold_left (fun m c ->
          let (e, set, get) =
            InOut.createSwitch (Utils.PSet.mem c selected_categories)
              (fun _ -> !onCategoryClick c) in
          PMap.add c (e, set, get, Utils.PSet.empty) m) PMap.empty all_categories in
      let categoriesButtons =
        List.fold_left (fun m c ->
            let deps = Driver.get_category_dependencies data c in
            Utils.PSet.fold (fun cd m ->
              let (e, set, get, ideps) = PMap.find cd m in
              PMap.add cd (e, set, get, Utils.PSet.add c ideps) m) m deps)
          categoriesButtons all_categories in
      onCategoryClick := (fun c ->
        let (_, _, get, ideps) = PMap.find c categoriesButtons in
        if get () then
          let deps = Driver.get_category_dependencies data c in
          Utils.PSet.iter (fun c ->
            let (_, set, _, _) = PMap.find c categoriesButtons in
            set true) deps
        else
          Utils.PSet.iter (fun c ->
            let (_, set, _, _) = PMap.find c categoriesButtons in
            set false) ideps) ;
      InOut.print_block (InOut.Div (InOut.Normal, [
          InOut.P [ InOut.Text (get_translation "unselectCategories") ] ;
          InOut.Div (InOut.Centered, [
              InOut.LinkContinuation (false, get_translation "noCategories",
                fun _ ->
                  PMap.iter (fun _ (_, set, _, _) -> set false) categoriesButtons) ;
              InOut.Space ;
              InOut.LinkContinuation (true, get_translation "allCategories",
                fun _ ->
                  PMap.iter (fun _ (_, set, _, _) -> set true) categoriesButtons) ;
            ]) ;
          InOut.List (false,
            PMap.foldi (fun c (e, _, _, _) l ->
              InOut.P ([
                  InOut.Node e ;
                  InOut.Text (translate_categories c)
                ] @ (
                  let deps =
                    List.map translate_categories (Utils.PSet.to_list
                      (Driver.get_category_dependencies data c)) in
                  if deps = [] then []
                  else
                    [ InOut.Text ("(" ^ get_translation "categoryDepends" ^ " "
                                  ^ print_list (get_translation "and") deps ^ ")") ]
                )) :: l) categoriesButtons [])
        ])) ;
      let%lwt cont =
        let (cont, w) = Lwt.task () in
        InOut.print_block (InOut.Div (InOut.Centered, [
          InOut.LinkContinuation (false, get_translation "previous", fun _ ->
            InOut.clear_response () ;
            Lwt.wakeup_later w (fun _ ->
              ask_for_basic parameters)) ;
          InOut.Space ;
          InOut.LinkContinuation (true, get_translation "next", fun _ ->
            InOut.clear_response () ;
            let selected_categories =
              PMap.foldi (fun c (_, _, get, _) s ->
                if get () then
                  Utils.PSet.add c s
                else s) categoriesButtons Utils.PSet.empty in
            Lwt.wakeup_later w (fun _ ->
              ask_for_player_constraints { parameters with
                categories = Some selected_categories })) ])) ;
        cont in
      cont ()
    and ask_for_player_constraints parameters =
      let get_translation = get_translation parameters in
      let%lwt data = data in
      let player_number = parameters.player_number in
      let categories =
        match parameters.categories with
        | Some s -> s
        | None -> assert false in
      let elements =
        Driver.get_all_elements data categories player_number in
      let (complexity, difficulty) =
        let generalLevel = parameters.general_level in
        let generalComplexity = parameters.general_complexity in
        let generalLevel = generalLevel *. generalLevel in
        let complexityDifficulty =
          10. +. generalLevel *. 90. in
        let result p = int_of_float (0.5 +. complexityDifficulty *. p) in
        (result generalComplexity, result (1. -. generalComplexity)) in
      (** Asking about individual player constraints. **)
      InOut.print_block (InOut.Div (InOut.Normal, [
        InOut.P [ InOut.Text (get_translation "individualConstraints") ] ;
        InOut.P [ InOut.Text (get_translation "complexityDifficultyExplanation") ] ;
        InOut.List (true, [
            InOut.Text (get_translation "lowComplexityLowDifficulty") ;
            InOut.Text (get_translation "lowComplexityHighDifficulty") ;
            InOut.Text (get_translation "highComplexityLowDifficulty") ;
            InOut.Text (get_translation "highComplexityHighDifficulty") ])])) ;
      let startV = get_translation "nameStartVowels" in
      let startC = get_translation "nameStartConsonant" in
      let middleV = get_translation "nameMiddleVowels" in
      let middleC = get_translation "nameMiddleConsonant" in
      let endV = get_translation "nameEndVowels" in
      let endC = get_translation "nameEndConsonant" in
      let size =
        let raw = get_translation "nameSize" in
        try int_of_string raw
        with Failure _ ->
          failwith ("The key “nameSize” for language “" ^
                    (Translation.iso639 (get_language parameters)) ^ "” is “" ^ raw
                    ^ "”, which is not a valid number.") in
      let seed =
        Names.createVowelConsonant size startV startC middleV middleC endV endC in
      let player_information =
        let player_information = parameters.player_information in
        let len = List.length player_information in
        if len >= player_number then
          ExtList.List.take player_number player_information
        else
          player_information @
            List.map (fun _ ->
                (Names.generate seed, complexity, difficulty, ()))
              (Utils.seq (player_number - len))
        in
      let table =
        List.map (fun (name, complexity, difficulty, _) ->
          (InOut.createTextInput name,
           InOut.createNumberInput complexity,
           InOut.createNumberInput difficulty,
           InOut.Space)) player_information in
      InOut.print_block (InOut.Div (InOut.Normal, [
        InOut.P [ InOut.Text (get_translation "changeThisTable") ] ;
        InOut.Div (InOut.Centered, [
          InOut.Table ([InOut.Text (get_translation "playerName") ;
                        InOut.Text (get_translation "complexity") ;
                        InOut.Text (get_translation "difficulty") ;
                        InOut.Text (get_translation "miscellaneous") ],
                        List.map (fun ((name, _), (complexity, _),
                                       (difficulty, _), misc) -> [
                            InOut.Node name ;
                            InOut.Node complexity ;
                            InOut.Node difficulty ;
                            misc
                          ]) table) ])])) ;
      InOut.print_block (InOut.P [
        InOut.Text (get_translation "underConstruction") ;
        InOut.Text (get_translation "participate") ;
        InOut.Link (get_translation "there",
                    "https://github.com/Mbodin/murder-generator") ]) ;
      let%lwt cont =
        let (cont, w) = Lwt.task () in
        InOut.print_block (InOut.Div (InOut.Centered, [
          InOut.LinkContinuation (false, get_translation "previous", fun _ ->
            InOut.clear_response () ;
            Lwt.wakeup_later w (fun _ ->
              ask_for_categories parameters)) ;
          InOut.Space ;
          (*InOut.LinkContinuation (true, get_translation "next", fun _ ->
            InOut.clear_response () ;
            Lwt.wakeup_later w (fun _ ->
              TODO))*) ])) ;
        cont in
      cont () in
    let parameters = {
        language = None ;
        player_number = 13 ;
        general_level = 0.5 ;
        general_complexity = 0.5 ;
        categories = None ;
        player_information = []
      } in
    ask_for_languages parameters
  (** Reporting errors. **)
  with e ->
    let issues = "https://github.com/Mbodin/murder-generator/issues" in
    try%lwt
      let (errorOccurred, reportIt, there, errorDetails) = !errorTranslations in
      InOut.print_block (InOut.Div (InOut.Normal, [
          InOut.P [
              InOut.Text errorOccurred ; InOut.Text reportIt ;
              InOut.Link (there, issues)
            ] ;
          InOut.P [
              InOut.Text errorDetails ;
              InOut.Text (Printexc.to_string e)
            ]
        ])) ;
      stopLoading () ;%lwt
      Lwt.return ()
    with e' -> (** If there have been an error when printing the error,
                * we failback to the console. **)
      Firebug.console##log "Unfortunately, a important error happened." ;
      Firebug.console##log ("Please report it to " ^ issues) ;
      Firebug.console##log ("Primary error details: " ^ Printexc.to_string e) ;
      Firebug.console##log ("Secondary error details: " ^ Printexc.to_string e') ;
      Lwt.return ()

