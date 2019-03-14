(** Module main.
 * This file is the one compiled to JavaScript, then fetched and executed
 * to run the whole program. **)

open Js_of_ocaml
open ExtList
open ExtString


(** The default error messages. **)
let errorTranslationsDefault =
  ("An error occurred!", "Please report it", "there", "Error details:")

(** The translations needed to print error messages. **)
let errorTranslations = ref errorTranslationsDefault

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
      let lg = Translation.iso639 lg in
      List.exists (fun ulg -> String.exists ulg lg) userLangs) languages in
  Lwt.return (translation, Utils.shuffle matching @ Utils.shuffle nonmatching)

(** Prints a list of strings, [andw] being the word for “and”
 * in the current language.
 * This function makes use of the Oxford comma. **)
let print_list andw = function
  | [] -> ""
  | a :: [] -> a
  | a :: b :: [] -> a ^ " " ^ andw ^ " " ^ b
  | l ->
    let (l, r) = Utils.assert_option __LOC__ (Utils.list_match_right l) in
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
    computation_power : float ;
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
      Utils.assert_option ("No key “" ^ key ^ "” found for language “"
                           ^ (Translation.iso639 lg) ^ "”.")
        (Translation.translate translation key lg) in
    let get_language p = Utils.assert_option __LOC__ p.language in
    let get_translation p = get_translation_language (get_language p) in
    (** Adds a “next” and “previous” buttons and call them when needed.
     * This function waits for the user to either click on the previous or
     * next button, then calls the function to get the parameters, and
     * finally calls the appropriate function.
     * Both functions are given as option-types: if [None] is given, the
     * corresponding button doesn’t appear.
     * The text (more precisely, its key) for each button can be changed. **)
    let next_button ?previousText:(previousText="previous")
          ?nextText:(nextText="next") p get_parameters previous next =
      let%lwt cont =
        let (cont, w) = Lwt.task () in
        let jump f _ =
          InOut.clear_response () ;
          Lwt.wakeup_later w (fun _ -> f (get_parameters ())) in
        let createListButton dir text f =
          match f with
          | None -> []
          | Some f ->
            [ InOut.LinkContinuation (dir, get_translation p text, jump f) ] in
        let previous = createListButton false previousText previous in
        let next = createListButton true nextText next in
        InOut.print_block (InOut.Div (InOut.Centered,
          if previous = [] || next = [] then previous @ next
          else previous @ [ InOut.Space ] @ next)) ;
        cont in
      cont () in
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
        InOut.stopLoading () ;%lwt
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
      let (fastOrSlow, readFastOrSlow) =
        InOut.createPercentageInput parameters.computation_power in
      InOut.print_block (InOut.Div (InOut.Normal, [
          InOut.P [ InOut.Text (get_translation "fastOrSlowGeneration") ] ;
          InOut.Div (InOut.Centered, [
              InOut.Text (get_translation "fastGeneration") ;
              InOut.Node fastOrSlow ;
              InOut.Text (get_translation "slowGeneration")
            ])
        ])) ;
      next_button parameters (fun _ ->
          { parameters with
              player_number = readPlayerNumber () ;
              general_level = readGeneralLevel () ;
              general_complexity = readGeneralComplexity () ;
              computation_power = readFastOrSlow () })
        (Some ask_for_languages) (Some ask_for_categories)
    and ask_for_categories parameters =
      let get_translation = get_translation parameters in
      (** Forcing the data to be loaded. **)
      (if Lwt.state data = Lwt.Sleep then
        InOut.startLoading ()
      else Lwt.return ()) ;%lwt
      let%lwt data = data in
      InOut.stopLoading () ;%lwt
      (** Asking about categories. **)
      let translate_categories =
        let translate_categories = Driver.translates_category data in fun c ->
          let category_name =
            Utils.assert_option __LOC__
              (Translation.translate translate_categories c Translation.generic) in
          Utils.assert_option ("No translation found for category “" ^
                               category_name ^ "” in language “" ^
                               (Translation.iso639 (get_language parameters)) ^ "”.")
            (Translation.translate translate_categories c
              (get_language parameters)) in
      let all_categories = Driver.all_categories data in
      let selected_categories =
        match parameters.categories with
        | None -> Utils.PSet.from_list all_categories
        | Some s -> s in
      let onCategoryClick = ref (fun _ -> ()) in
      let categoriesButtons =
        List.fold_left (fun m c ->
          let dependencies =
            let deps =
              List.map translate_categories (Utils.PSet.to_list
                (Driver.get_category_dependencies data c)) in
            if deps = [] then None
            else
              Some ("(" ^ get_translation "categoryDepends" ^ " "
                    ^ print_list (get_translation "and") deps ^ ")") in
          let (e, set, get) =
            InOut.createSwitch (translate_categories c)
              None dependencies (Utils.PSet.mem c selected_categories)
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
            PMap.fold (fun (e, _, _, _) l -> InOut.Node e :: l)
              categoriesButtons [])
        ])) ;
      next_button parameters (fun _ ->
          let selected_categories =
            PMap.foldi (fun c (_, _, get, _) s ->
              if get () then
                Utils.PSet.add c s
              else s) categoriesButtons Utils.PSet.empty in
          { parameters with categories = Some selected_categories })
        (Some ask_for_basic) (Some ask_for_player_constraints)
    and ask_for_player_constraints parameters =
      let get_translation = get_translation parameters in
      let%lwt data = data in
      let player_number = parameters.player_number in
      let categories = Utils.assert_option __LOC__ parameters.categories in
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
        let (complexity, difficulty) =
          let generalLevel = parameters.general_level in
          let generalComplexity = parameters.general_complexity in
          let generalLevel = generalLevel *. generalLevel in
          let complexityDifficulty =
            10. +. generalLevel *. 90. in
          let result p = int_of_float (0.5 +. complexityDifficulty *. p) in
          (result generalComplexity, result (1. -. generalComplexity)) in
          let player_information = parameters.player_information in
          let len = List.length player_information in
        if len >= player_number then
          List.take player_number player_information
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
                            (ignore categories ; misc (* TODO *))
                          ]) table) ])])) ;
      next_button ~nextText:"startGeneration" parameters (fun _ ->
        { parameters with
            player_information =
              List.map (fun ((_, get_name), (_, get_complexity),
                             (_, get_difficulty), misc) ->
                let misc = ignore misc (* TODO *) in
                (get_name (), get_complexity (), get_difficulty (), misc)) table
        }) (Some ask_for_categories) (Some generate)
    and generate parameters =
      let get_translation = get_translation parameters in
      (** Starts the generation! **)
      InOut.startLoading () ;%lwt
      let%lwt data = data in
      let categories = Utils.assert_option __LOC__ parameters.categories in
      let global =
        let elements_map = Driver.elements data in
        let elements =
          Driver.get_all_elements data categories parameters.player_number in
        let elements =
          List.map (fun e -> PMap.find e elements_map) elements in
        List.fold_left Solver.register_element
          (Solver.empty_global parameters.computation_power) elements in
      let objectives =
        Array.of_list (List.map (fun (_, complexity, difficulty, _) -> {
            Solver.complexity = complexity ;
            Solver.difficulty = difficulty
          }) parameters.player_information) in
      let state = State.create_state parameters.player_number in
      (* TODO: Update the state according to the miscellaneous player
       * informations. *)
      let%lwt state = Solver.solve global state objectives in
      InOut.stopLoading () ;%lwt
      InOut.print_block (InOut.P [
        InOut.Text (get_translation "underConstruction") ;
        InOut.Text (get_translation "participate") ;
        InOut.Link (get_translation "there",
                    "https://github.com/Mbodin/murder-generator") ]) ;
      InOut.print_block (InOut.Div (InOut.Normal,
        let get_name c =
          let c = Utils.Id.to_array c in
          try let (name, _, _, _) = List.nth parameters.player_information c in
              name
          with _ -> "??" in
        InOut.P [ InOut.Text "This is a test" ] (* TODO *)
        :: List.mapi (fun c (name, _, _, _) ->
             let cst = State.get_character_state state in
             let c = Utils.Id.from_array c in
             InOut.P (
                 InOut.Text (name ^ ":")
                 :: PMap.foldi (fun a v l ->
                        let a = State.PlayerAttribute a in
                        let tra =
                          let tr = Driver.translates_attribute data in
                          Utils.assert_option __LOC__
                            (Translation.translate tr a Translation.generic) in
                        let trv =
                          let tr_strict = function
                            | State.NonStrict -> "compatible "
                            | State.LowStrict -> ""
                            | State.Strict -> "strict " in
                          let tr l =
                            let tr = Driver.translates_constructor data in
                            String.concat ", " (List.map (fun v ->
                                let v = State.PlayerConstructor v in
                                Utils.assert_option __LOC__
                                  (Translation.translate tr v Translation.generic))
                              l) in
                          match v with
                          | State.Fixed_value (l, strict) -> tr_strict strict ^ tr l
                          | State.One_value_of l -> "? " ^ tr l in
                        InOut.Text (tra ^ "(" ^ trv ^ "); ") :: l)
                      (State.get_all_attributes_character cst c) []
                    @ PMap.foldi (fun a lv l ->
                          List.map (fun (c', v) ->
                            let a = State.ContactAttribute a in
                            let tra =
                              let tr = Driver.translates_attribute data in
                              Utils.assert_option __LOC__
                                (Translation.translate tr a Translation.generic) in
                            let trv =
                              let tr_strict = function
                                | State.NonStrict -> "compatible "
                                | State.LowStrict -> ""
                                | State.Strict -> "strict " in
                              let tr l =
                                let tr = Driver.translates_constructor data in
                                String.concat ", " (List.map (fun v ->
                                    let v = State.ContactConstructor v in
                                    Utils.assert_option __LOC__
                                      (Translation.translate tr v
                                        Translation.generic)) l) in
                              match v with
                              | State.Fixed_value (l, strict) ->
                                tr_strict strict ^ tr l
                              | State.One_value_of l -> "? " ^ tr l in
                            InOut.Text (tra ^ " to " ^ get_name c'
                              ^ "(" ^ trv ^ "); ")) lv @ l)
                        (State.get_all_contacts_character cst c) []
               )) parameters.player_information)) ;
      next_button parameters (fun _ -> parameters)
        (Some ask_for_player_constraints) None in
    let parameters = {
        language = None ;
        player_number = 13 ;
        general_level = 0.5 ;
        general_complexity = 0.5 ;
        computation_power = 0.5 ;
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
      InOut.stopLoading () ;%lwt
      Lwt.return ()
    with e' -> (** If there have been an error when printing the error,
                * we failback to the console. **)
      Firebug.console##log "Unfortunately, a important error happened." ;
      Firebug.console##log ("Please report it to " ^ issues) ;
      Firebug.console##log ("Primary error details: " ^ Printexc.to_string e) ;
      Firebug.console##log ("Secondary error details: " ^ Printexc.to_string e') ;
      Lwt.return ()

