(** Module Main.
 * This file is the one compiled to JavaScript, then fetched and executed
 * to run the whole program. **)

open ExtList
open ExtString


(** This entire file is parameterised by an interface as specified in the
 * InOut Module. **)
module Main (IO : InOut.T) = struct

let webpage_link = "https://github.com/Mbodin/murder-generator"
let webpage_issues = "https://github.com/Mbodin/murder-generator/issues"


(** A trace of the menu function to help debugging. **)
let trace = ref ["init"]

(** Adding a message in the trace **)
let add_trace msg = trace := msg :: !trace

(** Get the full trace. **)
let get_trace _ = List.rev !trace

(** The default error messages. **)
let errorTranslationsDefault =
  ("An error occurred!", "Please report it", "there", "Error details:")

(** The translations needed to print error messages. **)
let errorTranslations = ref errorTranslationsDefault

let file_signature file =
  string_of_int (Hashtbl.hash file)

(** Getting and parsing the translations file. **)
let get_translations _ =
  let%lwt (translation, languages) =
    let translations_file = "web/translations.json" in
    let%lwt translations = IO.get_file translations_file in
    add_trace ("getting translation file (" ^ file_signature translations_file ^ ")") ;
    Lwt.return (Translation.from_json translations_file translations) in
  (** Shuffling languages, but putting the user languages on top. **)
  let (matching, nonmatching) =
    List.partition (fun lg ->
      let lg = Translation.iso639 lg in
      List.exists (fun ulg -> String.exists ulg lg) IO.languages) languages in
  Lwt.return (translation, Utils.shuffle matching @ Utils.shuffle nonmatching)

(** Prints a list of strings, [andw] being the word for “and”
 * in the current language.
 * This function makes use of the Oxford comma. **)
let print_list andw = function
  | [] -> "none"
  | a :: [] -> a
  | a :: b :: [] -> a ^ " " ^ andw ^ " " ^ b
  | l ->
    let (l, r) = Utils.assert_option __LOC__ (Utils.list_match_right l) in
    String.concat ", " l ^ ", " ^ andw ^ " " ^ r

(** Get and parse each data file. **)
let get_data _ =
  let intermediate = ref Driver.empty_intermediary in
  Lwt_list.iter_p (fun fileName ->
    let%lwt file = IO.get_file fileName in
    add_trace ("getting " ^ fileName ^ " (" ^ file_signature file ^ ")") ;
    let lexbuf = Lexing.from_string file in
    let file = Driver.parse_lexbuf fileName lexbuf in
    intermediate := Driver.prepare_declarations !intermediate file ;
    Lwt.return ()) MurderFiles.files ;%lwt
  if not (Driver.is_intermediary_final !intermediate) then (
    let categories = Driver.categories_to_be_defined !intermediate in
    let (attributes, contacts) = Driver.attributes_to_be_defined !intermediate in
    let (player_constructors, contact_constructors) =
      Driver.constructors_to_be_defined !intermediate in
    let player_constructors =
      PSet.map (fun (a, c) -> c ^ " (" ^ a ^ ")") player_constructors in
    let contact_constructors =
      PSet.map (fun (a, c) -> c ^ " (" ^ a ^ ")") contact_constructors in
    let tags = Driver.tags_to_be_defined !intermediate in
    let tags = PSet.map (fun (lg, tag) -> lg ^ ":" ^ tag) tags in
    let events = Driver.events_to_be_defined !intermediate in
    let missing str s =
      " Missing " ^ str ^ ": " ^ print_list "and" (PSet.to_list s) ^ "." in
    Lwt.fail (Invalid_argument
      ("Non final intermediary after parsing all files."
       ^ missing "categories" categories
       ^ missing "attributes" attributes
       ^ missing "contacts" contacts
       ^ missing "attributes constructors" player_constructors
       ^ missing "contacts contacts" contact_constructors
       ^ missing "events" events
       ^ missing "language tags" tags)))
  else Lwt.return (Driver.parse !intermediate)

(** Get and parse each name file. **)
let get_names _ =
  Lwt_list.map_p (fun fileName ->
    let%lwt file = IO.get_file fileName in
    add_trace ("getting " ^ fileName ^ " (" ^ file_signature file ^ ")") ;
    let generator =
      try Names.import file
      with Invalid_argument str ->
        invalid_arg ("Error while parsing name file “" ^ fileName ^ "”: " ^ str) in
    Lwt.return generator) NameFiles.files


(** A type to store what each page of the menu provides. **)
type parameters = {
    language : Translation.language option ;
    player_number : int ;
    general_level : float ;
    general_complexity : float ;
    play_date : Date.t ;
    computation_power : float ;
    categories : Id.t PSet.t option ;
    player_information : (string (** Character name *)
                          * int (** Complexity **)
                          * int (** Difficulty **)
                          * Attribute.PlayerAttribute.constructor list
                              (** Preset attributes **)) list ;
    chosen_productions : string PSet.t (** The name of each chosen production. **)
  }

(** URL tags **)
let urltag_lang = "lang"
let urltag_number = "num"
let urltag_level = "level"
let urltag_complexity = "comp"
let urltag_date = "date"
let urltag_power = "power"
let urltag_categories = "cats"

exception InvalidUrlArgument

(** Update player informations from the parameters such that it matches the [player_number] data. **)
let create_player_information names get_translation parameters =
  let lg = Utils.assert_option __LOC__ parameters.language in
  let generator =
    let l = List.filter (fun g -> Names.is_default g lg) names in
    if l <> [] then
      Utils.select_any l
    else if names <> [] then
      Utils.select_any names
    else Names.empty in
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
  add_trace ("switching from " ^ string_of_int len
             ^ " players to " ^ string_of_int parameters.player_number) ;
  if len >= parameters.player_number then
    List.take parameters.player_number player_information
  else
    List.fold_left (fun player_information _ ->
        let name =
          (** Because the generator contains external data, one can hardly
           * assume that it can produce infinitely many different names.
           * We are thus stuck to just generate new ones until a really new
           * one appears. **)
          let rec aux fuel =
            let name = Names.generate generator in
            match fuel with
            | 0 -> name
            | n ->
              if List.exists (fun (name', _, _, _) -> name' = name) player_information then
                aux (n - 1)
              else name in
          aux 100 in
        (name, complexity, difficulty, []) :: player_information)
      player_information (Utils.seq (parameters.player_number - len))

(** Get the categories from the parameters, but return the default value if not defined. **)
let get_categories data parameters =
  match parameters.categories with
  | Some s -> s
  | None -> PSet.from_list (Driver.all_categories data)

(** Get all elements corresponding to these settings. **)
let get_all_elements data parameters =
  let lg = Utils.assert_option __LOC__ parameters.language in
  let categories = get_categories data parameters in
  Driver.get_all_elements data lg categories parameters.player_number

(** The main script. **)
let main =
  try%lwt
    IO.clear_response () ;
    let%lwt (translation, languages) = get_translations () in
    add_trace "Translations ready" ;
    let get_translation_language lg key =
      Utils.assert_option ("No key `" ^ key ^ "' found for language `"
                           ^ (Translation.iso639 lg) ^ "' at " ^ __LOC__ ^ ".")
        (Translation.translate translation lg key) in
    let get_language p = Utils.assert_option __LOC__ p.language in
    let get_translation p = get_translation_language (get_language p) in
    (** Adds a “next” and “previous” buttons and call them when needed.
     * This function waits for the user to either click on the previous or
     * next button, then calls the function to get the parameters, and
     * finally calls the appropriate function.
     * Both functions are given as option-types: if [None] is given, the
     * corresponding button doesn’t appear.
     * The text (more precisely, its key) for each button can be changed. **)
    let next_button ?(previousText = "previous") ?(nextText = "next")
        w p get_parameters previous next =
      let jump f _ =
        IO.clear_response () ;
        Lwt.wakeup_later w (fun _ -> f (get_parameters ())) in
      let createListButton dir text f =
        match f with
        | None -> []
        | Some f ->
          let t = Lwt.task () in
          [ InOut.LinkContinuation (dir, get_translation p text, jump (f t)) ] in
      let previous = createListButton false previousText previous in
      let next = createListButton true nextText next in
      IO.print_block (InOut.Div (InOut.Centered,
        if previous = [] || next = [] then previous @ next
        else previous @ [ InOut.Space ] @ next)) in
    (** We request the data without forcing it yet. **)
    let data = get_data () in
    let names = get_names () in

    let rec ask_for_languages _ parameters =
      (** Showing to the user all available languages. **)
      add_trace "ask_for_languages" ;
      IO.set_parameters [] ;
      errorTranslations := errorTranslationsDefault ;
      IO.stopLoading () ;%lwt
      let%lwt language =
        let (cont, w) = Lwt.task () in
        IO.print_block (InOut.Div (InOut.Normal, List.map (fun lg ->
          let get_translation = get_translation_language lg in
          InOut.Div (InOut.Centered, [
            InOut.P [ InOut.LinkContinuation (true, get_translation "name",
              fun _ ->
                IO.clear_response () ;
                errorTranslations :=
                  (get_translation "error", get_translation "report",
                   get_translation "there", get_translation "errorDetails") ;
                Lwt.wakeup_later w lg) ]])) languages)) ;
        IO.stopLoading () ;%lwt
        cont in
      load_or_create (Lwt.task ()) { parameters with language = Some language }

    and load_or_create (cont, w) parameters =
      (** Describing the project to the user. **)
      add_trace "load_or_create" ;
      IO.set_parameters [(urltag_lang, Translation.iso639 (get_language parameters))] ;
      let get_translation = get_translation parameters in
      IO.print_block (InOut.P [
          InOut.Text (get_translation "description") ;
          InOut.Text (get_translation "openSource") ;
          InOut.Link (get_translation "there", webpage_link)
        ]) ;
      (** Start a new scenario. **)
      IO.print_block (InOut.P [
          InOut.Text (get_translation "createNewScenario") ;
          InOut.LinkContinuation (true, get_translation "startGeneration",
            fun _ ->
              Lwt.wakeup_later w (fun _ ->
                IO.clear_response () ;
                ask_for_basic (Lwt.task ()) parameters)) ]) ;
      (** Suggest to shortcut the questions. **)
      let (numberOfPlayers, readNumberOfPlayers) =
        IO.createNumberInput ~min:1 parameters.player_number in
      IO.print_block (InOut.Div (InOut.Normal, [
          InOut.P [ InOut.Text (get_translation "fastCreation") ] ;
          InOut.Div (InOut.Centered, [
              InOut.Node numberOfPlayers ;
              InOut.LinkContinuation (true, get_translation "startFastGeneration",
                fun _ ->
                  Lwt.wakeup_later w (fun _ ->
                    let parameters =
                      { parameters with
                          player_number = readNumberOfPlayers () ;
                          computation_power = 0. } in
                    let%lwt names = names in
                    let player_information =
                      create_player_information names get_translation parameters in
                    let parameters = { parameters with player_information = player_information } in
                    IO.clear_response () ;
                    generate (Lwt.task ()) parameters))
            ])
        ])) ;
      (** Suggest to shortcut the generation by importing a file. **)
      let (shortcut, readShortcut) =
        IO.createFileImport ["json"] (fun _ ->
          IO.clear_response () ;
          IO.setLoading 0.2 ;%lwt
          IO.startLoading ()) in
      IO.print_block (InOut.Div (InOut.Normal, [
          InOut.P [ InOut.Text (get_translation "importFileShortcut") ] ;
          InOut.Div (InOut.Centered, [
              InOut.Node shortcut ;
              InOut.LinkContinuation (true, get_translation "shortcutGeneration",
                fun _ ->
                  Lwt.wakeup_later w (fun _ ->
                    let%lwt (fileName, str) = readShortcut () in
                    if fileName = "" && str = "" then (
                      IO.print_block ~error:true (InOut.P [
                        InOut.Text (get_translation "noFileSelected") ]) ;
                      IO.stopLoading () ;%lwt
                      load_or_create (Lwt.task ()) parameters
                    ) else (
                      let%lwt data = data in
                      let (names, state) =
                        Export.from_json (Driver.get_import_information data)
                          fileName str in
                      let informations =
                        List.mapi (fun c name ->
                          let c = Id.from_array c in
                          let st = State.get_relation_state state in
                          let attributes =
                            let m =
                              State.get_all_attributes_character
                                (State.get_character_state state) c in
                            PMap.fold (fun c l ->
                              match c with
                              | State.Fixed_value (c :: [], _) -> c :: l
                              | _ -> l) m [] in
                          (name, State.character_complexity st c,
                           State.character_difficulty st c,
                           attributes)) names in
                      let parameters =
                        { parameters with
                            player_number = List.length informations ;
                            player_information = informations ;
                            categories =
                              Some (PSet.from_list
                                      (Driver.all_categories data)) } in
                      choose_formats (Lwt.return state) (Lwt.task ()) parameters
                    )))
            ])
        ])) ;
      IO.stopLoading () ;%lwt
      next_button ~nextText:"startGeneration" w parameters (fun _ -> parameters)
        (Some ask_for_languages) (Some ask_for_basic) ;
      let%lwt cont = cont in cont ()

    and ask_for_basic (cont, w) parameters =
      (** Asking the first basic questions about the murder party. **)
      add_trace "ask_for_basic" ;
      let get_translation = get_translation parameters in
      let (playerNumber, readPlayerNumber) =
        IO.createNumberInput ~min:1 parameters.player_number in
      IO.print_block (InOut.P [
          InOut.Text (get_translation "howManyPlayers") ;
          InOut.Node playerNumber
        ]) ;
      let (generalLevel, readGeneralLevel) =
        IO.createPercentageInput parameters.general_level in
      IO.print_block (InOut.Div (InOut.Normal, [
          InOut.P [ InOut.Text (get_translation "experience") ] ;
          InOut.Div (InOut.Centered, [
              InOut.Text (get_translation "beginner") ;
              InOut.Node generalLevel ;
              InOut.Text (get_translation "experienced")
            ])
        ])) ;
      let (generalComplexity, readGeneralComplexity) =
        IO.createPercentageInput parameters.general_complexity in
      IO.print_block (InOut.Div (InOut.Normal, [
          InOut.P [ InOut.Text (get_translation "lengthOfCharacterSheets") ] ;
          InOut.Div (InOut.Centered, [
              InOut.Text (get_translation "shortSheets") ;
              InOut.Node generalComplexity ;
              InOut.Text (get_translation "longSheets")
            ])
        ])) ;
      let (playDate, readPlayDate) =
        IO.createDateInput parameters.play_date in
      IO.print_block (InOut.P [
          InOut.Text (get_translation "whenDoYouPlanToPlay") ;
          InOut.Node playDate
        ]) ;
      let (fastOrSlow, readFastOrSlow) =
        IO.createPercentageInput parameters.computation_power in
      IO.print_block (InOut.Div (InOut.Normal, [
          InOut.P [ InOut.Text (get_translation "fastOrSlowGeneration") ] ;
          InOut.Div (InOut.Centered, [
              InOut.Text (get_translation "fastGeneration") ;
              InOut.Node fastOrSlow ;
              InOut.Text (get_translation "slowGeneration")
            ])
        ])) ;
      next_button w parameters (fun _ ->
          { parameters with
              player_number = readPlayerNumber () ;
              general_level = readGeneralLevel () ;
              general_complexity = readGeneralComplexity () ;
              play_date = readPlayDate () ;
              computation_power = readFastOrSlow () })
        (Some load_or_create) (Some ask_for_categories) ;
      let%lwt cont = cont in cont ()

    and ask_for_categories (cont, w) parameters =
      (** Asking about categories. **)
      add_trace "ask_for_categories" ;
      IO.set_parameters [
          (urltag_lang, Translation.iso639 (get_language parameters)) ;
          (urltag_number, string_of_int parameters.player_number) ;
          (urltag_level, string_of_float parameters.general_level) ;
          (urltag_complexity, string_of_float parameters.general_complexity) ;
          (urltag_date, Date.iso8601 parameters.play_date) ;
          (urltag_power, string_of_float parameters.computation_power)
        ] ;
      let get_translation = get_translation parameters in
      (** Forcing the data to be loaded. **)
      (if Lwt.state data = Lwt.Sleep then (
         IO.setLoading 0.2 ;%lwt
         IO.startLoading ()
       ) else Lwt.return ()) ;%lwt
      let%lwt data = data in
      IO.stopLoading () ;%lwt
      let _ =
        let total = Driver.total_number_of_elements data in
        let n = Driver.number_of_elements data (get_language parameters) in
        assert (n <= total) ;
        let p =
          let n = float_of_int n in
          let total = float_of_int total in
          int_of_float (100. *. n /. total) in
        if n < 9 * total / 10 then
          IO.print_block ~error:true (InOut.P [
              InOut.Text (get_translation "WarningVeryFewTranslations") ;
              InOut.Text (string_of_int p ^ get_translation "percent" ^ ".")
            ]) in
      let translate_categories =
        let translate_categories =
          (Driver.get_translations data).Translation.category in
        Translation.force_translate translate_categories (get_language parameters) in
      let translate_category_descriptions =
        let translate_category_descriptions =
          (Driver.get_translations data).Translation.category_description in
        Translation.force_translate translate_category_descriptions (get_language parameters) in
      let all_categories = Driver.all_categories data in
      let selected_categories =
        match parameters.categories with
        | None -> PSet.from_list all_categories
        | Some s -> s in
      let get_element_number s =
        List.length (get_all_elements data { parameters with categories = Some s }) in
      let (elementNumberNode, setElementNumber) =
        IO.createNumberOutput (get_element_number selected_categories) in
      let onCategoryClick = ref (fun _ -> ()) in
      let categoriesButtons =
        List.fold_left (fun m c ->
          let dependencies =
            let deps =
              List.map translate_categories (PSet.to_list
                (Driver.get_category_dependencies data c)) in
            if deps = [] then None
            else
              Some ("(" ^ get_translation "categoryDepends" ^ " "
                    ^ print_list (get_translation "and") deps ^ ")") in
          let (e, set, get) =
            IO.createSwitch (translate_categories c)
              (Some (translate_category_descriptions c))
              None dependencies (PSet.mem c selected_categories)
              (fun _ -> !onCategoryClick c) in
          PMap.add c (e, set, get, PSet.empty) m) PMap.empty all_categories in
      let categoriesButtons =
        List.fold_left (fun m c ->
            let deps = Driver.get_category_dependencies data c in
            PSet.fold (fun cd m ->
              let (e, set, get, ideps) = PMap.find cd m in
              PMap.add cd (e, set, get, PSet.add c ideps) m) m deps)
          categoriesButtons all_categories in
      let get_selected_categories _ =
        PMap.foldi (fun c (_, _, get, _) s ->
          if get () then
            PSet.add c s
          else s) categoriesButtons PSet.empty in
      let update_element_number _ =
        setElementNumber (get_element_number (get_selected_categories ())) in
      onCategoryClick := (fun c ->
        let (_, _, get, ideps) = PMap.find c categoriesButtons in
        (if get () then
           let deps = Driver.get_category_dependencies data c in
           PSet.iter (fun c ->
             let (_, set, _, _) = PMap.find c categoriesButtons in
             set true) deps
         else
           PSet.iter (fun c ->
             let (_, set, _, _) = PMap.find c categoriesButtons in
             set false) ideps) ;
        update_element_number ()) ;
      IO.print_block (InOut.Div (InOut.Normal, [
          InOut.P [ InOut.Text (get_translation "unselectCategories") ] ;
          InOut.Div (InOut.Centered, [
              InOut.LinkContinuation (false, get_translation "noCategories",
                fun _ ->
                  PMap.iter (fun _ (_, set, _, _) -> set false) categoriesButtons ;
                  update_element_number ()) ;
              InOut.Space ;
              InOut.LinkContinuation (true, get_translation "allCategories",
                fun _ ->
                  PMap.iter (fun _ (_, set, _, _) -> set true) categoriesButtons ;
                  update_element_number ()) ;
            ]) ;
          InOut.List (false,
            PMap.fold (fun (e, _, _, _) l -> InOut.Node e :: l)
            categoriesButtons []) ;
          InOut.P [
              InOut.Text (get_translation "categoriesExplain") ;
              InOut.Text (get_translation "categoriesExamples")
            ]
        ])) ;
      IO.print_block (InOut.P [
          InOut.Text (get_translation "howManyElements1") ;
          InOut.Node elementNumberNode ;
          InOut.Text (get_translation "howManyElements2")
        ]) ;
      next_button w parameters (fun _ ->
          let selected_categories = get_selected_categories () in
          { parameters with categories = Some selected_categories })
        (Some ask_for_basic) (Some ask_for_player_constraints) ;
      let%lwt cont = cont in cont ()

    and ask_for_player_constraints (cont, w) parameters =
      (** Asking about individual player constraints. **)
      add_trace "ask_for_player_constraints" ;
      let get_translation = get_translation parameters in
      let%lwt data = data in
      let%lwt names = names in
      let translate_categories_generic =
        let translate_categories =
          (Driver.get_translations data).Translation.category in
        Translation.force_translate translate_categories Translation.generic in
      let categories =
        if PSet.length (get_categories data parameters)
           = List.length (Driver.all_categories data) then
          "all"
        else
          String.concat "," (List.map translate_categories_generic
            (PSet.to_list (get_categories data parameters))) in
      IO.set_parameters [
          (urltag_lang, Translation.iso639 (get_language parameters)) ;
          (urltag_number, string_of_int parameters.player_number) ;
          (urltag_level, string_of_float parameters.general_level) ;
          (urltag_complexity, string_of_float parameters.general_complexity) ;
          (urltag_date, Date.iso8601 parameters.play_date) ;
          (urltag_power, string_of_float parameters.computation_power) ;
          (urltag_categories, categories)
        ] ;
      IO.stopLoading () ;%lwt
      IO.print_block (InOut.Div (InOut.Normal, [
        InOut.P [ InOut.Text (get_translation "individualConstraints") ] ;
        InOut.P [ InOut.Text (get_translation "complexityDifficultyExplanation") ] ;
        InOut.List (true, [
            InOut.Text (get_translation "lowComplexityLowDifficulty") ;
            InOut.Text (get_translation "lowComplexityHighDifficulty") ;
            InOut.Text (get_translation "highComplexityLowDifficulty") ;
            InOut.Text (get_translation "highComplexityHighDifficulty") ])])) ;
      let (changingNames, get_name_generator, changingNamesOK) =
        let lg = get_language parameters in
        let (default, non_default) = List.partition (fun g -> Names.is_default g lg) names in
        let names = default @ non_default in
        let l =
          Utils.list_map_filter (fun g ->
            let tr = Names.translate g in
            Option.map (fun txt -> (txt, g))
              (Translation.translate tr lg ())) names in
        let (node, get) = IO.createListInput l in
        (node, get, l <> []) in
      let player_information = create_player_information names get_translation parameters in
      let constructor_maps = Driver.get_constructor_maps data in
      let translation = Driver.get_translations data in
      let constructor_infos c =
        let m = constructor_maps.Attribute.player in
        let a =
          Utils.assert_option __LOC__
            (Attribute.PlayerAttribute.constructor_attribute m c) in
        let name =
          let an =
            Utils.assert_option __LOC__
              (Attribute.PlayerAttribute.attribute_name m a) in
          let cn =
            Utils.assert_option __LOC__
              (Attribute.PlayerAttribute.constructor_name m c) in
          an ^ ": " ^ cn in
        let translated =
          Translation.force_translate translation.Translation.attribute
            (get_language parameters) (Attribute.PlayerAttribute a) ^ ": "
          ^ fst (Translation.gforce_translate translation.Translation.constructor
                  (get_language parameters) (Attribute.PlayerConstructor c)
                  (PSet.singleton Translation.base)) in
        (a, Attribute.PlayerAttribute.is_internal m a c, name, translated) in
      let (internal_cons, main_cons) =
        let categories = get_categories data parameters in
        let all_constructors =
          Utils.list_map_filter (fun c ->
            let dep =
              Driver.get_constructor_dependencies data
                (Attribute.PlayerConstructor c) in
            if PSet.incl dep categories then
              Some (c, constructor_infos c)
            else None) (Attribute.PlayerAttribute.all_constructors
                         constructor_maps.Attribute.player) in
        Utils.list_partition_map (fun (c, (a, internal, n, t)) ->
          let r = (c, a, n, t) in
          if internal then Utils.Left r else Utils.Right r) all_constructors in
      let table =
        List.map (fun (name, complexity, difficulty, misc) ->
          (IO.createSettableTextInput name,
           IO.createNumberInput complexity,
           IO.createNumberInput difficulty,
           let getrec =
             (* This reference is frustrating: I could not find another way to make
              * the compiler accept the recursion in this case. *)
             ref (fun _ -> assert false) in
           let (node, get) =
             let proposed =
               try
                 let (_, _, _, t) = Utils.select_any main_cons in
                 get_translation "forExample" ^ " " ^ t
               with Utils.EmptyList -> "" in
             let misc =
               List.map (fun c ->
                 let (a, _, _, t) = constructor_infos c in
                 (t, (a, c))) misc in
             IO.createResponsiveListInput misc proposed (fun txt ->
               let re = Re.Str.regexp_string_case_fold txt in
               let corresponds txt' =
                 try Some (Re.Str.search_forward re txt' 0)
                 with Not_found -> None in
               let already_chosen =
                 PSet.from_list (List.map (fun (a, _) -> a) (!getrec ())) in
               let num_shown = 10 in
               let get_from_list l =
                 let l =
                   Utils.list_map_filter (fun (c, a, n, t) ->
                     if PSet.mem a already_chosen then None
                     else
                       let d =
                         match corresponds t with
                         | Some d -> Some (true, d)
                         | None ->
                           Option.map (fun d -> (false, d)) (corresponds n) in
                       Option.map (fun d -> (c, a, d, t)) d) l in
                 let l =
                   List.sort ~cmp:(fun (_, _, (t1, d1), _) (_, _, (t2, d2), _) ->
                     if t1 && not t2 then -1
                     else if t2 && not t1 then 1
                     else compare d1 d2) l in
                 Utils.list_header num_shown l in
               let l =
                 Utils.list_header num_shown
                   (get_from_list main_cons @ get_from_list internal_cons) in
               List.map (fun (c, a, _, t) -> (t, (a, c))) l) in
           getrec := get ;
           (node, get))) player_information in
      if changingNamesOK then
        IO.print_block (InOut.P [
            InOut.Text (get_translation "changingNames") ;
            InOut.Node changingNames ;
            InOut.LinkContinuation (true, get_translation "changeNames", fun _ ->
              match get_name_generator () with
              | None -> ()
              | Some gen ->
                ignore (List.fold_left (fun avoid ((_, _, set_name), _, _, _) ->
                  (** Again, as the generator contains external data, one can hardly
                   * assume that it can produce infinitely many different names.
                   * We are thus stuck to just generate new ones until a really new
                   * one appears. **)
                  let name =
                    let rec aux fuel =
                      let name = Names.generate gen in
                      match fuel with
                      | 0 -> name
                      | n -> if PSet.mem name avoid then aux (fuel - 1) else name in
                    aux 100 in
                  set_name name ;
                  PSet.add name avoid) PSet.empty table))
          ]) ;
      IO.print_block (InOut.Div (InOut.Normal, [
        InOut.P [ InOut.Text (get_translation "changeThisTable") ] ;
        InOut.Div (InOut.Centered, [
          InOut.Table (["table"],
                       [(InOut.Text (get_translation "playerName"), InOut.default) ;
                        (InOut.Text (get_translation "complexity"), InOut.default) ;
                        (InOut.Text (get_translation "difficulty"), InOut.default) ;
                        (InOut.Text (get_translation "miscellaneous"),
                         InOut.default)],
                       List.map (fun ((name, _, _), (complexity, _),
                                      (difficulty, _), (misc, _)) -> ([], [
                           (InOut.Node name, InOut.default) ;
                           (InOut.Node complexity, InOut.default) ;
                           (InOut.Node difficulty, InOut.default) ;
                           (InOut.Node misc, InOut.default)
                         ])) table) ])])) ;
      next_button ~nextText:"startGeneration" w parameters (fun _ ->
        { parameters with
            player_information =
              List.map (fun ((_, get_name, _), (_, get_complexity),
                             (_, get_difficulty), (_, get_misc)) ->
                (get_name (), get_complexity (),
                 get_difficulty (), List.map snd (get_misc ()))) table
        }) (Some ask_for_categories) (Some generate) ;
      let%lwt cont = cont in cont ()

    and generate task parameters =
      (** Starting the generation. **)
      add_trace "generate" ;
      let get_translation = get_translation parameters in
      IO.setLoading 0. ;%lwt
      let%lwt names = names in
      let parameters =
        { parameters with
            player_information = create_player_information names get_translation parameters } in
      let state =
        IO.pause () ;%lwt
        let%lwt data = data in
        let categories = get_categories data parameters in
        let parameters = { parameters with categories = Some categories } in
        let global =
          let elements_map = Driver.elements data in
          let elements = get_all_elements data parameters in
          let elements =
            List.map (fun e -> PMap.find e elements_map) elements in
          List.fold_left Solver.register_element
            (Solver.empty_global parameters.computation_power) elements in
        let objectives =
          Array.of_list (List.map (fun (_, complexity, difficulty, _) -> {
              State.complexity = complexity ;
              State.difficulty = difficulty
            }) parameters.player_information) in
        let state = State.create_state parameters.player_number in
        let (state, diff) =
          let diff = Element.empty_difference in
          Utils.assert_option __LOC__
            (Utils.list_fold_lefti (fun i stdiff (_, _, _, attributes) ->
                Utils.if_option stdiff (fun (state, diff) ->
                  Utils.apply_option
                    (Element.apply_constructors (Driver.get_constructor_maps data)
                      state (Id.from_array i) attributes)
                    (fun (state, diff') ->
                      (state, Element.merge_attribute_differences diff diff'))))
              (Some (state, diff)) parameters.player_information) in
        Solver.solve_with_difference IO.setLoading global state diff objectives in
      choose_formats state task parameters

    and choose_formats state (cont, w) parameters =
      (** Asking the user to what formats we should export the scenario. **)
      add_trace "choose_formats" ;
      IO.unset_printing_mode () ;
      IO.stopLoading () ;%lwt
      let get_translation = get_translation parameters in
      if Lwt.state state = Lwt.Sleep then
        IO.print_block (InOut.P [
          InOut.Text (get_translation "backgroundGeneration") ;
          InOut.Link (get_translation "there", webpage_link) ]) ;
      let exportButtons =
        List.map (fun (name, descr) ->
            let (node, set, get) =
              IO.createSwitch (get_translation "generateAs"
                               ^ " " ^ get_translation name)
                (Some (get_translation descr)) None None
                (PSet.mem name parameters.chosen_productions) Utils.id in
            (name, InOut.Node node, set, get))
          (("html", "htmlDescription")
           :: List.map (fun (name, descr, _, _, _, _) -> (name, descr))
                Export.all_production) in
      IO.print_block (InOut.Div (InOut.Normal, [
          InOut.P [ InOut.Text (get_translation "exportPossibilities") ] ;
          InOut.List (false,
            List.map (fun (_, node, _, _) -> node) exportButtons)
        ])) ;
      let check cont task parameters =
        if PSet.is_empty parameters.chosen_productions then (
          IO.print_block ~error:true (InOut.P [
            InOut.Text (get_translation "noProductionSelected") ]) ;
          choose_formats state task parameters
        ) else (
          IO.startLoading () ;%lwt
          cont task parameters
        ) in
      next_button w parameters (fun _ ->
          { parameters with chosen_productions =
              List.fold_left (fun s (name, _, _, get) ->
                if get () then PSet.add name s else s) PSet.empty exportButtons })
        (Some ask_for_player_constraints) (Some (check (display state))) ;
      let%lwt cont = cont in cont ()

    and display state (cont, w) parameters =
      (** Exporting the generated state to various formats. **)
      add_trace "display" ;
      let get_translation = get_translation parameters in
      let%lwt data = data in
      let%lwt state = state in
      let estate =
        Export.process {
          Export.language = get_language parameters ;
          Export.date = parameters.play_date ;
          Export.names =
            List.map (fun (name, _, _, _) -> name) parameters.player_information ;
          Export.translation = Driver.get_translations data ;
          Export.generic_translation = translation ;
          Export.constructor_maps = Driver.get_constructor_maps data ;
          Export.state = state
        } in
      let html = PSet.mem "html" parameters.chosen_productions in
      let chosen_productions =
        List.filter (fun (name, _, _, _, _, _) ->
            PSet.mem name parameters.chosen_productions)
          Export.all_production in
      let switch_printing_mode =
        let get = ref (fun _ -> false) in
        let (node, _, actual_get) =
          IO.createSwitch (get_translation "changeStyles") None
            (Some (get_translation "changeStylesOff"))
            (Some (get_translation "changeStylesOn")) false
            (fun _ ->
              (if !get () then IO.set_printing_mode
               else IO.unset_printing_mode) ()) in
        get := actual_get ;
        node in
      let (results, error) =
        let (results, error) =
          List.fold_left (fun (l, e) (name, descr, mime, ext, native, f) ->
            try
              let file = f estate in
              let fileName = "murder" ^ if ext = "" then "" else ("." ^ ext) in
              let node =
                InOut.Div (InOut.Inlined, [
                    InOut.LinkFile (get_translation "downloadAs"
                                    ^ " " ^ get_translation name,
                                    fileName, mime, native, fun _ -> file) ;
                    InOut.Text (get_translation descr)
                  ]) in
              (node :: l, e)
            with e -> (l, Some e)) ([], None) chosen_productions in
        (List.rev results, error) in
      if results <> [] then
        IO.print_block (InOut.Div (InOut.Normal, [
            InOut.P
              (InOut.Text (get_translation "exportList")
               :: if html then [ InOut.Text (get_translation "exportedAsHTML") ]
                  else []) ;
            InOut.List (true, results)
          ])) ;
      if html then
        IO.print_block (InOut.P [ InOut.Node switch_printing_mode ]) ;
      let error =
        try
          if html then IO.print_block (Export.to_block estate) ;
          error
        with e -> Some e in
      if error <> None then
        IO.print_block ~error:true (InOut.P [ InOut.Text (get_translation "errorWhenExporting") ]) ;
      Option.may (fun e -> raise e) error ;
      IO.stopLoading () ;%lwt
      IO.print_block (InOut.P [
        InOut.Text (get_translation "lookingForContribution") ;
        InOut.Text (get_translation "participate") ;
        InOut.Link (get_translation "there", webpage_link) ]) ;
      next_button w parameters (fun _ -> parameters)
        (Some (choose_formats (Lwt.return state))) None ;
      let%lwt cont = cont in cont () in

    (** Setting the environment. **)
    let parameters = {
        language = None ;
        player_number = 7 ;
        general_level = 0.5 ;
        general_complexity = 0.5 ;
        play_date = Date.now ;
        computation_power = 0.5 ;
        categories = None ;
        player_information = [] ;
        chosen_productions = PSet.from_list ["html" ; "json"]
      } in
    let arguments = IO.get_parameters () in
    let print_jump w parameters =
      let get_translation = get_translation parameters in
      IO.print_block (InOut.P [
          InOut.Text (get_translation "linkJumped") ;
          InOut.LinkContinuation (true, get_translation "jumpBack",
            fun _ ->
              Lwt.wakeup_later w (fun _ ->
                IO.clear_response () ;
                ask_for_languages (Lwt.task ()) parameters))
        ]) in
    match List.assoc_opt urltag_lang arguments with
    | None -> (** No language is provided. **)
      ask_for_languages (Lwt.task ()) parameters
    | Some lg ->
      let lg = Translation.from_iso639 lg in
      match Translation.translate translation lg "iso639" with
      | None -> (** Invalid language. **)
        ask_for_languages (Lwt.task ()) parameters
      | Some _ ->
        let parameters = { parameters with language = Some lg } in
        let (cont, w) = Lwt.task () in
        print_jump w parameters ;
        match Utils.list_map_filter (fun tag -> List.assoc_opt tag arguments)
                [urltag_number ; urltag_level ; urltag_complexity ;
                 urltag_date ; urltag_power ] with
        | num :: level :: comp :: date :: power :: [] ->
          (try
            let num = int_of_string num in
            if num < 1 then raise InvalidUrlArgument ;
            let level = float_of_string level in
            if level < 0. || level > 1. then raise InvalidUrlArgument ;
            let comp = float_of_string comp in
            if comp < 0. || comp > 1. then raise InvalidUrlArgument ;
            let date =
              try Date.from_iso8601 date
              with _ -> raise InvalidUrlArgument in
            let power = float_of_string power in
            if power < 0. || power > 1. then raise InvalidUrlArgument ;
            let parameters =
              { parameters with
                  player_number = num ;
                  general_level = level ;
                  general_complexity = comp ;
                  play_date = date ;
                  computation_power = power } in
            match List.assoc_opt urltag_categories arguments with
            | None ->
              ask_for_categories (cont, w) parameters
            | Some cats ->
              let%lwt data = data in
              let cats =
                let translate_categories =
                  let translate_categories =
                    (Driver.get_translations data).Translation.category in
                  Translation.force_translate translate_categories Translation.generic in
                let all_categories = Driver.all_categories data in
                let assoc_categories =
                  List.map (fun c -> (translate_categories c, c)) all_categories in
                if cats = "" then
                  Some []
                else if cats = "all" then
                  Some all_categories
                else
                  let cats = String.split_on_char ',' cats in
                  Utils.list_map_option (fun c ->
                    List.assoc_opt c assoc_categories) cats in
              match cats with
              | None ->
                ask_for_categories (cont, w) parameters
              | Some cats ->
                let parameters =
                  { parameters with categories = Some (PSet.from_list cats) } in
                ask_for_player_constraints (cont, w) parameters
           with InvalidUrlArgument ->
             load_or_create (cont, w) parameters)
        | _ ->
          load_or_create (cont, w) parameters

  (** Reporting errors. **)
  with e ->
    try%lwt
      let (errorOccurred, reportIt, there, errorDetails) = !errorTranslations in
      IO.print_block ~error:true (InOut.Div (InOut.Normal, [
          InOut.P [
              InOut.Text errorOccurred ; InOut.Text reportIt ;
              InOut.Link (there, webpage_issues)
            ] ;
          InOut.P [
              InOut.Text errorDetails ;
              InOut.Text (Printexc.to_string e) ;
              InOut.Text "; " ;
              InOut.Text Version.version ;
              InOut.Text "; " ;
              InOut.Text (String.concat ", " (get_trace ()))
            ]
        ])) ;
      IO.stopLoading () ;%lwt
      Lwt.return ()
    with e' -> (** If there have been an error when printing the error,
                * we failback to the console. **)
      IO.log "Unfortunately, a important error happened." ;
      IO.log ("Please report it to " ^ webpage_issues) ;
      IO.log ("Primary error details: " ^ Printexc.to_string e) ;
      IO.log ("Secondary error details: " ^ Printexc.to_string e') ;
      IO.log ("Version: " ^ Version.version) ;
      IO.log ("Trace: " ^ String.concat "; " (get_trace ())) ;
      Lwt.return ()

end

