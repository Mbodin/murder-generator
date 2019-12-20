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
let get_names data =
  let%lwt data = data in
  let constructor_maps = Driver.get_constructor_maps data in
  Lwt_list.map_p (fun fileName ->
    let%lwt file = IO.get_file fileName in
    add_trace ("getting " ^ fileName ^ " (" ^ file_signature file ^ ")") ;
    let generator =
      try Names.import constructor_maps.Attribute.player file
      with Invalid_argument str ->
        invalid_arg ("Error while parsing name file “" ^ fileName ^ "”: " ^ str) in
    Lwt.return generator) NameFiles.files

(** A type to store player information **)
type player_information = {
    name : string (** Character name *) ;
    complexity : int (** Complexity **) ;
    difficulty : int (** Difficulty **) ;
    attributes : Attribute.PlayerAttribute.constructor list (** Preset attributes **) ;
    contacts : Attribute.ContactAttribute.constructor list list (** Preset contacts **)
  }

(** A type to store what each page of the menu provides. **)
type parameters = {
    language : Translation.language option ;
    player_number : int ;
    general_level : float ;
    general_complexity : float ;
    play_date : Date.t ;
    computation_power : float ;
    categories : Id.t PSet.t option ;
    player_information : player_information list ;
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
        let (name, attributes) =
          (** Because the generator contains external data, one can hardly
           * assume that it can produce infinitely many different names.
           * We are thus stuck to just generate new ones until a really new
           * one appears. **)
          let rec aux fuel =
            let (name, attributes) = Names.generate generator PSet.empty in
            match fuel with
            | 0 -> (name, attributes)
            | n ->
              if List.exists (fun infos -> infos.name = name) player_information then
                aux (n - 1)
              else (name, attributes) in
          aux 100 in {
          name = name ;
          complexity = complexity ;
          difficulty = difficulty ;
          attributes = attributes ;
          contacts = []
        } :: player_information)
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

(** In order to factorise between attributes and contact, the following two declarations
 * defines some functions in common between the two structures, so that subfunctions can
 * be reused for both. **)
let attribute_functions =
  (Attribute.PlayerAttribute.all_constructors,
   Attribute.PlayerAttribute.constructor_attribute,
   Attribute.PlayerAttribute.attribute_name,
   Attribute.PlayerAttribute.constructor_name,
   Attribute.PlayerAttribute.is_internal,
   (fun m -> m.Attribute.player),
   (fun c -> Attribute.PlayerAttribute c),
   (fun c -> Attribute.PlayerConstructor c))
let contact_functions =
  (Attribute.ContactAttribute.all_constructors,
   Attribute.ContactAttribute.constructor_attribute,
   Attribute.ContactAttribute.attribute_name,
   Attribute.ContactAttribute.constructor_name,
   Attribute.ContactAttribute.is_internal,
   (fun m -> m.Attribute.contact),
   (fun c -> Attribute.ContactAttribute c),
   (fun c -> Attribute.ContactConstructor c))

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
          [ InOut.LinkContinuation (dir, Button dir,
                                    get_translation p text, jump (f t)) ] in
      let previous = createListButton false previousText previous in
      let next = createListButton true nextText next in
      IO.print_block (InOut.Div (InOut.Centered,
        if previous = [] || next = [] then previous @ next
        else previous @ [ InOut.Space ] @ next)) in
    (** We request the data without forcing it yet. **)
    let data = get_data () in
    let names = get_names data in

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
            InOut.P [ InOut.LinkContinuation (true, InOut.Button true, get_translation "name",
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
          InOut.LinkExtern (InOut.Simple, get_translation "there", webpage_link)
        ]) ;
      (** Start a new scenario. **)
      IO.print_block (InOut.P [
          InOut.Text (get_translation "createNewScenario") ;
          InOut.LinkContinuation (true, InOut.Button true, get_translation "startGeneration",
            fun _ ->
              Lwt.wakeup_later w (fun _ ->
                IO.clear_response () ;
                ask_for_basic (Lwt.task ()) parameters)) ]) ;
      (** Suggest to shortcut the questions. **)
      let numberOfPlayers =
        IO.createNumberInput ~min:1 parameters.player_number in
      IO.print_block (InOut.Div (InOut.Normal, [
          InOut.P [ InOut.Text (get_translation "fastCreation") ] ;
          InOut.Div (InOut.Centered, [
              InOut.Node numberOfPlayers.IO.node ;
              InOut.LinkContinuation (true, InOut.Button true, get_translation "startFastGeneration",
                fun _ ->
                  Lwt.wakeup_later w (fun _ ->
                    let parameters =
                      { parameters with
                          player_number = numberOfPlayers.IO.get () ;
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
              InOut.LinkContinuation (true, InOut.Button false, get_translation "shortcutGeneration",
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
                          let contacts =
                            List.mapi (fun c' _ ->
                              let c' = Id.from_array c' in
                              let m =
                                State.get_all_contact_character
                                  (State.get_character_state state) c c' in
                              Utils.list_map_filter (function
                                | (_, State.Fixed_value (c :: [], _)) -> Some c
                                | _ -> None) m) names in {
                            name = name ;
                            complexity = State.character_complexity st c ;
                            difficulty = State.character_difficulty st c ;
                            attributes = attributes ;
                            contacts = contacts
                          }) names in
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
        (Some ask_for_languages) None ;
      let%lwt cont = cont in cont ()

    and ask_for_basic (cont, w) parameters =
      (** Asking the first basic questions about the murder party. **)
      add_trace "ask_for_basic" ;
      let get_translation = get_translation parameters in
      let playerNumber =
        IO.createNumberInput ~min:1 parameters.player_number in
      IO.print_block (InOut.P [
          InOut.Text (get_translation "howManyPlayers") ;
          InOut.Node playerNumber.IO.node
        ]) ;
      let generalLevel =
        IO.createPercentageInput parameters.general_level in
      IO.print_block (InOut.Div (InOut.Normal, [
          InOut.P [ InOut.Text (get_translation "experience") ] ;
          InOut.Div (InOut.Centered, [
              InOut.Text (get_translation "beginner") ;
              InOut.Node generalLevel.IO.node ;
              InOut.Text (get_translation "experienced")
            ])
        ])) ;
      let generalComplexity =
        IO.createPercentageInput parameters.general_complexity in
      IO.print_block (InOut.Div (InOut.Normal, [
          InOut.P [ InOut.Text (get_translation "lengthOfCharacterSheets") ] ;
          InOut.Div (InOut.Centered, [
              InOut.Text (get_translation "shortSheets") ;
              InOut.Node generalComplexity.IO.node ;
              InOut.Text (get_translation "longSheets")
            ])
        ])) ;
      let playDate =
        IO.createDateInput parameters.play_date in
      IO.print_block (InOut.P [
          InOut.Text (get_translation "whenDoYouPlanToPlay") ;
          InOut.Node playDate.IO.node
        ]) ;
      let fastOrSlow =
        IO.createPercentageInput parameters.computation_power in
      IO.print_block (InOut.Div (InOut.Normal, [
          InOut.P [ InOut.Text (get_translation "fastOrSlowGeneration") ] ;
          InOut.Div (InOut.Centered, [
              InOut.Text (get_translation "fastGeneration") ;
              InOut.Node fastOrSlow.IO.node ;
              InOut.Text (get_translation "slowGeneration")
            ])
        ])) ;
      next_button w parameters (fun _ ->
          { parameters with
              player_number = playerNumber.IO.get () ;
              general_level = generalLevel.IO.get () ;
              general_complexity = generalComplexity.IO.get () ;
              play_date = playDate.IO.get () ;
              computation_power = fastOrSlow.IO.get () })
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
          IO.print_block ~error:(n < total / 2) (InOut.P [
              InOut.Text (get_translation "WarningVeryFewTranslations") ;
              InOut.Text (string_of_int p ^ get_translation "percent" ^ ".") ;
               InOut.Text (get_translation "lookingForContribution") ;
               InOut.Text (get_translation "participate") ;
               InOut.LinkExtern (InOut.Simple, get_translation "there", webpage_link)
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
          let e =
            IO.createSwitch (translate_categories c)
              (Some (translate_category_descriptions c))
              None dependencies (PSet.mem c selected_categories) in
          e.IO.onChange (fun _ -> !onCategoryClick c) ;
          PMap.add c (e, PSet.empty) m) PMap.empty all_categories in
      let categoriesButtons =
        List.fold_left (fun m c ->
            let deps = Driver.get_category_dependencies data c in
            PSet.fold (fun cd m ->
              let (e, ideps) = PMap.find cd m in
              PMap.add cd (e, PSet.add c ideps) m) m deps)
          categoriesButtons all_categories in
      let get_selected_categories _ =
        PMap.foldi (fun c (e, _) s ->
          if e.IO.get () then
            PSet.add c s
          else s) categoriesButtons PSet.empty in
      let update_element_number _ =
        setElementNumber (get_element_number (get_selected_categories ())) in
      onCategoryClick := (fun c ->
        let (e, ideps) = PMap.find c categoriesButtons in
        (if e.IO.get () then
           let deps = Driver.get_category_dependencies data c in
           PSet.iter (fun c ->
             let (e, _) = PMap.find c categoriesButtons in
             e.IO.set true) deps
         else
           PSet.iter (fun c ->
             let (e, _) = PMap.find c categoriesButtons in
             e.IO.set false) ideps) ;
        update_element_number ()) ;
      IO.print_block (InOut.Div (InOut.Normal, [
          InOut.P [ InOut.Text (get_translation "unselectCategories") ] ;
          InOut.Div (InOut.Centered, [
              InOut.LinkContinuation (false, InOut.Simple, get_translation "noCategories",
                fun _ ->
                  PMap.iter (fun _ (e, _) -> e.IO.set false) categoriesButtons ;
                  update_element_number ()) ;
              InOut.Space ;
              InOut.LinkContinuation (true, InOut.Simple, get_translation "allCategories",
                fun _ ->
                  PMap.iter (fun _ (e, _) -> e.IO.set true) categoriesButtons ;
                  update_element_number ()) ;
            ]) ;
          InOut.List (false,
            PMap.fold (fun (e, _) l -> InOut.Node e.IO.node :: l)
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
      let player_information = create_player_information names get_translation parameters in
      let constructor_maps = Driver.get_constructor_maps data in
      let translation = Driver.get_translations data in
      let all_players = Utils.seq (List.length player_information) in
      (** Given either [attribute_functions] or [contact_functions], return some information
       * about all constructors:
       * - the associated attribute,
       * - whether it is internal,
       * - its generic name,
       * - its translation,
       * - all its possible translations. **)
      let constructor_infos (_, constructor_attribute, attribute_name, constructor_name,
          is_internal, proj, consa, consc) c =
        let m = proj constructor_maps in
        let a = Utils.assert_option __LOC__ (constructor_attribute m c) in
        let name =
          let an = Utils.assert_option __LOC__ (attribute_name m a) in
          let cn = Utils.assert_option __LOC__ (constructor_name m c) in
          an ^ ": " ^ cn in
        let translated =
          Translation.force_translate translation.Translation.attribute
            (get_language parameters) (consa a) ^ ": "
          ^ fst (Translation.gforce_translate translation.Translation.constructor
                  (get_language parameters) (consc c)
                  (PSet.singleton Translation.base)) in
        let all_translations =
          Translation.gall_translations translation.Translation.constructor
            (get_language parameters) (consc c) in
        (a, is_internal m a c, name, translated, all_translations) in
      (** Given either [attribute_functions] or [contact_functions], fetch the constructors
       * that are effectively chosen in the current settings, and return two lists of constructors,
       * one for internal constructors and the other for the normal ones. **)
      let create_lists_cons functions =
        let (all, _, _, _, _, proj, _, cons) = functions in
        let m = proj constructor_maps in
        let categories = get_categories data parameters in
        let all_constructors =
          Utils.list_map_filter (fun c ->
            let dep = Driver.get_constructor_dependencies data (cons c) in
            if PSet.incl dep categories then
              Some (c, constructor_infos functions c)
            else None) (all m) in
        Utils.list_partition_map (fun (c, (a, internal, n, t, ts)) ->
          let r = (c, a, n, t, ts) in
          if internal then Utils.Left r else Utils.Right r) all_constructors in
      let (attribute_internal_cons, attribute_main_cons) =
        create_lists_cons attribute_functions in
      let (contact_internal_cons, contact_main_cons) =
        create_lists_cons contact_functions in
      IO.stopLoading () ;%lwt
      IO.print_block (InOut.P [ InOut.Text (get_translation "individualConstraints") ]) ;
      let get_responsible_list_infos functions c =
        let (a, _, _, t, _) = constructor_infos functions c in
        (t, (a, c)) in
      (** Create a responsive list for either attributes or contacts, depending whether
       * [functions] is [attribute_functions] or [contact_functions]. **)
      let create_responsive_list functions internal_cons main_cons current =
        let pick_list internal_cons main_cons get txt =
          let get = !get in
          let re = Re.Str.regexp_string_case_fold txt in
          let corresponds txt' =
            try Some (Re.Str.search_forward re txt' 0)
            with Not_found -> None in
          let exact = (=) txt in
          let already_chosen =
            PSet.from_list (List.map fst (get ())) in
          let num_shown = 10 in
          let extract_enum enum =
            let rec aux acc already_seen = function
              | 0 -> acc
              | n ->
                match Enum.get enum with
                | None -> acc
                | Some (t, (a, c)) ->
                  if PSet.mem c already_seen then
                    aux acc already_seen n
                  else aux ((t, (a, c)) :: acc) (PSet.add c already_seen) (n - 1) in
            aux [] PSet.empty num_shown in
          let get_partial_enum l =
            Utils.lazy_enum (lazy (
              let l =
                Utils.list_map_filter (fun (c, a, n, t, ts) ->
                  (** As a help for the reader, here are the meaning of each of these values:
                   * - [c] is the considered attribute constructor,
                   * - [a] is the associated attribute,
                   * - [n] its generic name,
                   * - [t] its full translation in the current language,
                   * - [ts] possible contextualised translations for the current language. **)
                  if PSet.mem a already_chosen then None
                  else
                    let d =
                      match Utils.list_find_map_opt corresponds (t :: ts) with
                      | Some d -> Some (true, d)
                      | None -> Option.map (fun d -> (false, d)) (corresponds n) in
                    Option.map (fun d -> (c, a, d, t)) d) l in
              let l =
                List.sort ~cmp:(fun (_, _, (t1, d1), _) (_, _, (t2, d2), _) ->
                  if t1 && not t2 then -1
                  else if t2 && not t1 then 1
                  else compare d1 d2) l in
              Enum.map (fun (c, a, _, t) -> (t, (a, c))) (List.enum l))) in
          let exact_match l =
            Enum.filter_map (fun (c, a, n, t, ts) ->
              if PSet.mem a already_chosen then
                None
              else if List.exists exact (t :: ts) then
                Some (t, (a, c))
              else None) (List.enum l) in
          let exact_name_match l =
            Enum.filter_map (fun (c, a, n, t, ts) ->
              if PSet.mem a already_chosen then
                None
              else if exact t then
                Some (t, (a, c))
              else None) (List.enum l) in
          let enum =
            Enum.concat (List.enum [
                exact_name_match internal_cons ;
                exact_match main_cons ;
                get_partial_enum main_cons ;
                get_partial_enum internal_cons
              ]) in
          let l = extract_enum enum in
          List.sort_uniq (fun e1 e2 -> - compare (fst e1) (fst e2)) l in
        let attributes =
          List.map (get_responsible_list_infos functions) current in
        let getrec =
          (* This reference is frustrating: I could not find another way to make
           * the compiler accept the recursion in this case. *)
          ref (fun _ -> List.map snd attributes) in
        let node =
          let proposed =
            try
              let (_, _, _, t, _) = Utils.select_any main_cons in
              get_translation "forExample" ^ " " ^ t
            with Utils.EmptyList -> "" in
          IO.createResponsiveListInput attributes proposed
            (pick_list internal_cons main_cons getrec) in
        getrec := (fun _ -> List.map snd (node.IO.get ())) ;
        node in
      let (attributeTable, attributeBlock) =
        let table =
          List.map (fun infos ->
            (IO.createTextInput infos.name,
             create_responsive_list attribute_functions
               attribute_internal_cons attribute_main_cons infos.attributes)) player_information in
        (table,
         InOut.FoldableBlock (false, get_translation "stepAttributes",
           InOut.Div (InOut.Normal, [
               InOut.P [
                   InOut.Text (get_translation "attributeExplanation") ;
                   InOut.Text (get_translation "setAttribute") ;
                   InOut.Text (get_translation "generatorWillDealWithThese")
                 ] ;
               InOut.Div (InOut.Centered, [
                   InOut.Table (["table"],
                                [(InOut.Text (get_translation "playerName"), InOut.default) ;
                                 (InOut.Text (get_translation "attributes"), InOut.default) ;
                                 (InOut.Text (get_translation "commands"), InOut.default)],
                                List.map (fun (name, attributes) ->
                                  ([], [
                                     (InOut.Node name.IO.node, InOut.default) ;
                                     (InOut.Node attributes.IO.node, InOut.default) ;
                                     (InOut.LinkContinuation (false, InOut.Simple,
                                        get_translation "resetAttribute", fun _ ->
                                          attributes.IO.set []), InOut.default)
                                   ])) table)
                 ])
             ]))) in
      let (nameTable, nameBlock) =
        let (changingNames, bunchNames) =
          let lg = get_language parameters in
          let (default, non_default) = List.partition (fun g -> Names.is_default g lg) names in
          let names = default @ non_default in
          let l =
            Utils.list_map_filter (fun g ->
              let tr = Names.translate g in
              Option.map (fun txt -> (txt, g))
                (Translation.translate tr lg ())) names in
          let create _ = IO.createListInput l in
          (create (), create ()) in
        IO.synchroniseListInput changingNames bunchNames ;
        let (bunchResult, bunchResultSet) = IO.createTextOutput "" in
        let bunchNumber = IO.createNumberInput ~min:1 (max 100 (3 * parameters.player_number)) in
        let table =
          List.map (fun infos -> IO.createTextInput infos.name) player_information in
        List.iter2 (fun name (name', _) -> IO.synchronise name name') table attributeTable ;
        (table,
         InOut.FoldableBlock (false, get_translation "stepNames",
           InOut.Div (InOut.Normal, [
               InOut.P [
                   InOut.Text (get_translation "changingNamesManually") ;
                   InOut.Text (get_translation "changingNamesAutomatically") ;
                   InOut.Node changingNames.IO.node ;
                   InOut.LinkContinuation (true,  InOut.Button false, get_translation "changeNames",
                     fun _ ->
                       match changingNames.IO.get () with
                       | None -> ()
                       | Some (_, gen) ->
                         ignore (List.fold_left (fun avoid (nameNode, attributesNode) ->
                           if nameNode.IO.locked () then
                             avoid
                           else
                             (** As the generator contains external data, one can hardly assume
                              * that it can produce infinitely many different names.
                              * We are thus stuck to just generate new ones until a really new
                              * one appears. **)
                             let (name, attributes) =
                               let attributes =
                                 let l = attributesNode.IO.get () in
                                 PSet.from_list (List.map (fun (_, (_, c)) -> c) l) in
                               let rec aux fuel =
                                 let (name, attributes) = Names.generate gen attributes in
                                 match fuel with
                                 | 0 -> (name, attributes)
                                 | n ->
                                   if PSet.mem name avoid then
                                     aux (fuel - 1)
                                   else (name, attributes) in
                               aux 100 in
                             nameNode.IO.set name ;
                             let attributes =
                               let old_attributes = attributesNode.IO.get () in
                               let new_attribute =
                                 let set = PSet.from_list old_attributes in
                                 fun a -> not (PSet.mem a set) in
                               let attributes =
                                 List.map (get_responsible_list_infos attribute_functions)
                                   attributes in
                               List.filter new_attribute attributes @ old_attributes in
                             attributesNode.IO.set attributes ;
                             PSet.add name avoid) PSet.empty attributeTable))
                 ] ;
               InOut.P [ InOut.Text (get_translation "lockNameDescription") ] ;
               InOut.P [ InOut.Text (get_translation "nameInteractions") ] ;
               InOut.Div (InOut.Centered, [
                   InOut.Table (["table"],
                                [(InOut.Text (get_translation "playerName"), InOut.default) ;
                                 (InOut.Text (get_translation "commands"), InOut.default)],
                                List.map (fun node ->
                                  let switch =
                                    IO.createSwitch "" None
                                      (Some (get_translation "unlockName"))
                                      (Some (get_translation "lockName"))
                                      false in
                                  switch.IO.onChange (function
                                    | true -> node.IO.lock ()
                                    | false -> node.IO.unlock ()) ;
                                  ([], [
                                      (InOut.Node node.IO.node, InOut.default) ;
                                      (InOut.Node switch.IO.node, InOut.default)
                                    ])) table)
                 ]) ;
               InOut.P [
                   InOut.Text (get_translation "bunchOfNames") ;
                   InOut.Node bunchNames.IO.node ;
                   InOut.Node bunchNumber.IO.node ;
                   InOut.LinkContinuation (true,  InOut.Button false,
                     get_translation "generateBunchOfNames",
                     fun _ ->
                       match bunchNames.IO.get () with
                       | None -> ()
                       | Some (_, gen) ->
                         let names =
                           let rec aux acc = function
                             | 0 -> acc
                             | n ->
                               let name = fst (Names.generate gen PSet.empty) in
                               aux (name :: acc) (n - 1) in
                           aux [] (bunchNumber.IO.get ()) in
                         bunchResultSet (print_list (get_translation "and") names ^ ".")) ;
                 ] ;
               InOut.P [ InOut.Node bunchResult ]
             ]))) in
      let (contactTable, contactBlock) =
        let table =
          List.mapi (fun c infos ->
            (IO.createTextInput infos.name,
             List.map (fun c' ->
               if c = c' then
                 None
               else
                 Some (
                   let contacts =
                     match List.nth_opt infos.contacts c' with
                     | None -> []
                     | Some l -> l in
                   create_responsive_list contact_functions
                     contact_internal_cons contact_main_cons contacts
                 )) all_players)) player_information in
        let header =
          List.map (fun infos -> IO.createTextInput infos.name) player_information in
        List.iter2 IO.synchronise nameTable header ;
        let header =
          (InOut.Text (get_translation "contacts"), InOut.default)
           :: List.map (fun node ->
                (InOut.Node node.IO.node, InOut.default)) header
           @ [(InOut.Text (get_translation "commands"), InOut.default)] in
        let hsettings = { InOut.default with InOut.classes = ["table-hheader"] } in
        let cellNA = (InOut.Space, { InOut.default with classes = ["cellNA"] }) in
        List.iter2 (fun name (name', _) -> IO.synchronise name name') nameTable table ;
        (table,
         InOut.FoldableBlock (false, get_translation "stepContacts",
           InOut.Div (InOut.Normal, [
               InOut.P [
                   InOut.Text (get_translation "contactExplanation") ;
                   InOut.Text (get_translation "setContact") ;
                   InOut.Text (get_translation "generatorWillDealWithThese")
                 ] ;
               InOut.Div (InOut.Centered, [
                   InOut.Table (["table"], header,
                                List.map (fun (name, contacts) ->
                                    ([],
                                     (InOut.Node name.IO.node, hsettings)
                                     :: List.map (function
                                          | None -> cellNA 
                                          | Some node ->
                                            (InOut.Node node.IO.node, InOut.default)) contacts
                                     @ [(InOut.LinkContinuation (false, InOut.Simple,
                                           get_translation "resetContactLine", fun _ ->
                                             List.iter (function
                                               | None -> ()
                                               | Some node -> node.IO.set []) contacts),
                                         InOut.default)]))
                                  table
                                @ [([],
                                   (InOut.Text (get_translation "commands"), hsettings)
                                   :: List.mapi (fun i _ ->
                                        (InOut.LinkContinuation (false, InOut.Simple,
                                           get_translation "resetContactColumn", fun _ ->
                                             List.iter (fun (_, contacts) ->
                                               match Utils.assert_option __LOC__
                                                       (List.nth_opt contacts i) with
                                               | None -> ()
                                               | Some node -> node.IO.set []) table),
                                         InOut.default)) table
                                   @ [cellNA])])
                 ])
             ]))) in
      let (complexityDifficultyTable, complexityDifficultyBlock) =
        let table =
          List.map (fun infos ->
            (IO.createTextInput infos.name,
             IO.createNumberInput infos.complexity,
             IO.createNumberInput infos.difficulty)) player_information in
        List.iter2 (fun name (name', _, _) -> IO.synchronise name name') nameTable table ;
        (table,
         InOut.FoldableBlock (false, get_translation "stepComplexityDifficulty",
           InOut.Div (InOut.Normal, [
               InOut.P [ InOut.Text (get_translation "complexityDifficultyExplanation") ] ;
               InOut.List (true, [
                   InOut.Text (get_translation "lowComplexityLowDifficulty") ;
                   InOut.Text (get_translation "lowComplexityHighDifficulty") ;
                   InOut.Text (get_translation "highComplexityLowDifficulty") ;
                   InOut.Text (get_translation "highComplexityHighDifficulty")
                 ]) ;
               InOut.P [
                   InOut.Text (get_translation "complexityDifficultyPrefilled") ;
                   InOut.Text (get_translation "changeThisTable")
                 ] ;
               InOut.Div (InOut.Centered, [
                   InOut.Table (["table"],
                                [(InOut.Text (get_translation "playerName"), InOut.default) ;
                                 (InOut.Text (get_translation "complexity"), InOut.default) ;
                                 (InOut.Text (get_translation "difficulty"), InOut.default)],
                                List.map (fun (name, complexity, difficulty) ->
                                  ([], [
                                     (InOut.Node name.IO.node, InOut.default) ;
                                     (InOut.Node complexity.IO.node, InOut.default) ;
                                     (InOut.Node difficulty.IO.node, InOut.default)
                                   ])) table)
                 ])
             ]))) in
      IO.print_block (InOut.List (false,
        [nameBlock ; attributeBlock ; contactBlock ; complexityDifficultyBlock])) ;
      next_button ~nextText:"startGeneration" w parameters (fun _ ->
        { parameters with
            player_information =
              Utils.list_map4 (fun name (_, attributes) (_, contacts)
                    (_, complexity, difficulty) -> {
                  name = name.IO.get () ;
                  complexity = complexity.IO.get () ;
                  difficulty = difficulty.IO.get () ;
                  attributes = List.map (fun (_, (_, c)) -> c) (attributes.IO.get ()) ;
                  contacts =
                    List.map (function
                      | None -> []
                      | Some node -> List.map (fun (_, (_, c)) -> c) (node.IO.get ())) contacts
                }) nameTable attributeTable contactTable complexityDifficultyTable
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
          Array.of_list (List.map (fun infos -> {
              State.complexity = infos.complexity ;
              State.difficulty = infos.difficulty
            }) parameters.player_information) in
        let state = State.create_state parameters.player_number in
        let (state, diff) =
          let diff = Element.empty_difference in
          Utils.assert_option __LOC__
            (Utils.list_fold_lefti (fun c stdiff infos ->
                Utils.if_option stdiff (fun (state, diff) ->
                  let c = Id.from_array c in
                  Utils.if_option
                    (Element.apply_attributes (Driver.get_constructor_maps data)
                      state c infos.attributes)
                    (fun (state, diff') ->
                      let diff = Element.merge_attribute_differences diff diff' in
                      Utils.list_fold_lefti (fun c' stdiff contacts ->
                          Utils.if_option stdiff (fun (state, diff) ->
                            let c' = Id.from_array c' in
                            Utils.apply_option
                              (Element.apply_contacts (Driver.get_constructor_maps data)
                                state c c' contacts)
                              (fun (state, diff') ->
                                let diff = Element.merge_attribute_differences diff diff' in
                                (state, diff))))
                        (Some (state, diff)) infos.contacts)))
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
          InOut.LinkExtern (InOut.Simple, get_translation "there", webpage_link) ]) ;
      let exportButtons =
        List.map (fun (name, descr) ->
            let node =
              IO.createSwitch (get_translation "generateAs" ^ " " ^ get_translation name)
                (Some (get_translation descr)) None None
                (PSet.mem name parameters.chosen_productions) in
            (name, node))
          (("html", "htmlDescription")
           :: List.map (fun (name, descr, _, _, _, _) -> (name, descr))
                Export.all_production) in
      IO.print_block (InOut.Div (InOut.Normal, [
          InOut.P [ InOut.Text (get_translation "exportPossibilities") ] ;
          InOut.List (false,
            List.map (fun (_, node) -> InOut.Node node.IO.node) exportButtons)
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
              List.fold_left (fun s (name, node) ->
                if node.IO.get () then PSet.add name s else s) PSet.empty exportButtons })
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
          Export.names = List.map (fun infos -> infos.name) parameters.player_information ;
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
        let node =
          IO.createSwitch (get_translation "changeStyles") None
            (Some (get_translation "changeStylesOff"))
            (Some (get_translation "changeStylesOn")) false in
        node.IO.onChange (fun _ ->
          (if !get () then IO.set_printing_mode
           else IO.unset_printing_mode) ()) ;
        get := node.IO.get ;
        node in
      let (results, error) =
        let (results, error) =
          List.fold_left (fun (l, e) (name, descr, mime, ext, native, f) ->
            try
              let file = f estate in
              let fileName = "murder" ^ if ext = "" then "" else ("." ^ ext) in
              let node =
                InOut.Div (InOut.Inlined, [
                    InOut.LinkFile (InOut.Button false,
                                    get_translation "downloadAs" ^ " " ^ get_translation name,
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
        IO.print_block (InOut.P [ InOut.Node switch_printing_mode.IO.node ]) ;
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
          InOut.LinkExtern (InOut.Simple, get_translation "there", webpage_link)
        ]) ;
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
          InOut.LinkContinuation (true, InOut.Simple, get_translation "jumpBack",
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
              InOut.LinkExtern (InOut.Simple, there, webpage_issues)
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

