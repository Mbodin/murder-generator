
open Libutils
open ExtList

let get_file fileName =
  let fileName = "../" ^ fileName in
  print_endline ("Reading file " ^ fileName) ;
  Std.input_file fileName

let _test_relations _ =
  let open Relation in
  let b = [ Neutral; Hate; Trust; Chaotic; Undetermined; Avoidance ] in
  let l =
    List.map (fun r -> Basic r, false) b @
    List.concat (List.map (fun r1 -> List.map (fun r2 -> Asymmetrical (r1, r2), false) b) b) @
    List.concat (List.map (fun r1 -> List.map (fun r2 -> Explosive (Basic r1, Basic r2), false) b) b)
  in
  for _ = 0 to 5 do
    let r1 = Utils.select_any l in
    let r2 = Utils.select_any l in
    let r = compose r1 r2 in
    print_string (to_string r1) ;
    print_string " composed with " ;
    print_string (to_string r2) ;
    print_string " returns " ;
    print_string (to_string r) ;
    print_string " (final complexity of " ;
    print_int (complexity r) ;
    print_string " and difficulty of " ;
    print_int (difficulty r) ;
    print_string ")." ;
    print_newline ()
  done

let test_translations _ =
  let f = "translations.json" in
  let content = get_file f in
  let (translations, languages) = Translation.from_json f content in
  let ok = ref true in
  let translate key lg =
    match Translation.translate translations lg key with
    | Some str -> str
    | None ->
      print_endline ("Missing translation of `" ^ key ^ "' for language "
                     ^ Translation.iso639 lg ^ ".") ;
      ok := false ; "" in
  let used =
    List.concat (List.map (fun (id, descr, _, _, _, _) ->
      id :: descr :: []) Export.all_production)
    @ UsedTranslations.used in
  List.iter (fun lg ->
    List.iter (fun key ->
      ignore (translate key lg)) used) languages ;
  if not !ok then
    failwith "Error: There were some missing translations." ;
  languages

let test_name_generation languages constructor_maps =
  print_endline ("Number of name files: " ^
                 string_of_int (List.length NameFiles.files)) ;
  let read_file fileName =
    let file = get_file fileName in
    let gen = Names.import constructor_maps file in
    ignore (Names.generate gen PSet.empty) ;
    (fileName, gen) in
  let generators = List.map read_file NameFiles.files in
  List.iter (fun lg ->
    let count = ref 0 in
    let limit = 5 in
    let has_default =
      List.fold_left (fun has_default (fileName, gen) ->
        let tr = Names.translate gen in
         if Translation.translate tr lg () = None then (
           incr count ;
           if !count <= limit then
             print_endline ("Warning: Missing translation of `" ^ fileName
                            ^ "' for language " ^ Translation.iso639 lg ^ ".")) ;
        has_default || Names.is_default gen lg) false generators in
    if !count > limit then
      print_endline ("Skipping further occurrences of missing translations"
                     ^ " for language " ^ Translation.iso639 lg
                     ^ " (" ^ string_of_int !count ^ " in total).") ;
    if not has_default then
      print_endline ("Warning: No default name generation for language "
                     ^ Translation.iso639 lg ^ ".")) languages

let test_parser languages =
  print_endline ("Number of murder files: " ^
                 string_of_int (List.length MurderFiles.files)) ;
  let read_file f =
    let f = "../" ^ f in
    print_endline ("Reading file " ^ f) ;
    let buf = Lexing.from_channel (open_in f) in
    Driver.parse_lexbuf f buf in
  let asts = List.map read_file MurderFiles.files in
  let i =
    List.fold_left Driver.prepare_declarations Driver.empty_intermediary asts in
  if not (Driver.is_intermediary_final i) then (
    let missing_param print name set =
      let count = ref 0 in
      let limit = 5 in
      PSet.iter (fun o ->
        incr count ;
        if !count <= limit then
          print_endline ("Missing " ^ name ^ ": " ^ print o)) set ;
      if !count > limit then
        print_endline ("Skipping further occurrences"
                       ^ " (" ^ string_of_int !count ^ " in total).") in
    let missing = missing_param Utils.id in
    missing "category" (Driver.categories_to_be_defined i) ;
    missing "event" (Driver.events_to_be_defined i) ;
    let (attributes, contacts) = Driver.attributes_to_be_defined i in
    missing "attribute" attributes ;
    missing "contact" contacts ;
    let (attributes, contacts) = Driver.constructors_to_be_defined i in
    let missing_constructor name =
      missing_param (fun (a, c) ->
        c ^ " (for "  ^ name ^ ": " ^ a ^ ")") (name ^ " constructor") in
    missing_constructor "attribute" attributes ;
    missing_constructor "contact" contacts ;
    missing_param (fun (lg, tag) ->  tag ^ " for language: " ^ lg) "tag"
      (Driver.tags_to_be_defined i) ;
    failwith "Non final intermediary!" ) ;
  let s = Driver.parse i in
  let translations = Driver.get_translations s in
  let categories = Driver.all_categories s in
  let constructor_maps = Driver.get_constructor_maps s in
  let constructors_player =
    Attribute.PlayerAttribute.all_constructors constructor_maps.Attribute.player in
  let constructors_contact =
    Attribute.ContactAttribute.all_constructors constructor_maps.Attribute.contact in
  let elements = Driver.elements s in
  List.iter (fun lg ->
    let count = ref 0 in
    let limit = 5 in
    let warn =
      let seen = ref PSet.empty in fun t name ->
      if not (PSet.mem (t, name) !seen) then (
        seen := PSet.add (t, name) !seen ;
        incr count ;
        if !count <= limit then
          prerr_endline ("Warning: Missing translation for language "
                         ^ Translation.iso639 lg ^ " for " ^ t
                         ^ " `" ^ name ^ "'.")) in
    List.iter (fun c ->
      let t = translations.Translation.category in
      if Translation.translate t lg c = None then
        warn "category" (Translation.force_translate t Translation.generic c) ;
      let td = translations.Translation.category_description in
      if Translation.translate td lg c = None then
        warn "category description"
          (Translation.force_translate t Translation.generic c)) categories ;
    let check_constructors m constructor_attribute get_attribute get_constructor constructors only =
      let ta = translations.Translation.attribute in
      let tc = translations.Translation.constructor in
      let base = PSet.singleton Translation.base in
      List.iter (fun c ->
          let a = Utils.assert_option __LOC__ (constructor_attribute m c) in
          if only a c then (
            let a = get_attribute a in
            if Translation.translate ta lg a = None then
              warn "attribute" (Translation.force_translate ta Translation.generic a) ;
            let c = get_constructor c in
            if Translation.gtranslate tc lg c base = None then
              warn "attribute" (fst (Translation.gforce_translate tc Translation.generic c base))))
        constructors in
    check_constructors
      constructor_maps.Attribute.player
      Attribute.PlayerAttribute.constructor_attribute
      (fun a -> Attribute.PlayerAttribute a)
      (fun c -> Attribute.PlayerConstructor c)
      constructors_player
      (fun a _ -> a <> Attribute.object_type) ;
    check_constructors
      constructor_maps.Attribute.contact
      Attribute.ContactAttribute.constructor_attribute
      (fun a -> Attribute.ContactAttribute a)
      (fun c -> Attribute.ContactConstructor c)
      constructors_contact
      (fun _ _ -> true) ;
    let n =
      PMap.foldi (fun id e n ->
        let name = Utils.assert_option __LOC__ (Driver.get_element_name s id) in
        let ok =
          Utils.list_fold_lefti (fun id ok e ->
            let (n, t) = e.Events.translation in
            List.fold_left (fun ok i ->
              ok &&
                if Translation.stranslate t lg (fun _ -> PSet.empty)
                     (fun _ _ -> Some ("", PSet.empty))
                     i (PSet.singleton Translation.base) = None then (
                  warn "element" (name ^ "#" ^ string_of_int id) ;
                  false
                ) else true) ok (Utils.seq n)) true e.Element.events in
        if ok then 1 + n else n) elements 0 in
    if !count > limit then
      print_endline ("Skipping further occurrences of missing translations"
                     ^ " for language " ^ Translation.iso639 lg
                     ^ " (" ^ string_of_int !count ^ " in total).") ;
    print_endline ("Number of available elements for language "
                   ^ Translation.iso639 lg ^ ": " ^ string_of_int n)) languages ;
  constructor_maps

let%test_unit _ =
  let languages = test_translations () in
  let constructor_maps = test_parser languages in
  test_name_generation languages constructor_maps.Attribute.player

