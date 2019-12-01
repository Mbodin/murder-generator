
open ExtList

let test_utils _ =
  let test_split_on_char c str =
    assert (List.of_enum (Utils.enum_split_on_char c str) = String.split_on_char c str) in
  test_split_on_char ':' "a:b:c:d" ;
  test_split_on_char ':' ":::" ;
  test_split_on_char ':' "abcd" ;
  test_split_on_char ':' ""

let get_file fileName =
  let file = open_in fileName in
  let rec aux _ =
    try let str = input_line file in str :: aux ()
    with End_of_file -> [] in
  String.concat "\n" (aux ())

let test_date _ =
  let test d =
    assert (Date.compare d (Date.from_rfc2445 (Date.rfc2445 d)) = 0) in
  test Date.now ;
  test (Date.add_years Date.now (Random.int 100 - 50)) ;
  test (Date.add_days Date.now (Random.int 100 - 50)) ;
  test (Date.add_minutes Date.now (Random.int 100 - 50))

let test_pool _ =
  let new_id = Id.new_id_function () in
  let g = Pool.empty_global in
  let e1 = new_id () in
  let e2 = new_id () in
  let e3 = new_id () in
  let g = Pool.register_element g e1 [] in
  let g = Pool.register_element g e2 [] in
  let g = Pool.register_element g e3 [] in
  let p = Pool.empty g in
  print_endline ("is_empty empty = " ^ string_of_bool (Pool.is_empty p)) ;
  print_endline ("pick empty = " ^ match Pool.pick p with None, _ -> "None" | Some _, _ -> "Some") ;
  print_endline ("pop empty = " ^ match Pool.pop p with None, _ -> "None" | Some _, _ -> "Some") ;
  let p = Pool.add p e1 in
  let p = Pool.add p e2 in
  let p = Pool.add p e3 in
  print_endline ("is_empty [e1; e2; e3] = " ^ string_of_bool (Pool.is_empty p)) ;
  let rec foo p =
    let o, p = Pool.pop p in
    match o with
    | None -> "[]"
    | Some _ -> ";" ^ foo p in
  print_endline ("pop* [e1; e2; e3] = " ^ foo p) ;
  let rec bar i p =
    if i = 0 then "-"
    else
      let o, p = Pool.pick p in
      match o with
      | None -> "[]"
      | Some _ -> ";" ^ bar (i - 1) p in
  print_endline ("pick^10 [e1; e2; e3] = " ^ bar 10 p)

let test_relations _ =
  let open Relation in
  let b = [ Neutral; Hate; Trust; Chaotic; Undetermined; Avoidance ] in
  let l =
    List.map (fun r -> Basic r, false) b @
    List.concat (List.map (fun r1 -> List.map (fun r2 -> Asymmetrical (r1, r2), false) b) b) @
    List.concat (List.map (fun r1 -> List.map (fun r2 -> Explosive (Basic r1, Basic r2), false) b) b)
  in
  for i = 0 to 5 do
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
  let f = "web/translations.json" in
  print_endline ("Reading file " ^ f) ;
  let content = Std.input_file f in
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
    print_endline ("Reading file " ^ fileName) ;
    let file = get_file fileName in
    let gen = Names.import constructor_maps file in
    ignore (Names.generate gen PSet.empty) ;
    (fileName, gen) in
  let generators = List.map read_file NameFiles.files in
  List.iter (fun lg ->
    let has_default =
      List.fold_left (fun has_default (fileName, gen) ->
        let tr = Names.translate gen in
         if Translation.translate tr lg () = None then
           print_endline ("Warning: Missing translation of `" ^ fileName
                          ^ "' for language " ^ Translation.iso639 lg ^ ".") ;
        has_default || Names.is_default gen lg) false generators in
    if not has_default then
      print_endline ("Warning: No default name generation for language "
                     ^ Translation.iso639 lg ^ ".")) languages

let test_parser languages =
  print_endline ("Number of murder files: " ^
                 string_of_int (List.length MurderFiles.files)) ;
  let read_file f =
    print_endline ("Reading file " ^ f) ;
    let buf = Lexing.from_channel (open_in f) in
    Driver.parse_lexbuf f buf in
  let asts = List.map read_file MurderFiles.files in
  let i =
    List.fold_left Driver.prepare_declarations Driver.empty_intermediary asts in
  if not (Driver.is_intermediary_final i) then (
    PSet.iter (fun c -> print_endline ("Missing category: " ^ c))
      (Driver.categories_to_be_defined i) ;
    PSet.iter (fun e -> print_endline ("Missing event: " ^ e))
      (Driver.events_to_be_defined i) ;
    let (attributes, contacts) = Driver.attributes_to_be_defined i in
    PSet.iter (fun a -> print_endline ("Missing attribute: " ^ a)) attributes ;
    PSet.iter (fun c -> print_endline ("Missing contact: " ^ c)) contacts ;
    let (attributes, contacts) = Driver.constructors_to_be_defined i in
    PSet.iter (fun (a, n) ->
      print_endline ("Missing attribute constructor: " ^ n
                     ^ " (for attribute: " ^ a ^ ")")) attributes ;
    PSet.iter (fun (c, n) ->
      print_endline ("Missing contact constructor: " ^ n
                     ^ " (for contact: " ^ c ^ ")")) contacts ;
    PSet.iter (fun (lg, tag) ->
        print_endline ("Missing tag: " ^ tag ^ " for language: " ^ lg))
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
    let warn =
      let seen = ref PSet.empty in fun t name ->
      if not (PSet.mem (t, name) !seen) then (
        seen := PSet.add (t, name) !seen ;
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
    let check_constructors m constructor_attribute get_attribute get_constructor constructors =
      let ta = translations.Translation.attribute in
      let tc = translations.Translation.constructor in
      let base = PSet.singleton Translation.base in
      List.iter (fun c ->
          let a = Utils.assert_option __LOC__ (constructor_attribute m c) in
          let a = get_attribute a in
          if Translation.translate ta lg a = None then
            warn "attribute" (Translation.force_translate ta Translation.generic a) ;
          let c = get_constructor c in
          if Translation.gtranslate tc lg c base = None then
            warn "attribute" (fst (Translation.gforce_translate tc Translation.generic c base)))
        constructors in
    check_constructors
      constructor_maps.Attribute.player
      Attribute.PlayerAttribute.constructor_attribute
      (fun a -> Attribute.PlayerAttribute a)
      (fun c -> Attribute.PlayerConstructor c)
      constructors_player ;
    check_constructors
      constructor_maps.Attribute.contact
      Attribute.ContactAttribute.constructor_attribute
      (fun a -> Attribute.ContactAttribute a)
      (fun c -> Attribute.ContactConstructor c)
      constructors_contact ;
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
    print_endline ("Number of available elements for language "
                   ^ Translation.iso639 lg ^ ": " ^ string_of_int n)) languages ;
  constructor_maps

let main =
  test_utils () ;
  let languages = test_translations () in
  let constructor_maps = test_parser languages in
  test_name_generation languages constructor_maps.Attribute.player ;

