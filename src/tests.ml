
let test_relations =
  let open Relations in
  let b = [ Neutral; Hate; Trust; Chaotic; Undetermined; Avoidance ] in
  let l =
    List.map (fun r -> Basic r, false) b @
    List.concat (List.map (fun r1 -> List.map (fun r2 -> Asymetrical (r1, r2), false) b) b) @
    List.concat (List.map (fun r1 -> List.map (fun r2 -> Explosive (Basic r1, Basic r2), false) b) b)
  in
  for i = 0 to 50 do
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

