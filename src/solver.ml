
let sort_characters_by_expectation s =
  TODO (* Return the list of characters, the first one being the one
    the farthest away from its expectations, and the last one the
    closest, in the current state s. *)

let solver_step s u =
  match Utils.match_left u with
  | None -> (s, u)
  | Some ((g, _), u) ->
    match sort_characters_by_expectation s with
    | [] -> (* No character! *) (s, u)
    | c :: cl ->
      let (rl, hl, uf, g) = Generator.apply g s c cl in
      let is_compatible = TODO in
      let s =
        if is_compatible then
          TODO (* apply everything *)
        else s in
      let was_a_success =
        (rl <> [] || hl <> []) && is_compatible in
      let u =
        match uf with
        | Generator.Reuse_as_soon_as_possible ->
          if was_a_success then
            Utils.add_left (g, true) u
          else Utils.add_right (g, false) u
        | Generator.Reuse_later ->
          Utils.add_right (g, was_a_success) u
        | Generator.Do_not_reuse -> u in
      (s, u)

let rec iterate_step s u =
  if Utils.for_all (fun (_, b) -> not b) u then
    s (* All generators have been called and failed to produce anything. *)
  else
    let (s, u) = solver_step s u in
    iterate_step s u

let rec solver s = function
  | [] -> s
  | u :: ul ->
    let u = Utils.two_direction_list_from_list (List.map (fun g -> (g, true)) u) in
    solver (iterate_step s u) ul

