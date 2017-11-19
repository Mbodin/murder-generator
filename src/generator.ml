
type character = Utils.idt

type relation_state =
  Relations.t array array

exception SelfRelation

let create_relation_state n =
  Array.init (n - 1) (fun i ->
    Array.make (n - 1 - i) (Relations.Basic Relations.Neutral))

let rec get_relation_state a c1 c2 =
  let c1 = Utils.idt_to_array c1 in
  let c2 = Utils.idt_to_array c2 in
  if c1 = c2 then
    raise SelfRelation
  else if c1 > c2 then
    Relations.reverse a.(c2).(c1)
  else a.(c1).(c2)

let write_relation_state a c1 c2 r =
  let c1 = Utils.idt_to_array c1 in
  let c2 = Utils.idt_to_array c2 in
  if c1 = c2 then
    raise SelfRelation
  else if c1 > c2 then
    a.(c2).(c1) <- Relations.reverse r
  else a.(c2).(c1) <- r

type state =
  relation_state * History.state

let get_relation (a, _) = get_relation_state a

let write_relation (a, _) = write_relation_state a

let create_state n =
  (create_relation_state n, History.create_state n)

type usage_flag =
  | Reuse_as_soon_as_possible
  | Reuse_later
  | Do_not_reuse

type generator =
  state -> character -> character list -> (character * character * Relations.t) list * (character * History.t) list * usage_flag

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
      let (rl, hl, uf) = g s c cl in
      let is_compatible = TODO in
      let s =
        if is_compatible then
          TODO (* apply everything *)
        else s in
      let was_a_success =
        (rl <> [] || hl <> []) && is_compatible in
      let u =
        match uf with
        | Reuse_as_soon_as_possible ->
          if was_a_success then
            Utils.add_left (g, true) u
          else Utils.add_right (g, false) u
        | Reuse_later ->
          Utils.add_right (g, was_a_success) u
        | Do_not_reuse -> u in
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

