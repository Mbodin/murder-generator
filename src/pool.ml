
type element = Utils.Id.t

(** These two maps store how elements are linked to attributes:
  * - [all_elements] is a map from elements to lists of attributes
  *   (the attributes that the elements provide);
  * - [element_attribute] is a map from attributes to lists of
  *   elements (the elements that provide this attribute).
  * They are not supposed to be changed once pools are created. **)

let all_elements = ref PMap.empty

let element_attribute = ref PMap.empty

(** Returns the list of attributes associated to an element. **)
let get_attributes e =
  try PMap.find e !all_elements
  with Not_found -> []

(** Returns the list of elements associated to an attribute. **)
let get_elements a =
  try PMap.find a !element_attribute
  with Not_found -> []

let add_element e l =
  all_elements := PMap.add e l !all_elements ;
  List.iter (fun a ->
    let l = get_elements a in
    let l = if List.mem e l then l else e :: l in
    element_attribute := PMap.add a l !element_attribute) l

let remove_element e =
  let l = get_attributes e in
  all_elements := PMap.remove e !all_elements ;
  List.iter (fun a ->
    let l = get_elements a in
    let l = List.filter ((<>) e) l in
    element_attribute := PMap.add a l !element_attribute) l

type t = {
    current_elements : element Utils.PSet.t ; (** The set of the current elements in the pool. **)
    pool : element Utils.BidirectionalList.t ; (** The pool. Each element is present at most once in the pool. **)
    filtered_out_attributes : State.attribute Utils.PSet.t (** Attributes meant to be removed from the pool. **)
  }

let empty = {
    current_elements = Utils.PSet.empty ;
    pool = Utils.BidirectionalList.from_list [] ;
    filtered_out_attributes = Utils.PSet.empty
  }

(** To avoid a costly iteration over the pool when removing elements relative
 * to a particular attribute, the removing is done lazily.
 * When removing an attribute, it is only stored in the set [filtered_out_attributes].
 * This function states whether an element is meant to be ignored by this mechanism. **)
let to_be_ignored p e =
  List.exists (fun a ->
    Utils.PSet.is_in a p.filtered_out_attributes) (get_attributes e)

(** The following operation actually performs the removal of the to-be-removed
 * elements of a pool. **)
let normalize p =
  let rec aux = function
    | [] -> (Utils.PSet.empty, [])
    | e :: l ->
      let (s, l) = aux l in
      if to_be_ignored p e then (s, l)
      else (Utils.PSet.add e s, e :: l) in
  let (s, l) = aux (Utils.BidirectionalList.to_list p.pool) in {
    current_elements = s ;
    pool = Utils.BidirectionalList.from_list l ;
    filtered_out_attributes = Utils.PSet.empty
  }

(** We often don’t want to fully normalize a pool, but to only ensure that
 * the next element is not to be ignored.
 * The following function deals with that. **)
let partial_normalize p =
  let rec aux l =
    match Utils.BidirectionalList.match_left l with
    | None -> empty
    | Some (e, l') ->
      if to_be_ignored p e then aux l'
      else { p with pool = l } in
  aux p.pool

let is_empty p =
  let p = partial_normalize p in
  Utils.BidirectionalList.is_empty p.pool

(** The interesting case of [add] is when adding an element which is marked
 * as an element to be ignored: this mean that the previous note stating
 * that this element should be ignored is no longer up to date.
 * To fix this, we have to empty the set of attributes to be ignored.
 * This is done through normalisation.
 * Note that we do not add an element if already present in the pool, to
 * avoid breaking the invariant that elements are present at most once in it. **)
let add p e =
  let p =
    if to_be_ignored p e then normalize p
    else p in
  if Utils.PSet.is_in e p.current_elements then p
  else {
    current_elements = Utils.PSet.add e p.current_elements ;
    pool = Utils.BidirectionalList.add_right p.pool e ;
    filtered_out_attributes = p.filtered_out_attributes
  }

let rec pop p =
  match Utils.BidirectionalList.match_left p.pool with
  | None -> (None, empty)
  | Some (e, l) ->
    let p' = {
        current_elements = Utils.PSet.remove e p.current_elements ;
        pool = l ;
        filtered_out_attributes = p.filtered_out_attributes
      } in
    if to_be_ignored p e then pop p'
    else (Some e, partial_normalize p')

let pick p =
  let (r, p) = pop p in
  match r with
  | None -> (r, p)
  | Some e -> (Some e, add p e)

(** Only keeps in the pool the elements that satisfy [f]. **)
let filter f p a =
  let rec aux = function
  | [] -> empty
  | e :: l ->
    if f a (get_attributes e) then add (aux l) e
    else aux l
  in aux (Utils.BidirectionalList.to_list p.pool)

let restrict = filter (fun a l -> List.mem a l)

(** A naive implementation of [filter_out] could be
 * [filter (fun a l -> not (List.mem a l))].
 * We here chose to be lazy (assuming that the pool is large)
 * and simply rely on the [filtered_out_attributes] mechanism. **)
let filter_out p a =
  { p with filtered_out_attributes = Utils.PSet.add a p.filtered_out_attributes }

let add_attribute p a =
  List.fold_left add p (get_elements a)

