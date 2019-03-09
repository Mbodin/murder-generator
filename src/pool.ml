
type element = Utils.Id.t

(** These two maps store how elements are linked to attributes:
  * - [all_elements] is a map from elements to lists of attributes
  *   (the attributes that the elements provide);
  * - [element_attribute] is a map from attributes to lists of
  *   elements (the elements that provide this attribute).
  * They are not supposed to be changed once pools are created. **)
type global = {
    all_elements : (element, State.attribute list) PMap.t ;
    element_attribute : (State.attribute, element list) PMap.t
  }

let empty_global = {
    all_elements = PMap.empty ;
    element_attribute = PMap.empty ;
  }

(** Returns the list of attributes associated to an element. **)
let get_attributes g e =
  try PMap.find e g.all_elements
  with Not_found -> []

(** Internal function for function reading a global when being changed. **)
let get_elements_from_element_attribute element_attribute a =
  try PMap.find a element_attribute
  with Not_found -> []

(** Returns the list of elements associated to an attribute. **)
let get_elements g a =
  get_elements_from_element_attribute g.element_attribute a

let register_element g e l =
  let l = Utils.shuffle l in {
      all_elements = PMap.add e l g.all_elements ;
      element_attribute =
        List.fold_left (fun element_attribute a ->
          let l = get_elements_from_element_attribute element_attribute a in
          let l = if List.mem e l then l else e :: l in
          PMap.add a l element_attribute) g.element_attribute l
  }

let unregister_element g e =
  let l = get_attributes g e in {
    all_elements = PMap.remove e g.all_elements ;
    element_attribute =
      List.fold_left (fun element_attribute a ->
        let l = get_elements_from_element_attribute element_attribute a in
        let l = List.filter ((<>) e) l in
        PMap.add a l element_attribute) g.element_attribute l
  }

let filter_global g f =
  PMap.foldi (fun e _ g ->
    if f e then g
    else unregister_element g e) g.all_elements g

type t = {
    current_elements : element Utils.PSet.t
      (** The set of the current elements in the pool. **) ;
    pool : element Utils.BidirectionalList.t
      (** The pool.
       * Each element is present at most once in the pool. **) ;
    filtered_out_attributes : State.attribute Utils.PSet.t
      (** Attributes meant to be removed from the pool. **) ;
    global : global
      (** The associated global values. **)
  }

let empty g = {
    current_elements = Utils.PSet.empty ;
    pool = Utils.BidirectionalList.from_list [] ;
    filtered_out_attributes = Utils.PSet.empty ;
    global = g
  }

(** To avoid a costly iteration over the pool when removing elements relative
 * to a particular attribute, the removing is done lazily.
 * When removing an attribute, it is only stored in the set
 * [filtered_out_attributes].
 * This function states whether an element is meant to be ignored by this
 * mechanism. **)
let to_be_ignored p e =
  List.exists (fun a ->
    Utils.PSet.mem a p.filtered_out_attributes) (get_attributes p.global e)

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
    filtered_out_attributes = Utils.PSet.empty ;
    global = p.global
  }

(** We often don’t want to fully normalize a pool, but to only ensure that
 * the next element is not to be ignored.
 * The following function deals with that. **)
let partial_normalize p =
  let rec aux l =
    match Utils.BidirectionalList.match_left l with
    | None -> empty p.global
    | Some (e, l') ->
      if to_be_ignored p e then aux l'
      else { p with pool = l } in
  aux p.pool

let is_empty p =
  let p = partial_normalize p in
  Utils.BidirectionalList.is_empty p.pool

let length p =
  let p = normalize p in
  Utils.BidirectionalList.length p.pool

let quick_length p =
  Utils.BidirectionalList.length p.pool

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
  if Utils.PSet.mem e p.current_elements then p
  else {
    current_elements = Utils.PSet.add e p.current_elements ;
    pool = Utils.BidirectionalList.add_right p.pool e ;
    filtered_out_attributes = p.filtered_out_attributes ;
    global = p.global
  }

let rec pop p =
  match Utils.BidirectionalList.match_left p.pool with
  | None -> (None, empty p.global)
  | Some (e, l) ->
    let p' = {
        current_elements = Utils.PSet.remove e p.current_elements ;
        pool = l ;
        filtered_out_attributes = p.filtered_out_attributes ;
        global = p.global
      } in
    if to_be_ignored p e then pop p'
    else (Some e, partial_normalize p')

let pick p =
  let (r, p) = pop p in
  match r with
  | None -> (r, p)
  | Some e -> (Some e, add p e)

let filter p f =
  let rec aux = function
  | [] -> empty p.global
  | e :: l -> if f e then add (aux l) e else aux l
  in aux (Utils.BidirectionalList.to_list p.pool)

(** Filters a pool [p] from a function [f] taking the attributes of each element
 * and a global attribute.
 * The function [f] is meant to be instantiated either with
 * [fun a l -> List.mem a l] or [fun a l -> not (List.mem a l)]. **)
let filter_attribute f p a =
  filter p (fun e -> f a (get_attributes p.global e))

let restrict = filter_attribute (fun a l -> List.mem a l)

(** A naive implementation of [filter_out] could be
 * [filter_attribute (fun a l -> not (List.mem a l))].
 * We here chose to be lazy (assuming that the pool is large)
 * and simply rely on the [filtered_out_attributes] mechanism. **)
let filter_out p a =
  { p with filtered_out_attributes = Utils.PSet.add a p.filtered_out_attributes }

let add_attribute p a =
  List.fold_left add p (Utils.shuffle (get_elements p.global a))

