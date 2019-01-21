
type element = Utils.Id.t

type attribute =
  | PlayerAttribute of State.PlayerAttribute.attribute
  | ContactAttribute of State.ContactAttribute.attribute

(** Elements are stored in four different places:
  * - [all_elements] is a map from elements to lists of attributes
  *   (the attributes that the elements provide);
  * - [element_attribute] is a map from attributes to lists of
  *   elements (the elements that provide this attribute);
  * - [current_elements] is a set corresponding to the current
  *   elements in the pool;
  * - [pool] is the file of the pool.
  * The first two are not supposed to be changed once pools are
  * created, the other two are included in the [t] type for pools. **)

let all_elements = ref PMap.empty

let element_attribute = ref PMap.empty

let add_element e l =
  all_elements := PMap.add e l !all_elements ;
  List.iter (fun a ->
    let l =
      try PMap.find a !element_attribute
      with Not_found -> [] in
    element_attribute := PMap.add a (e :: l) !element_attribute) l

let remove_element e =
  let l =
    try PMap.find e !all_elements
    with Not_found -> [] in
  all_elements := PMap.remove e !all_elements ;
  List.iter (fun a ->
    let l =
      try PMap.find a !element_attribute
      with Not_found -> [] in
    let l = List.filter ((<>) e) l in
    element_attribute := PMap.add a l !element_attribute) l

type t = {
    current_elements : element Utils.PSet.t ; (** The set of the current elements in the pool. **)
    pool : element Utils.BidirectionalList.t (** The pool. Each element is present at most once in the pool. **)
  }

let empty = {
    current_elements = Utils.PSet.empty ;
    pool = Utils.BidirectionalList.from_list []
  }

let is_empty p =
  Utils.BidirectionalList.is_empty p.pool

let pop p =
  match Utils.BidirectionalList.match_left p.pool with
  | None -> (None, empty)
  | Some (e, l) ->
    (Some e, {
       current_elements = Utils.PSet.remove e p.current_elements ;
       pool = l
     })

let add p e =
  if Utils.PSet.is_in e p.current_elements then p
  else {
    current_elements = Utils.PSet.add e p.current_elements ;
    pool = Utils.BidirectionalList.add_right p.pool e
  }

let pick p =
  let (r, p) = pop p in
  match r with
  | None -> (r, p)
  | Some e -> (Some e, add p e)

let restrict f p a =
  let rec aux = function
  | [] -> empty
  | e :: l ->
    let al =
      try PMap.find e !all_elements
      with Not_found -> [] in
    if f a al then add (aux l) e
    else aux l
  in aux (Utils.BidirectionalList.to_list p.pool)

let restrict_only = restrict (fun a l -> List.mem a l)
let restrict_but = restrict (fun a l -> not (List.mem a l))

let add_attribute p a =
  let l =
    try PMap.find a !element_attribute
    with Not_found -> [] in
  List.fold_left add p l

