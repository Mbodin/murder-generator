
type character = Id.t

(** There are two kinds of kinds: user defined and kinds from attributes. **)
type kind =
  | Kind of Id.t (** A user-defined kind. **)
  | Attribute of Attribute.attribute

let kind_names = ref (Id.map_create ())

let kind_of_string name =
  let (k, m) = Id.map_insert_t !kind_names name in
  kind_names := m ;
  Kind k

let kind_of_attribute a = Attribute a

type event_type =
    | For_life_event
    | Long_term_event
    | Medium_term_event
    | Short_term_event
    | Very_short_term_event
    | Immediate_event

type t = {
    event_type : event_type ;
    event_attendees : character list ;
    event_kinds : kind PSet.t ;
    constraints_none : (kind * character) PSet.t * (kind * character) PSet.t ;
    constraints_some : (kind * character) PSet.t * (kind * character) PSet.t
  }

type partial = {
    partial_type : event_type ;
    partial_kinds : kind PSet.t ;
    partial_attendees : int PSet.t ;
    constraints_none : (int, kind PSet.t * kind PSet.t) PMap.t ;
    constraints_some : (int, kind PSet.t * kind PSet.t) PMap.t
  }

let partially_instantiate i c e =
  let get_constraints m =
    let (before, after) =
      try PMap.find i m
      with Not_found -> (PSet.empty, PSet.empty) in
    let add k = (k, c) in
    (PSet.map add before, PSet.map add after) in
  if PSet.mem i e.partial_attendees then
    Some {
      event_type = e.partial_type ;
      event_attendees = [c] ;
      event_kinds = e.partial_kinds ;
      constraints_none = get_constraints e.constraints_none ;
      constraints_some = get_constraints e.constraints_some
    }
  else None

let instantiate inst e =
  let convert m =
    PMap.foldi (fun i (beforei, afteri) (before, after) ->
      let c = inst.(i) in
      let add k = (k, c) in
      (PSet.merge (PSet.map add beforei) before,
       PSet.merge (PSet.map add afteri) after)) m (PSet.empty, PSet.empty) in {
    event_type = e.partial_type ;
    event_attendees =
      List.map (fun i -> inst.(i)) (PSet.to_list e.partial_attendees) ;
    event_kinds = e.partial_kinds ;
    constraints_none = convert e.constraints_none ;
    constraints_some = convert e.constraints_some
  }

