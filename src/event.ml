
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
    event_kinds : kind PSet.t
  }

