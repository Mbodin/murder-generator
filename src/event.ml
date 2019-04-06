
(** There are two kinds of kinds: user defined and kinds from attributes. **)
type 'character kind =
  | Kind of Id.t (** A user-defined kind. **)
  | ProvidePlayerAttribute of Attribute.PlayerAttribute.attribute
      (** An event that provide this attribute **)
  | ProvideContactAttribute of Attribute.ContactAttribute.attribute * 'character
      (** An event that provide this contact to this character. **)

let kind_of_id id = Kind id

let kind_of_attribute a = ProvidePlayerAttribute a

let kind_of_contact a c = ProvideContactAttribute (a, c)

let kind_convert f = function
  | Kind id -> Some (Kind id)
  | ProvidePlayerAttribute a -> Some (ProvidePlayerAttribute a)
  | ProvideContactAttribute (a, c) ->
    Utils.apply_option (f c) (fun c -> ProvideContactAttribute (a, c))

type event_type =
  | For_life_event
  | Long_term_event
  | Medium_term_event
  | Short_term_event
  | Very_short_term_event
  | Immediate_event

let all_event_type = [
    For_life_event ;
    Long_term_event ;
    Medium_term_event ;
    Short_term_event ;
    Very_short_term_event ;
    Immediate_event
  ]

type 'character t = {
    event_type : event_type ;
    event_attendees : 'character PSet.t ;
    event_kinds : ('character, 'character kind PSet.t) PMap.t ;
    constraints_none :
      ('character, 'character kind PSet.t * 'character kind PSet.t) PMap.t ;
    constraints_some :
      ('character, 'character kind PSet.t * 'character kind PSet.t) PMap.t
  }

let partially_instantiate a b e =
  let convert a' = if a' = a then Some b else None in
  let get_constraints m =
    let (before, after) =
      try PMap.find a m
      with Not_found -> (PSet.empty, PSet.empty) in
    let convert_set = PSet.map_filter (kind_convert convert) in
    PMap.add b (convert_set before, convert_set after) PMap.empty in
  if PSet.mem a e.event_attendees then
    Some {
      event_type = e.event_type ;
      event_attendees = PSet.singleton b ;
      event_kinds =
        PMap.add b (
          let k =
            try PMap.find a e.event_kinds
            with Not_found -> PSet.empty in
          PSet.map_filter (kind_convert convert) k) PMap.empty ;
      constraints_none = get_constraints e.constraints_none ;
      constraints_some = get_constraints e.constraints_some
    }
  else None

let instantiate f e =
  let convert m =
    PMap.foldi (fun c (before, after) m ->
      Utils.if_option m (fun m ->
        Utils.if_option (f c) (fun c ->
          Utils.if_option (PSet.map_option (kind_convert f) before) (fun before ->
            Utils.apply_option (PSet.map_option (kind_convert f) after)
              (fun after -> PMap.add c (before, after) m))))) m (Some PMap.empty) in
  Utils.if_option (PSet.map_option f e.event_attendees) (fun attendees ->
    Utils.if_option
      (PMap.foldi (fun c k m ->
        Utils.if_option m (fun m ->
          Utils.if_option (f c) (fun c ->
            Utils.apply_option (PSet.map_option (kind_convert f) k) (fun k ->
              PMap.add c k m)))) e.event_kinds (Some PMap.empty)) (fun kinds ->
      Utils.if_option (convert e.constraints_none) (fun none ->
        Utils.apply_option (convert e.constraints_some) (fun some -> {
            event_type = e.event_type ;
            event_attendees = attendees ;
            event_kinds = kinds ;
            constraints_none = none ;
            constraints_some = some
          }))))

