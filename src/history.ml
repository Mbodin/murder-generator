
type character = Utils.idt

type result =
    | Relation_event
        of character
        * Relation.t

type date =
    int
    * int
    * int

let current_year = TODO

let add_years (y, d, m) i =
    (y + i, d, m)

(* FIXME: Do we really want to bother with this? *)
let leap_year y =
    let y = y + current_year in
    if y mod 4 <> 0 then false
    else if y mod 100 <> 0 then true
    else y mod 400 = 0

let rec add_days (y, d, m) i =
    if d + i < 0 then
        add_years (TODO)
    (y + i, d, m)

val add_minutes : date -> int -> date

type event_type =
    | For_life_event
    | Long_term_event
    | Medium_term_event
    | Short_term_event
    | Very_short_term_event
    | Instance_event

type event (** An important event in the player life. **) =
    date (** Beginning of the event **)
    * date (** End of the event **)
    * result (** Result of the event in the character’s vision of the world. **)
    * event_type

(** A smart constructor for events **)
type generate_event : date (** Beginning **) -> event_type (** Duration **) -> result -> event

(** States whether two events are compatible, that is that they do not overlap, or that they
 * are of different types. **)
type compatible_events : event -> event -> bool

(** The history of a character is a list of events that created
 * the current mind state of the character. **)
type t = event list

(** States whether an event is compatible with an history. **)
type compatible : t -> event -> bool

