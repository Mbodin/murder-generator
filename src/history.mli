(** Module History
 * Stores the history of a character. **)

type character = int

type result (** The result of an event **) =
    | Relation_event (** An event that changed the way a character relates with another one. **)
        of character (** The character in question **)
        * Relation.t (** How the character now perceive this new relation.
                       * Note that in case of a compound relation, this
                       * relation is from the point of view of the player:
                       * asymmetrical relations should probably never
                       * appear here. **)

type event (** An important event in the player life. **) =
    date (** Beginning of the event **) *
    date (** End of the event **) *
    result (** Result of the event in the character’s vision of the world. **)

type t = event list
(** The history of a character is a list of events that created
 * the current mind state of the character. **)

