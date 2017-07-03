(** Module Relation
 * Explicits the different types of relations between characters. **)

type basic (** Basic relations **) =
    | Neutral (** The two characters doesn’t know each other. **)
    | Hate (** The two characters hate each others, or have motive to specifically hinder the actions of the other one. **)
    | Trust (** The two characters have reasons to trust each others. **)
    | Chaotic (** The characters may hinder or help each other, but it will never be an end goal: their interactions will naturally just to these kinds of alliances and hindering. **)
    | Undetermined (** The characters knows things about each others, but are not sure whether they can trust the other or whether they should stop its actions. A typical example is old friends who ceased to contact each others long ago. **)

type t (** The type of relations between players **) =
    | Basic of basic (** A basic relation, as shown above. **)
    | Asymetrical of basic * basic (** There is some kind of asymmetry. For instance, one know things about the other (from a newspaper or from the point of view of other characters) but not the other: there would then probably be a relation Asymetrical (Neutral, Undetermined). **)
    | Explosive of t * t (** There is some kind of contradictions in the relation between the two characters. For instance, they love each others, but one is forced to do something against this player (which would probably be a relation Explosive (Basic Trust, Basic Chaotic)). These are really complex relations and the generator should avoid to create too many of them. **)


val complexity : t -> int (** How complex it is to understand the character relation. **)
val difficulty : t -> int (** How complex it is to play and survive the murder. **)

val is_explosive : t -> bool

val compose_relation : t -> t -> t

