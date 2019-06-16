(** Module Relation
 * Explicits the different types of relations between characters. **)

(** Basic relations **)
type basic =
  | Neutral (** The two characters doesn’t know each other. **)
  | Hate (** The two characters hate each others, or have motive to specifically
          * hinder the actions of the other one. **)
  | Trust (** The two characters have reasons to trust each others. **)
  | Chaotic (** The characters may hinder or help each other, but it will never
             * be an end goal: their interactions will naturally just be these
             * kinds of temporary alliances and hindering. **)
  | Undetermined (** The characters knows things about each others, but are not
                  * sure whether they can trust the other or whether they should
                  * stop its actions. A typical example is old friends who ceased
                  * to contact each others long ago. **)
  | Avoidance (** Both characters try to avoid one each other, or to avoid saying
               * a specific subject. For instance, if one hides something from the
               * other. **)

(** The type of relations between players **)
type relation =
  | Basic of basic (** A basic relation, as shown above. **)
  | Asymmetrical of basic * basic (** There is some kind of asymmetry.
                                   * For instance, one know things about the other
                                   * (from a newspaper or from the point of view of
                                   * other characters) but not the other: there
                                   * would then probably be a relation
                                   * [Asymmetrical (Neutral, Undetermined)]. **)
  | Explosive of relation * relation (** There is some kind of contradictions in
                                      * the relation between the two characters.
                                      * For instance, they love each others, but
                                      * one is forced to do something against this
                                      * player (this would probably be a relation
                                      * [Explosive (Basic Trust, Basic Chaotic)]).
                                      * These are really complex relations and the
                                      * generator should avoid to create too many
                                      * of them. **)

(** This type is a relation coupled with a boolean stating
 * whether the relation is strong or not. **)
type t = relation * bool

(** How complex it is to understand the character relation.
 * Note that [reverse] does not change the complexity. **)
val complexity : t -> int

(** How complex it is to play and survive the murder.
 * Note that [reverse] does not change the difficulty. **)
val difficulty : t -> int

(** Whether a relation is strong. **)
val is_strong : t -> bool

(** Whether the relation is explosive. **)
val is_explosive : t -> bool

(** The following two functions remove all the [Asymmetrical] constructors,
 * replacing them by either the left or right basic relation. **)
val left_projection : t -> t
val right_projection : t -> t

(** Compose two relations.
 * It tries to summarize both into one as much as it can. **)
val compose : t -> t -> t

(** A neutral element for the composition. **)
val neutral : t

(** Converts a relation to a string. **)
val to_string : t -> string

(** Reverse the point of view of the given relation: every
 * [Asymmetrical (t1, t2)] are replaced by [Asymmetrical (t2, t1)]. **)
val reverse : t -> t

(** Like [Asymmetrical], but combining two arbitrary relations instead of just
 * basic ones. **)
val asymmetrical : t -> t -> t

(** Like [asymmetrical], but combining two [relation]s instead **)
val asymmetrical_relation : relation -> relation -> relation

