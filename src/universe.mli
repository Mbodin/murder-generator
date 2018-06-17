(** Module Universe
 * Contains all the relation and history generators of the game. **)

type universe = Generator.t list list
(** A universe is a list of generators.
 * These generators are categorized by steps: we continue applying
 * the generators of a step until none are left, then consider the
 * next step, etc.
 * This way enables generators that are meant to be used only in
 * the beginning of the generation and generators only useful at
 * the end of it to be executed as planned. **)

val universes : universe list

