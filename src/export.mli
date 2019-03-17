(** Module Export
 * Provides various functions to translate states into files. **)

(** Each function in this file takes an argument of this type.
 * It contains all the information needed to generate any file. **)
type state = {
    names : string list (** The names of each player. **) ;
    language : Translation.language
      (** The current language.
       * It will be ignored in outputs that are meant to return a reusable
       * file (like a JSON file), but not for the one targetted to the final
       * user (character sheets or tools for the game master). **) ;
    translation : Translation.element (** The translation objects. **) ;
    state : State.t (** The actual generated state. **)
  }

(** Produces a Graphviz representation of the interactions between players. **)
val to_graphviz : state -> string

(** Produces a JSON representation of the state. **)
val to_json : state -> string

(** Reverts [to_json] back to a list of names and a state.
 * The first argument is the file name (to report errors), and the second one
 * its actual content. **)
val from_json : State.constructor_maps -> string -> string -> string list * State.t

(** All the functions of this file are resumed in the following list.
 * Each function is associated two identifiers in [web/translations.json]
 * (one for the name and one for the description), a mime type, and an
 * extension. **)
val all_production : (string * string * string * string * (state -> string)) list

