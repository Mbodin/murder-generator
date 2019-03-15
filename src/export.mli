(** Module Export
 * Provides various functions to translate states into files. **)

(** Each function in this file takes an argument of this type.
 * It contains all the information needed to generate any file. **)
type state = {
    names : string array (** The names of each player. **) ;
    driver : Driver.state
      (** The driver state, containing in particular the translation objects. **) ;
    state : State.t (** The actual generated state. **)
  }

(** Produces a Graphviz representation of the interactions between players. **)
val to_graphviz : state -> string

(** Produces a JSON representation of the state. **)
val to_json : state -> string

(** All the functions of this file are resumed in the following list.
 * Each function is associated two identifiers in [web/translations.json]
 * (one for the name and one for the description), a mime type, and an extension. **)
val all_production : (string * string * string * string * (state -> string)) list

