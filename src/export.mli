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
    generic_translation : string Translation.t
      (** Generic translations, defined in [web/translations.json]
       * instead of files in [data/]. **) ;
    state : State.final (** The actual generated state. **)
  }

(** Produce a Graphviz representation of the interactions between players. **)
val to_graphviz : state -> string

(** Produce a JSON representation of the state. **)
val to_json : state -> string

(** Produce a printable representation of the state. **)
val to_block : state -> 'node InOut.block

(** Revert [to_json] back to a list of names and a state.
 * The first argument is the file name (to report errors), and the second one
 * its actual content. **)
val from_json : Attribute.constructor_maps -> string -> string -> string list * State.t

(** All the functions of this file are resumed in the following list.
 * Each function is associated two identifiers in [web/translations.json]
 * (one for the name and one for the description), a mime type, a boolean,
 * and an extension.  The boolean indicates whether newlines should be
 * converted to suits the host’s operating system. **)
val all_production : (string * string * string * string * bool * (state -> string)) list

