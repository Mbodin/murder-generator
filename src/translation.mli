(** Module Translation.
 * Provides utilities to store and deal with translations. **)

(** A type to store informations about translations for a given type. **)
type 'a t

(** Languages are just defined by their two/three-letters code. **)
type language = string

(** An empty map. **)
val empty : 'a t

(** Add a translation to an object. **)
val add : 'a t -> language -> 'a -> string -> 'a t

(** Translates an object to a given language.
 * Returns [None] if the object has not been translated to this language. **)
val translate : 'a t -> 'a -> language -> string option

