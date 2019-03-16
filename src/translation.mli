(** Module Translation.
 * Provides utilities to store and deal with translations. **)

(** A type to store informations about translations for a given type. **)
type 'a t

(** The type of language. **)
type language

(** A special “language” for internal use. **)
val generic : language

(** Returns the two/three-letters code corresponding to the provided language. **)
val iso639 : language -> string

(** Converts the two/three-letters code to the corresponding language. **)
val from_iso639 : string -> language

(** An empty map. **)
val empty : 'a t

(** Add a translation to an object. **)
val add : 'a t -> language -> 'a -> string -> 'a t

(** Translates an object to a given language.
 * Returns [None] if the object has not been translated to this language. **)
val translate : 'a t -> 'a -> language -> string option

(** Calls [translate], and finds a way to translate it anyway. **)
val force_translate : 'a t -> 'a -> language -> string

(** [from_json fileName fileContent] reads the [fileContent] string as a
 * JSON object representing translations in different languages.
 * It then returns a translation object as well as the list of the found
 * languages in the file. **)
val from_json : string -> string -> string t * language list

(** A type to store each notion used in elements. **)
type element = {
    category : Utils.Id.t t (** The translations of categories. **) ;
    attribute : State.attribute t (** The translations of attributes. **) ;
    constructor : State.constructor t (** The translations of constructors. **)
  }

(** A record whose fields are all empty. **)
val empty_element : element

