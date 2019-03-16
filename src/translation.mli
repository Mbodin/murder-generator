(** Module Translation.
 * Provides utilities to store and deal with translations. **)

(** The type of language. **)
type language

(** A special “language” for internal use.
 * It typically stores the developping name of each object. **)
val generic : language

(** A type to store informations about translations for a given type.
 * It is a simple representation: for each language, we can get a
 * translation of the stored objects. **)
type 'a t

(** This type is for more advanced translations where grammar has to
 * be involved. **)
type 'a gt

(** Returns the two/three-letters code corresponding to the provided language. **)
val iso639 : language -> string

(** Converts the two/three-letters code to the corresponding language. **)
val from_iso639 : string -> language

(** The type of grammatical tags.
 * A tag can be used to represent any language case (accusative, genetive,
 * locative, etc., but also less frequent ones like aversive or abessive)
 * as well as more fundamental language notions (gender of words, specific
 * inflexions, noun classes, etc.). **)
type tag

(** Imports tags.
 * Tags are expressed as any lowercase string and depends on the language. **)
val get_tag : string -> tag

(** Empty maps. **)
val empty : 'a t
val gempty : 'a gt

(** Add a translation to an object. **)
val add : 'a t -> language -> 'a -> string -> 'a t

(** Add a translation to an object with a specification on the tags where
 * this translation applies.
 * Tags should be ordered consistently across translations, with the most
 * important at the beginning of the list.
 * With the translation, we also have to provide a list of tags. **)
val gadd : 'a gt -> language -> tag list -> 'a -> string -> tag list -> 'a gt

(** Translates an object to a given language.
 * Returns [None] if the object has not been translated to this language. **)
val translate : 'a t -> 'a -> language -> string option

(** Calls [translate], and finds a way to translate it anyway. **)
val force_translate : 'a t -> 'a -> language -> string

(** Translates an object to a given language.
 * Each tag associated with the return value is assured to be given as argument.
 * If there are several matching possible values, then a maximal one
 * is chosen, in the sense that there should not be another solution
 * whose associated matching tags are strictly greater (in term of list
 * inclusion, not list length) then the returned value.
 * List length is not considered.  Instead, tags are considered as they
 * have been used in the declaration of translation (if provided consistently).
 * With the returned translation, a set of additional tags is returned: these
 * are meant to be added to the grammar construct that is being translated.
 * Typically, if a generic translation is gendered, this gender has to be propagated
 * to the rest of the sentence even though the original notion was not directly
 * gendered. **)
val gtranslate : 'a gt -> 'a -> language -> tag Utils.PSet.t -> (string * tag Utils.PSet.t) option

(** Similar to [gtranslate], but will try to find a translation following
 * some heuristics (but no guarantee is provided about them). **)
val gforce_translate : 'a gt -> 'a -> language -> tag Utils.PSet.t -> string * tag Utils.PSet.t

(** [from_json fileName fileContent] reads the [fileContent] string as a
 * JSON object representing translations in different languages.
 * It then returns a translation object as well as the list of the found
 * languages in the file. **)
val from_json : string -> string -> string t * language list

(** A type to store each notion used in elements. **)
type element = {
    category : Utils.Id.t t (** The translations of categories. **) ;
    attribute : State.attribute t (** The translations of attributes. **) ;
    constructor : State.constructor gt (** The translations of constructors. **)
  }

(** A record whose fields are all empty. **)
val empty_element : element

