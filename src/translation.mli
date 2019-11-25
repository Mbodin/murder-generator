(** Module Translation.
 * Provides utilities to store and deal with translations. **)

(** The type of language. **)
type language

(** A special “language” for internal use.
 * It typically stores the developping name of each object. **)
val generic : language

(** The type of grammatical tags.
 * A tag can be used to represent any language case (accusative, genitive,
 * locative, etc., but also less frequent ones like aversive or abessive)
 * as well as more fundamental language notions (gender of words, specific
 * inflexions, noun classes, etc.). **)
type tag

(** A special tag to mean that the translation only applies for the generic
 * object and not its instances. **)
val base : tag

(** A tag command.
 * It can be either associated to [None]: it is then a constraint.
 * It can also be associated to a boolean: and it then becomes a command.
 * If the boolean is [true], this tag is added when evaluated the variable,
 * if [false], the command is removed.
 * This is for instance handy for gendered languages where the name of a group
 * might not be of the same gender than each of its components. **)
type command = bool option * tag

(** Imports tags.
 * Tags are expressed as any lowercase string and depends on the language. **)
val get_tag : string -> tag

(** Export back a tag. **)
val print_tag : tag -> string

(** A type to store informations about translations for a given type.
 * It is a simple representation: for each language, we can get a
 * translation of the stored objects. **)
type 'a t

(** This type is for more advanced translations where grammar has to
 * be involved. **)
type 'a gt

(** This type is for even more advanced translations involving sentences
 * with a lot of grammar.
 * Sentences involve partial translations, with symbolic variables.
 * The type ['b] represents these variables. **)
type ('a, 'b) st

(** This type lists the possible elements in a partial sentence.
 * Items’s result are concatenated (separated by space) to form
 * the overall translation. **)
type 'b sitem =
  | Direct of string (** A direct translation given as a string **)
  | Variable of 'b * tag PSet.t * tag PSet.t * tag PSet.t
    (** A variable, which is supposed associated with its own translations.
     * The variable is associates three sets:
     * - the first one is the constraints over the variable,
     * - the second one is the added tags when fetching the translation of this
     *   variable,
     * - the third is the removed tags when fetching the translation of the
     *   variable. **)

(** A smart constructor for [Variable] taking a list of commands instead
 * of three sets. **)
val variable : 'b -> command list -> 'b sitem

(** Change the variables used in a sentence item. **)
val sitem_map : ('a -> 'b) -> 'a sitem -> 'b sitem

(** Returns the two/three-letters code corresponding to the provided language. **)
val iso639 : language -> string

(** Converts the two/three-letters code to the corresponding language. **)
val from_iso639 : string -> language

(** Empty maps. **)
val empty : 'a t
val gempty : 'a gt
val sempty : ('a, 'b) st

(** Add a translation to an object. **)
val add : 'a t -> language -> 'a -> string -> 'a t

(** Returned by [gadd] and [sadd] when given commands that conflicts with
 * each others (typically when the same tag is both added and removed). **)
exception ConflictingCommands of command * command

(** Add a translation to an object with a specification on the tags where
 * this translation applies.
 * Tags should be ordered consistently across translations, with the most
 * important at the beginning of the list. **)
val gadd : 'a gt -> language -> command list -> 'a -> string -> 'a gt

(** Similar to [gadd] but for sentences. **)
val sadd : ('a, 'b) st -> language -> command list -> 'a -> 'b sitem list -> ('a, 'b) st

(** Translates an object to a given language.
 * Returns [None] if the object has not been translated to this language. **)
val translate : 'a t -> language -> 'a -> string option

(** Calls [translate], and finds a way to translate it anyway.
 * The additional debugging argument helps understanding from where came
 * a missing translation. **)
val force_translate : ?debug:('a -> string option) -> 'a t -> language -> 'a -> string

(** Translation functions involving grammar have the following type.
 * See [gtranslate] for more details. **)
type 'a translation_function = 'a -> tag PSet.t -> (string * tag PSet.t) option

(** An alternative to [translation_function] where the function uses heuristics
 * to always return a result. **)
type 'a complete_translation_function = 'a -> tag PSet.t -> string * tag PSet.t

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
val gtranslate : 'a gt -> language -> 'a translation_function

(** Similar to [gtranslate], but will try to find a translation following
 * some heuristics (but no guarantee is provided about them). **)
val gforce_translate : ?debug:('a -> string option) -> 'a gt -> language -> 'a complete_translation_function

(** Like [gtranslate] but for sentences.
 * It is supposed to be given a translation function for the variables, as well
 * as a function providing for each variable its natural tags. **)
val stranslate : ('a, 'b) st -> language -> ('b -> tag PSet.t) -> 'b translation_function -> 'a translation_function

(** Similar to [stranslate], but never fails. **)
val sforce_translate : ?debug:('a -> string option) -> ('a, 'b) st -> language -> ('b -> tag PSet.t) -> 'b translation_function -> 'a complete_translation_function

(** Update the variables used in a translation object. **)
val smap_option : ('b -> 'c option) -> ('a, 'b) st -> ('a, 'c) st option

(** Return all the possible translations of a given object, regardless of the context. **)
val gall_translations : 'a gt -> language -> 'a -> string list

(** Return all the possible translations of a given object, regardless of the context.
 * This function takes a function with a similar effect on the sentence items. **)
val sall_translations : ('a, 'b) st -> language -> ('b -> string list) -> 'a -> string list

(** [from_json fileName fileContent] reads the [fileContent] string as a
 * JSON object representing translations in different languages.
 * It then returns a translation object as well as the list of the found
 * languages in the file. **)
val from_json : string -> string -> string t * language list

(** A type to store each notion used in elements. **)
type element = {
    category : Id.t t (** The translations of categories. **) ;
    category_description : Id.t t (** The translations of categories’s descriptions. **) ;
    attribute : Attribute.attribute t (** The translations of attributes. **) ;
    constructor : Attribute.constructor gt (** The translations of constructors. **) ;
    add : (language,
           (Attribute.PlayerAttribute.constructor, tag PSet.t) PMap.t) PMap.t
      (** For each constructor and language, provides a set of added tags. **)
  }

(** A record whose fields are all empty. **)
val empty_element : element

(** Fold over the translations of a particular object and a particular language
 * in a [gt] translation structure.
 * The propagating function takes as argument the list of tags needed to reach
 * this point, the current translation, the set of tag that are added by this
 * translation, and the set of tags that are removed. **)
val gfold : ('a -> tag list -> string -> tag PSet.t -> tag PSet.t -> 'a) -> 'a -> 'b gt -> 'b -> language -> 'a

