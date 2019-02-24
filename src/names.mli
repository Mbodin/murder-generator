(** Module Names
 * Deals with name generation. **)

(** A specification of the language sounds.
 * The parameterised type is the state system. **)
type 'a t

(** A simple type to represent the alternance of vowels and consonant. **)
type vowelConsonant

(** Create a transition system for vowels and consonants.
 * It takes as an argument six string specifying how the language sounds, as well
 * as the expected size of the output (in term of the given vowels and consonants).
 * Each string is a list of list separated by [,] for the inner lists and [;] for
 * the outer.
 * The inner lists (separated by [,]) commutes, whilst the outer lists represent
 * changes in probability (the first elements being more probable).
 * The six lists corresponds to:
 * - initial vowels and consonants;
 * - middle vowels and consonants;
 * - end vowels and consonants. **)
val createVowelConsonant : int -> string -> string -> string -> string -> string -> string -> vowelConsonant t

(** Generates a random name. **)
val generate : 'a t -> string

