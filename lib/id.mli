(** Module Id
 * Contains function to manipulate identifiers. **)

(** A type for identifiers.
 * Integers are used internally, but hiding this fact in the signature
 * helps preventing mistakes. **)
type t (** = int **)

(** Generates a new identifier. **)
val new_id : unit -> t

(** If a large number of identifiers is generated, it is better to have
 * a count just for the application and not a global one.
 * This function thus returns a function counting for the specific usage
 * to be applied. **)
val new_id_function : unit -> unit -> t

(* TODO: Make [new_id_function] the new [new_id] by making the module [Id] generative (parameterised by [()]).
 * (This is more complex than expected.) *)

(** Converts an identifier to a number that can be used as an array index. **)
val to_array : t -> int
(** Converts it back to an identifier. **)
val from_array : int -> t


(** A type for a mapping from a type to identifiers. **)
type 'a map

(** Get the identifier from the identifier map.
 * Returns [None] if the identifier is not in the mapping.
 * Returns [Some id] where [id] is the identifier if the object
 * is in the map. **)
val get_id : 'a map -> 'a -> t option

(** Create a identifier map. **)
val map_create : ?compare:('a -> 'a -> int) -> unit -> 'a map

(** Inserts an object to an identifier map, giving it an identifier.
 * If the object is already associated an identifier, the old one is returned. **)
val map_insert : 'a map -> 'a -> 'a map

(** Similar to [map_insert], but also returns the chosen identifier.  **)
val map_insert_t : 'a map -> 'a -> t * 'a map

(** Return the object corresponding to this identifier. **)
val map_inverse : 'a map -> t -> 'a option

(** Folds along the map.
 * The order is not specified. **)
val map_fold : ('b -> t -> 'a -> 'a) -> 'a -> 'b map -> 'a

