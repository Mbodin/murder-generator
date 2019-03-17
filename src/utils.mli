(** Module Utils
 * Contains useful type declarations and functions. **)

(** Switches some asserts on.
 * These asserts can be costly to perform. **)
val assert_defend : bool


(** Returns its argument. **)
val id : 'a -> 'a

(** Composes two functions. **)
val compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c

(** The monadic version of [option_map] **)
val if_option : 'a option -> ('a -> 'b option) -> 'b option

(** Maps over the option type. **)
val apply_option : 'a option -> ('a -> 'b) -> 'b option

(** If one is *sure* that an option type is actually defined,
 * this function can extract its value.
 * The string is meant to help debug: using [__LOC__] is a good idea. **)
val assert_option : string -> 'a option -> 'a

(** The sum type of two types. **)
type ('a, 'b) plus =
    | Left of 'a
    | Right of 'b

(** The following function considers the Left-constructor to represent an error. **)
val error_monad : ('a, 'b) plus -> ('b -> ('a, 'c) plus) -> ('a, 'c) plus


(** Returns the tail of the list, the empty list being associated
 * with the empty list. **)
val safe_tail : 'a list -> 'a list

(** Creates a list from a function providing the optional next element and iterator.
 * The first element given is used to initialise the function and is not inserted
 * into the list. **)
val unfold : ('a -> ('b * 'a) option) -> 'a -> 'b list

(** Builds the list of nth first elements, from [0] to [n - 1]. **)
val seq : int -> int list

(** Builds the list of nth first elements, from [0] to [n]. **)
val seq_incl : int -> int list

(** Builds the list from [i] to [j], included. **)
val seq_range : int -> int -> int list

(** Builds the array of [n]th first elements, from [0] to [n - 1]. **)
val seq_array : int -> int array

(** Builds the array of [n + 1]th first elements, from [0] to [n]. **)
val seq_incl_array : int -> int array

(** Builds the array from [i] to [j], included. **)
val seq_range_array : int -> int -> int array


(** Creates a list of the given size filled with the given argument. **)
val repeat : int -> 'a -> 'a list

(** Updates the given list element. **)
val list_update : int -> 'a -> 'a list -> 'a list

(** Removes the nth element from a list. **)
val list_remove : int -> 'a list -> 'a list

(** Returns the index where this element is in the list. **)
val list_index : 'a -> 'a list -> int option

(** Returns the index of the first element matching the predicate in the list. **)
val list_predicate_index : ('a -> bool) -> 'a list -> int option

(** Like [List.fold_left], but taking the current index as argument. **)
val list_fold_lefti : (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a

(** Maps the list, removing any element returning [None]. **)
val list_map_filter : ('a -> 'b option) -> 'a list -> 'b list

(** Maps the list.
 * If any of the element of the list maps to [None], then the whole list
 * will be mapped to [None]. **)
val list_map_option : ('a -> 'b option) -> 'a list -> 'b list option

(** Given a transitive comparison function and a list, returns the greatest
 * element of the list (or [None] if the list is empty). **)
val argmax : ('a -> 'a -> int) -> 'a list -> 'a option

(** Sorts and remove all duplicated element from the given list. **)
val uniq : 'a list -> 'a list

(** Shuffle a list. **)
val shuffle : 'a list -> 'a list

(** Pattern match the list in the right instead of the left. **)
val list_match_right : 'a list -> ('a list * 'a) option

(** Swaps the given pair. **)
val swap : 'a * 'b -> 'b * 'a

(** Sorts the two given elements. **)
val pair_sort : 'a * 'a -> 'a * 'a

(** Returns the positive modulo. **)
val positive_mod : int -> int -> int

(** The square of its argument. **)
val square : int -> int

(** Returns a random number between its two arguments, included. **)
val rand : int -> int -> int

(** Takes a list and return a random element from it. **)
val select_any : 'a list -> 'a

(** Similar to select_any, but it removes the element from the list. **)
val take_any : 'a list -> 'a * 'a list

(** Takes a weighted list and return a random element from it. **)
val select : (int * 'a) list -> 'a

(** Similar to select, but it removes the element from the list. **)
val take : (int * 'a) list -> 'a * (int * 'a) list

(** Possible exception returned by the select function. **)
exception EmptyList
exception NegativeWeigth
exception InternalError

(** Sums the integers of the list. **)
val sum : int list -> int

(** Sums the integers of the array. **)
val array_sum : int array -> int

(** Indicates how many elements of the array satisfy the predicates. **)
val array_count : ('a -> bool) -> 'a array -> int

(** Similar to [Array.fold_left], but the function takes the current index
 * as argument. **)
val array_fold_lefti : (int -> 'a -> 'b -> 'a) -> 'a -> 'b array -> 'a

(** Fold through both array. They must have the same size. **)
val array_fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b array -> 'c array -> 'a


(** Flattens a map into a list. **)
val pmap_to_list : ('a, 'b) PMap.t -> ('a * 'b) list


module Id : sig

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
    val map_create : unit -> 'a map

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

  end

module UnionFind : sig

    (** A type for a union-find structure. **)
    type 'a t

    (** Creates an empty union-find structure. **)
    val create : unit -> 'a t

    (** Inserts an element to the given union-find structure:
     * it is now associated to a identifier. **)
    val insert : 'a t -> 'a -> 'a t

    (** Merges two elements of the union-find structure.
     * If the elements are not present, it creates them in the structure. **)
    val merge : 'a t -> 'a -> 'a -> 'a t

    (** States whether two elements are in the same equivalence class in the
     * union-find.
     * Returns [None] if one element is not in the structure. **)
    val same_class : 'a t -> 'a -> 'a -> bool option

    (** Same than same_class, but inserts the elements in the structure
     * if they are not present. **)
    val same_class_insert : 'a t -> 'a -> 'a -> bool * 'a t

    (** Fold along all classes of the union-find structure. **)
    val fold : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b

    (** Iterate along all classes of the union-find structure. **)
    val iter : ('a -> unit) -> 'a t -> unit

    (** Provides a list of equivalence classes. **)
    val to_list : 'a t -> 'a list

    (** Provides one class of the union-find structure.
     * There is no guarantee which one will be picked.
     * Only returns [None] if the structure is empty. **)
    val get_one_class : 'a t -> 'a option

    (** Returns true if there is exactly or less than one class,
     * that is if all elements of the union-find are from the same class. **)
    val one_class : 'a t -> bool

  end

module BidirectionalList : sig

    (** A list whose elements can be easily taken and added from and
     * to both directions. **)
    type 'a t

    (** An empty bidirectional list. **)
    val empty : 'a t

    (** Checks whether the list is empty. **)
    val is_empty : 'a t -> bool

    (** If the list is empty, returns [None], otherwise returns the head
     * and the tail from the left side of the list. **)
    val match_left : 'a t -> ('a * 'a t) option

    (** Same as [match_left], but from the right side of the list. **)
    val match_right : 'a t -> ('a t * 'a) option

    (** Add an element to the left of the list. **)
    val add_left : 'a -> 'a t -> 'a t

    (** Add an element to the right of the list. **)
    val add_right : 'a t -> 'a -> 'a t

    (** Converts a usual list to a bidirectional list.
     * The head of the list is on the left of the bidirectional list. **)
    val from_list : 'a list -> 'a t

    (** Converts the bidirectional list into a list, from left to right. **)
    val to_list : 'a t -> 'a list

    (** Check whether all the elements of the list satisfy the given predicate. **)
    val for_all : ('a -> bool) -> 'a t -> bool

    (** Check whether there exists at least one element in the list satisfying
     * the given predicate. **)
    val exists : ('a -> bool) -> 'a t -> bool

    (** Returns the size of the bidirectional list. **)
    val length : 'a t -> int

    (** Only keeps the elements in the bidirectional satisfying the given
     * predicate. **)
    val filter : ('a -> bool) -> 'a t -> 'a t

  end

module PSet : sig

    (** An implementation of sets based on [PMap]. **)
    type 'a t

    (** The empty set. **)
    val empty : 'a t

    (** The singleton set. **)
    val singleton : 'a -> 'a t

    (** Check whether a set is empty. **)
    val is_empty : 'a t -> bool

    (** Add an element to a set. **)
    val add : 'a -> 'a t -> 'a t

    (** Removes an element from a set.
     * If the element is not present, the set is left unchanged. **)
    val remove : 'a -> 'a t -> 'a t

    (** Checks whether an element is present in the set. **)
    val mem : 'a -> 'a t -> bool

    (** Merges two sets. **)
    val merge : 'a t -> 'a t -> 'a t

    (** Computes the intersection of two sets. **)
    val inter : 'a t -> 'a t -> 'a t

    (** Given two sets, states whether the first one is included
     * in the second one. **)
    val incl : 'a t -> 'a t -> bool

    (** Folds over the set. **)
    val fold : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b

    (** Iterates over the set. **)
    val iter : ('a -> unit) -> 'a t -> unit

    (** Check whether all the elements of the set satisfy the given predicate. **)
    val for_all : ('a -> bool) -> 'a t -> bool

    (** Check whether there exists at least one element in the set satisfying
     * the given predicate. **)
    val exists : ('a -> bool) -> 'a t -> bool

    (** Maps the set to a function. **)
    val map : ('a -> 'b) -> 'a t -> 'b t

    (** Given a predicate, splits the set into a set of elements
     * satisfying the predicate and a set of elements that don’t. **)
    val partition : ('a -> bool) -> 'a t -> 'a t * 'a t

    (** Same as [partition], but the predicate maps each value to
     * a different type whether they satisfy the predicate. **)
    val partition_map : ('a -> ('b, 'c) plus) -> 'a t -> 'b t * 'c t

    (** Converts a list into a set. **)
    val from_list : 'a list -> 'a t

    (** Converts a set into a list. **)
    val to_list : 'a t -> 'a list

    (** Returns the domain of a map. **)
    val domain : ('a, 'b) PMap.t -> 'a t

    (** Merges a set of set into a single set. **)
    val flatten : 'a t t -> 'a t

  end

