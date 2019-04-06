(** Module PSet
 * An implementation of sets. **)

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

(** Merges two sets.
 * This function has been optimised with the assumption that the second set is
 * larger than the first. **)
val merge : 'a t -> 'a t -> 'a t

(** Computes the intersection of two sets.
 * This function has been optimised with the assumption that the second set is
 * larger than the first. **)
val inter : 'a t -> 'a t -> 'a t

(** Computes the difference of two sets. **)
val diff : 'a t -> 'a t -> 'a t

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

(** Maps the set through a function. **)
val map : ('a -> 'b) -> 'a t -> 'b t

(** Only conserves the element of the set that satisfy the given predicate. **)
val filter : ('a -> bool) -> 'a t -> 'a t

(** Maps the set through a function, removing any element returning [None]. **)
val map_filter : ('a -> 'b option) -> 'a t -> 'b t

(** Maps the set through a function.
 * If any element gets mapped to [None], the whole function returns [None]. **)
val map_option : ('a -> 'b option) -> 'a t -> 'b t option

(** Given a predicate, splits the set into a set of elements
 * satisfying the predicate and a set of elements that don’t. **)
val partition : ('a -> bool) -> 'a t -> 'a t * 'a t

(** Same as [partition], but the predicate maps each value to
 * a different type whether they satisfy the predicate. **)
val partition_map : ('a -> ('b, 'c) Utils.plus) -> 'a t -> 'b t * 'c t

(** Converts a list into a set. **)
val from_list : 'a list -> 'a t

(** Converts a set into a list. **)
val to_list : 'a t -> 'a list

(** Returns the domain of a map. **)
val domain : ('a, 'b) PMap.t -> 'a t

(** Merges a set of set into a single set. **)
val flatten : 'a t t -> 'a t

(** Equivalent to [flatten] compose to [map], but more efficient. **)
val flat_map : ('a -> 'b t) -> 'a t -> 'b t

