(** Module UnionFind
 * A union-find strucutre. **)

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

