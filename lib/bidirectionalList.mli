(** Module BidirectionalList
 * Defines a kind of list with fast access in both directions. **)

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

