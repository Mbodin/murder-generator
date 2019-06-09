(** Module Pool
 * Deals with a pool of current elements, prioritising elements
 * that can explain the currently needed attributes. **)

(** In this file, elements are supposed to be given as identifiers. **)
type element = Id.t

(** A register of existing elements. **)
type global

(** An empty register. **)
val empty_global : global

(** Register that an element exists, and that it may provide the following
 * attributes. **)
val register_element : global -> element -> Attribute.attribute list -> global

(** Unregister an element.
 * It won’t be considered by the pool. **)
val unregister_element : global -> element -> global

(** Same as [unregister_element], but for a set of elements. **)
val unregister_elements : global -> element PSet.t -> global

(** Unregister any element not satisfying the provided predicate. **)
val filter_global : global -> (element -> bool) -> global

(** The pool type. **)
type t

(** The empty pool. **)
val empty : global -> t

(** States whether the pool is empty. **)
val is_empty : t -> bool

(** Returns the number of elements in the pool.
 * This function puts the pool into normal form and thus may be costly. **)
val length : t -> int

(** Returns an overapproximation of the length of the pool. **)
val quick_length : t -> int

(** Pick an element from the pool.
 * This element will then be put at the end of the pool.
 * It only fails if the pool is empty, in this case the returned pool is empty. **)
val pick : t -> element option * t

(** As of [pick], but the returned element is guaranteed not being in the set.
 * If the function returns [None], this means that all elements of the pool
 * are in the input set. **)
val pick_except : t -> element PSet.t -> element option * t

(** Pick and remove an element from the pool.
 * It only fails if the pool is empty, in this case the returned pool is empty. **)
val pop : t -> element option * t

(** Reorders all elements of the pool. **)
val shuffle : t -> t

(** Reorders the 10 first elements of the pool.
 * This number can be set by the optional argument. **)
val shuffle_beginning : ?size:int -> t -> t

(** Remove from the pool all elements that don’t provide this attribute. **)
val restrict : t -> Attribute.attribute -> t

(** Only keeps from the pool the elements satisfying the provided predicate. **)
val filter : t -> (element -> bool) -> t

(** Remove from the pool all elements that provide this attribute. **)
val filter_out : t -> Attribute.attribute -> t

(** Add an element to the pool to its end. **)
val add : t -> element -> t

(** Add all elements that provide this attribute to the pool. **)
val add_attribute : t -> Attribute.attribute -> t

(** Remove an element from the pool. **)
val remove : t -> element -> t

(** Definitely remove an element from a pool: it will be removed from the pool,
 * but it will also never appear in it when calling [add_attribute].
 * The functions in this module assume that once an element has been definitely
 * removed, no attempt in adding it manually with the [add] function will be
 * attempted.
 * The same element is assumed never to be removed twice. **)
val definitely_remove : t -> element -> t

(** As of [definitely_remove], but with a set. **)
val definitely_remove_set : t -> element PSet.t -> t

