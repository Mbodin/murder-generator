(** Module Pool
 * Deals with a pool of current elements, prioritising elements
 * that can explain the currently needed attributes. **)

(** In this file, elements are supposed to be given as identifiers. **)
type element = Utils.Id.t

(** Register that an element exists, and that it may provide the following attributes. **)
val add_element : element -> State.attribute list -> unit

(** Unregister an element.
 * It won’t be considered by the pool. **)
val remove_element : element -> unit

(** The pool type.
 * Note that any call to [add_element] and [remove_element] may invalidate
 * any existing pool. **)
type t

(** The empty pool. **)
val empty : t

(** States whether the pool is empty. **)
val is_empty : t -> bool

(** Pick an element from the pool.
 * This element will then be put at the end of the pool.
 * It only fails if the pool is empty, in this case the returned pool is empty. **)
val pick : t -> element option * t

(** Pick and remove an element from the pool.
 * It only fails if the pool is empty, in this case the returned pool is empty. **)
val pop : t -> element option * t

(** Remove from the pool all elements that don’t provide this attribute. **)
val restrict_only : t -> State.attribute -> t

(** Remove from the pool all elements that provide this attribute. **)
val restrict_but : t -> State.attribute -> t

(** Add an element to the pool. **)
val add : t -> element -> t

(** Add all elements that provide this attribute to the pool. **)
val add_attribute : t -> State.attribute -> t

