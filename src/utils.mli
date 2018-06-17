(** Module Utils
 * Contains useful type declarations and functions. **)

(** Returns its argument. **)
val id : 'a -> 'a

(** Maps the option type. **)
val option_map : ('a -> 'b) -> 'a option -> 'b option

(** The monadic version of option_map **)
val if_option : 'a option -> ('a -> 'b option) -> 'b option

(** The sum type of two types. **)
type ('a, 'b) plus =
    | Left of 'a
    | Right of 'b

(** The following function considers the Left-constructor to represent an error. **)
val error_monad : ('a, 'b) plus -> ('b -> ('a, 'c) plus) -> ('a, 'c) plus


(** Returns the tail of the list, the empty list being associated with the empty list. **)
val safe_tail : 'a list -> 'a list

(** Creates a list from a function providing the optional next element and iterator. The first element given is used to initialise the function and is not inserted into the list. **)
val unfold : ('a -> ('b * 'a) option) -> 'a -> 'b list

(** Builds the list of nth first elements, from 0 to n - 1. **)
val seq : int -> int list

(** Builds the list of nth first elements, from 0 to n. **)
val seq_incl : int -> int list

(** Builds the list from i to j, included. **)
val seq_range : int -> int -> int list

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

(** Sorts and remove all duplicated element from the given list. **)
val uniq : 'a list -> 'a list


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
exception NegativeWeigth
(** Note that if the list is empty, the total weight will be zero and the exception NegativeWeigth will also be sent. **)
exception InternalError

(** Sums the integers of the list. **)
val sum : int list -> int

(** Sums the integers of the array. **)
val array_sum : int array -> int

(** Indicates how many elements of the array satisfy the predicates. **)
val array_count : ('a -> bool) -> 'a array -> int

(** Indicates whether all the elements of the array satisfies the predicate. **)
val array_for_all : ('a -> bool) -> 'a array -> bool


module Id : sig

    (** A type for identifiers. Integers are used internally, but hiding this fact in the signature helps preventing mistakes. **)
    type t (** = int **)

    (** Generates a new identifier. **)
    val new_id : unit -> t

    (** If a large number of identifiers is generated, it is better to have a count just for the application and not a global one. This function thus returns a function counting for the specific usage to be applied. **)
    val new_id_function : unit -> unit -> t

    (** Converts an identifier to a number that can be used as an array index. **)
    val to_array : t -> int


    (** A type for a mapping from a type to identifiers. **)
    type 'a map

    (** Get the identifier from the identifier map. The function may return None if the identifier is not in the mapping, but the important property is that if it returns Some for an object, this value is consistently different from the one of any other objects, expect of course for the same objects. **)
    val get_id : 'a map -> 'a -> t option

    (** Create a identifier map. **)
    val map_create : unit -> 'a map

    (** Create a identifier map specialised for the type t. **)
    val t_map_create : t map
    (** Create a identifier map specialised for the type int. **)
    val int_map_create : int map

    (** Inserts an object to an identifier map, giving it an identifier.
     * If the object is already associated an identifier, the old one is returned. **)
    val map_insert : 'a map -> 'a -> 'a map
    (** As for map_insert, but also returns the chosen identifier.  **)
    val map_insert_t : 'a map -> 'a -> t * 'a map

    (** Return the object corresponding to this identifier. **)
    val map_inverse : 'a map -> t -> 'a option

  end

module UnionFind : sig

    (** A type for a union-find structure. **)
    type 'a t

    (** Creates an empty union-find structure. **)
    val create : unit -> 'a t

    (** Variant for specialised versions of the union-find structure. **)
    val create_idt : unit -> Id.t t
    val create_int : unit -> int t

    (** Inserts an element to the given union-find structure: it is now associated to a identifier. **)
    val insert : 'a t -> 'a -> 'a t

    (** Merges two elements of the union-find structure. If the elements are not present, it creates them in the structure. **)
    val merge : 'a t -> 'a -> 'a -> 'a t

    (** States whether two elements are in the same equivalence class in the union-find.  Returns None if one element is not in the structure. **)
    val same_class : 'a t -> 'a -> 'a -> bool option

    (** Same than same_class, but inserts the elements in the structure if they are not present. **)
    val same_class_insert : 'a t -> 'a -> 'a -> bool * 'a t

    (** Fold along all classes of the union-find structure. **)
    val fold : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b

    (** Iterate along all classes of the union-find structure. **)
    val iter : ('a -> unit) -> 'a t -> unit

    (** Provides a list of equivalence classes. **)
    val to_list : 'a t -> 'a list

    (** Provides one class of the union-find structure. There is no guarantee which one will be picked. Only returns None if the structure is empty. **)
    val get_one_class : 'a t -> 'a option

    (** Returns true if there is exactly or less than one class, that is if all elements of the union-find are from the same class. **)
    val one_class : 'a t -> bool

  end

module BidirectionalList : sig

    (** A list whose elements can be easily taken and added from and to both directions. **)
    type 'a t

    val is_empty : 'a t -> bool
    val match_left : 'a t -> ('a * 'a t) option
    val match_right : 'a t -> ('a t * 'a) option
    val add_left : 'a -> 'a t -> 'a t
    val add_right : 'a t -> 'a -> 'a t
    val from_list : 'a list -> 'a t
    val to_list : 'a t -> 'a list
    val for_all : ('a -> bool) -> 'a t -> bool

  end

