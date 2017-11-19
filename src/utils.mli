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


(** Creates a list from a function providing the optional next element and iterator. The first element given is used to initialise the function and is not inserted into the list. **)
val unfold : ('a -> ('b * 'a) option) -> 'a -> 'b list

(** Builds the list of nth first elements, from 0 to n - 1. **)
val seq : int -> int list

(** Returns a random number between its two arguments, included. **)
val rand : int -> int -> int

(** Takes a list and return a random element from it. **)
val select_any : 'a list -> 'a

(** Takes a weighted list and return a random element from it. **)
val select : (int * 'a) list -> 'a

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


(** A type for identifiers. Integers are used internally, but hiding this fact in the signature helps preventing mistakes. **)
type idt (** = int **)

(** Generates a new identifier. **)
val new_id : unit -> idt

(** If a large number of identifiers is generated, it is better to have a count just for the application and not a global one. This function thus returns a function counting for the specific usage to be applied. **)
val new_id_function : unit -> unit -> idt

(** Converts an identifier to a number that can be used as an array index. **)
val idt_to_array : idt -> int


(** A type for a mapping from a type to identifiers. **)
type 'a idt_map

(** Get the identifier from the identifier map. The function may return None if the identifier is not in the mapping, but the important property is that if it returns Some for an object, this value is consistently different from the one of any other objects, expect of course for the same objects. **)
val get_id : 'a idt_map -> 'a -> idt option

(** Create a identifier map. **)
val idt_map_create : unit -> 'a idt_map

(** Create a identifier map specialised for the type idt. **)
val idt_idt_map_create : idt idt_map
(** Create a identifier map specialised for the type int. **)
val idt_int_map_create : int idt_map

(** Inserts an object to an identifier map, giving it an identifier. **)
val idt_map_insert : 'a idt_map -> 'a -> 'a idt_map
(** As for idt_map_insert, but also returns the chosen identifier.  **)
val idt_map_insert_idt : 'a idt_map -> 'a -> idt * 'a idt_map


(** A type for a union-find structure. **)
type 'a union_find

(** Creates an empty union-find structure. **)
val create_union_find : unit -> 'a union_find

(** Variant for specialised versions of the union-find structure. **)
val create_union_find_idt : idt union_find
val create_union_find_int : int union_find

(** Inserts an element to the given union-find structure: it is now associated to a identifier. **)
val insert : 'a union_find -> 'a -> 'a union_find

(** Same as insert, but also returns the associated identifier. **)
val insert_idt : 'a union_find -> 'a -> idt * 'a union_find

(** Merges two elements of the union-find structure. If the elements are not present, it creates it. **)
val merge : 'a union_find -> 'a -> 'a -> 'a union_find

(** Same as merge, but also returns the associated identifier. **)
val merge_idt : 'a union_find -> 'a -> 'a -> idt * 'a union_find

(** Returns the identifier of the class of an element in a union-find. Note that such identifiers are updated at each merge. May return None if the element is not present. The find function may optimise the union-find structure: the update structure is returned in the result. Such an optimised structure is equivalent than the previous one, just quicker to read. **)
val find : 'a union_find -> 'a -> (idt * 'a union_find) option

(** Same as find, but inserts the element if not present. **)
val find_insert : 'a union_find -> 'a -> idt * 'a union_find


(** A list whose elements can be easily taken and added from and to both directions. **)
type 'a two_direction_list

val two_direction_list_is_empty : 'a two_direction_list -> bool
val match_left : 'a two_direction_list -> ('a * 'a two_direction_list) option
val match_right : 'a two_direction_list -> ('a two_direction_list * 'a) option
val add_left : 'a -> 'a two_direction_list -> 'a two_direction_list
val add_right : 'a two_direction_list -> 'a -> 'a two_direction_list
val two_direction_list_from_list : 'a list -> 'a two_direction_list
val two_direction_list_to_list : 'a two_direction_list -> 'a list
val for_all : ('a -> bool) -> 'a two_direction_list -> bool

