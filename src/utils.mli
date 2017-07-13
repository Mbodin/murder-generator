(** Module Utils
 * Contains useful type declarations and functions. **)


(** Returns its argument. **)
val id : 'a -> 'a

(** Maps the option type. **)
val option_map : ('a -> 'b) -> 'a option -> 'b option


(** The sum type of two types. **)
type ('a, 'b) plus =
    | Left of 'a
    | Right of 'b

(** The following function considers the Left-constructor to represent an error. **)
val error_monad : ('a, 'b) plus -> ('b -> ('a, 'c) plus) -> ('a, 'c) plus


(** Creates a list from a function providing the optionnal next element and iterator. The first element given is used to initialise the function and is not inserted into the list. **)
val unfold : ('a -> ('b * 'a) option) -> 'a -> 'b list

(** Builds the list of nth first elements, from 0 to n - 1. **)
val seq : int -> int list

(** Takes a list and return a random element from it. **)
val select_any : 'a list -> 'a

(** Takes a weigthed list and return a random element from it. **)
val select : (int * 'a) list -> 'a

(** Possible exception returned by the select function. **)
exception NegativeWeigth
(** Note that if the list is empty, the total weigth will be zero and the exception NegativeWeigth will also be sent. **)
exception InternalError

(** Sums the integers of the list. **)
val sum : int list -> int

(** Sums the integers of the array. **)
val array_sum : int array -> int

(** Indicates how many elements of the array satisfy the predicates. **)
val array_count : ('a -> bool) -> 'a array -> int


(** A type for identifiers. Integers are used internally, but hidding this faxt in the signature helps preventing mistakes. **)
type idt (** = int **)

(** Generates a new identifier. **)
val new_id : unit -> idt

(** If a large number of identifiers is generated, it is better to have a count just for the application and not a global one. This function thus returns a function counting for the specific usage to be applied. **)
val new_id_function : unit -> unit -> idt

