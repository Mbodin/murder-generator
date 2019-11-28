(** Module Utils
 * Contains useful type declarations and functions. **)

(** Switche some asserts on.
 * These asserts can be costly to perform. **)
val assert_defend : bool


(** Return its argument. **)
val id : 'a -> 'a

(** Compose two functions. **)
val compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c

(** The monadic version of [option_map] **)
val if_option : 'a option -> ('a -> 'b option) -> 'b option

(** Map over the option type. **)
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

(** A value associated with some cache. **)
type ('value, 'cache) cached

(** A constructor for [cached] values. **)
val cached : 'value -> 'cache -> ('value, 'cache) cached

(** Get the value without the cache. **)
val get_value : ('value, 'cache) cached -> 'value

(** Get the current cache associated with a value. **)
val get_cache : ('value, 'cache) cached -> 'cache

(** Non-functionnally update a cache. **)
val set_cache : ('value, 'cache) cached -> 'cache -> unit


(** Return the tail of the list, the empty list being associated
 * with the empty list. **)
val safe_tail : 'a list -> 'a list

(** Returns the [n]th first elements of the list, in order.
 * If the list is smaller than this number of elements, returns the list. **)
val list_header : int -> 'a list -> 'a list

(** Create a list from a function providing the optional next element and iterator.
 * The first element given is used to initialise the function and is not inserted
 * into the list. **)
val unfold : ('a -> ('b * 'a) option) -> 'a -> 'b list

(** Build the list of nth first elements, from [0] to [n - 1]. **)
val seq : int -> int list

(** Build the list of nth first elements, from [0] to [n]. **)
val seq_incl : int -> int list

(** Build the list from [i] to [j], included. **)
val seq_range : int -> int -> int list

(** Build the array of [n]th first elements, from [0] to [n - 1]. **)
val seq_array : int -> int array

(** Build the array of [n + 1]th first elements, from [0] to [n]. **)
val seq_incl_array : int -> int array

(** Build the array from [i] to [j], included. **)
val seq_range_array : int -> int -> int array


(** Create a list of the given size filled with the given argument. **)
val repeat : int -> 'a -> 'a list

(** Update the given list element. **)
val list_update : int -> 'a -> 'a list -> 'a list

(** Remove the [n]th element from a list. **)
val list_remove : int -> 'a list -> 'a list

(** Split a list into two: the part whose index is less than the given index
 * and the part where it is more.
 * This function can be specified as follows:
 * - the concatenation of both its return values creates the original list.
 * - if the given index is positive and less than the size of the list, then
 *   the first returned list has the same size than the given index.
 * - if the given index is greater than the size of the input list, then the
 *   second returned list is empty.
 * - if the given index is negative, it is treated as [0]. **)
val list_split : int -> 'a list -> 'a list * 'a list

(** Return the index where this element is in the list. **)
val list_index : 'a -> 'a list -> int option

(** Return the index of the first element matching the predicate in the list. **)
val list_predicate_index : ('a -> bool) -> 'a list -> int option

(** Given a predicate and a list, return the maximum prefix of the list
 * such that all its elements satisfy the given predicate.
 * This function also returns the rest of the list. **)
val list_predicate_prefix : ('a -> bool) -> 'a list -> 'a list * 'a list

(** Like [List.fold_left], but taking the current index as argument. **)
val list_fold_lefti : (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a

(** Map the list, removing any element returning [None]. **)
val list_map_filter : ('a -> 'b option) -> 'a list -> 'b list

(** Map the list.
 * If any of the element of the list maps to [None], then the whole list
 * will be mapped to [None]. **)
val list_map_option : ('a -> 'b option) -> 'a list -> 'b list option

(** Similar to [List.find_opt], but also return the associated value. **)
val list_find_map_opt : ('a -> 'b option) -> 'a list -> 'b option

(** Same as [List.partition], but the predicate maps each value to
 * a different type whether they satisfy the predicate. **)
val list_partition_map : ('a -> ('b, 'c) plus) -> 'a list -> 'b list * 'c list

(** Return the index and associated value of the pair with first element equal
 * to the first argument, or raise [Not_found] if not present. **)
val list_associ : 'a -> ('a * 'b) list -> (int * 'b)

(** Same as [list_associ], but return [None] if not present. **)
val list_associ_opt : 'a -> ('a * 'b) list -> (int * 'b) option

(** Generalised versions of [List.map2]. **)
val list_map3 : ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list
val list_map4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a list -> 'b list -> 'c list -> 'd list -> 'e list

(** Given a transitive comparison function and a list, returns the greatest
 * element of the list (or [None] if the list is empty). **)
val argmax : ('a -> 'a -> int) -> 'a list -> 'a option

(** Sort and remove all duplicated element from the given list.
 * The comparison function is optional: if not given, it will be [compare]. **)
val uniq : ?cmp:('a -> 'a -> int) -> 'a list -> 'a list

(** Return [true] if and only if [uniq] does not remove any element
 * from the given list. **)
val is_uniq : ?cmp:('a -> 'a -> int) -> 'a list -> bool

(** Return [None] if and only if [is_uniq] returns [true] on the given list.
 * If returning [Some], the returned element is an element that would have
 * been removed by [sort]. **)
val is_uniq_witness : ?cmp:('a -> 'a -> int) -> 'a list -> 'a option

(** Shuffle a list. **)
val shuffle : 'a list -> 'a list

(** Shuffle an array. **)
val array_shuffle : 'a array -> 'a array

(** Pattern match the list in the right instead of the left. **)
val list_match_right : 'a list -> ('a list * 'a) option

(** Return a list such that [(a, b)] is in the final list iff [a] was in the first list
 * and [b] in the second list. **)
val list_square : 'a list -> 'b list -> ('a * 'b) list


(** Swap the given pair. **)
val swap : 'a * 'b -> 'b * 'a

(** Sort the two given elements. **)
val pair_sort : 'a * 'a -> 'a * 'a

(** Some projecting functions **)
val fst3 : 'a * 'b * 'c -> 'a
val snd3 : 'a * 'b * 'c -> 'b
val fst4 : 'a * 'b * 'c * 'd -> 'a
val snd4 : 'a * 'b * 'c * 'd -> 'b

(** Return the positive modulo. **)
val positive_mod : int -> int -> int

(** The square of its argument. **)
val square : int -> int

(** Return a random number between its two arguments, included. **)
val rand : int -> int -> int

(** Take a list and return a random element from it.
 * Returns [EmptyList] if called on an empty list. **)
val select_any : 'a list -> 'a

(** Similar to select_any, but it removes the element from the list. **)
val take_any : 'a list -> 'a * 'a list

(** Take a weighted list and return a random element from it. **)
val select : (int * 'a) list -> 'a

(** Similar to select, but it removes the element from the list. **)
val take : (int * 'a) list -> 'a * (int * 'a) list

(** Possible exception returned by the selecting functions. **)
exception EmptyList
exception NegativeWeigth
exception InternalError

(** Sum the integers of the list. **)
val sum : int list -> int

(** Sum the integers of the array. **)
val array_sum : int array -> int

(** COmpute the average number of the list. **)
val average : int list -> int

(** Indicate how many elements of the list satisfy the predicates. **)
val count : ('a -> bool) -> 'a list -> int

(** Indicate how many elements of the array satisfy the predicates. **)
val array_count : ('a -> bool) -> 'a array -> int

(** Similar to [Array.fold_left], but the function takes the current index
 * as argument. **)
val array_fold_lefti : (int -> 'a -> 'b -> 'a) -> 'a -> 'b array -> 'a

(** Fold through both array. They must have the same size. **)
val array_fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b array -> 'c array -> 'a


(** Flatten a map into a list. **)
val pmap_to_list : ('a, 'b) PMap.t -> ('a * 'b) list


(** Given a prefix, a string, and a size, concat the prefix and the string
 * up to when the string is larger to or the same size than the given size. **)
val complete_string_pre : string -> string -> int -> string

(** Same than [complete_string_pre], but the first argument is now a postfix
 * and no longer a prefix. **)
val complete_string_post : string -> string -> int -> string

(** Similar to [String.split_on_char], but returns an enumeration. **)
val enum_split_on_char : char -> string -> string Enum.t

(** Lazyness is inherent to enumerations: this function creates a lazy enumeration
 * from a lazy value. **)
val lazy_enum : 'a Enum.t Lazy.t -> 'a Enum.t

