(** Module InOut
 * Contains functions for intputs and outputs. **)

open Js_of_ocaml


(** Fetch a file from an adress and returns its content. **)
val get_file : string -> string Lwt.t

val document : Dom_html.document Js.t

(** A simplified representation of DOM’s nodes. **)
type block =
  | Div of block list (** A div node. **)
  | P of bool * block list
      (** A paragraph node.
       * The boolean indicates whether its content is centered. **)
  | List of bool * block list
      (** A list of items.
       * The boolean indicates whether bullets should be drawn. **)
  | Space (** Some space between text **)
  | Text of string (** A simple text. **)
  | Link of string * string (** A link and its associated address. **)
  | LinkContinuation of bool * string * (unit -> unit)
      (** A link and its associated continuation.
       * The boolean indicates whether the arrow is forwards. **)
  | Node of Dom_html.element Js.t (** For all cases where more control is needed,
                                   * we can directly send a node. **)

(** Adds the expected spaces between block elements. **)
val add_spaces : block -> block

(** Converts the block to a node. **)
val block_node : block -> Dom_html.element Js.t

(** Adds the node to the [response] div in the main webpage. **)
val print_node : Dom.node Js.t -> unit

(** A composition of [add_spaces], [block_node], and [print_node]. **)
val print_block : block -> unit

(** Clears the [response] div in the main webpage. **)
val clear_response : unit -> unit

(** Create a (positive) number input with default value given as argument.
 * It also returns a function reading it. **)
val createNumberInput : int -> Dom_html.element Js.t * (unit -> int)

(** Create a range input between [0.] and [1.].
 * As of [createNumberInput], it also returns a function reading it. **)
val createPercentageInput : float -> Dom_html.element Js.t * (unit -> float)

(** Create a switch button.
 * It takes a function that will be called at each state change as argument.
 * It also returns a setter and a getter. **)
val createSwitch : bool -> (unit -> unit) -> Dom_html.element Js.t * (bool -> unit) * (unit -> bool)

