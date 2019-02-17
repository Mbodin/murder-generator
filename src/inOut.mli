(** Module InOut
 * Contains functions for intputs and outputs. **)

(** Fetch a file from an adress and returns its content. **)
val get_file : string -> string Lwt.t

val document : Dom_html.document Js.t

(** A simplified representation of DOM’s nodes. **)
type block =
  | Div of block list (** A div node. **)
  | P of block list (** A paragraph node. **)
  | CenterP of block list (** A paragraph node whose content is centered. **)
  | Text of string (** A simple text. **)
  | Link of string * string (** A link and its associated address. **)
  | LinkContinuation of string * (unit -> unit) (** A link and its associated
                                                 * continuation. **)
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

