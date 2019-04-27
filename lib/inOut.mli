(** Module InOut
 * Specifies an interface for inputs and outputs. **)

(** Specifies the different properties of div elements. **)
type layout =
  | Normal (** No special layout. **)
  | Centered (** Its content is centered. **)
  | Inlined (** The block is inlined. **)

(** A simplified representation of DOM’s nodes. **)
type 'node block =
  | Div of layout * 'node block list (** A div node, with its layout. **)
  | P of 'node block list (** A paragraph node. **)
  | List of bool * 'node block list
      (** A list of items.
       * The boolean indicates whether bullets should be drawn. **)
  | Space (** Some space between texts. **)
  | Text of string (** A simple text. **)
  | FoldableBlock of bool * string * 'node block
      (** A title that can hide a node.
       * The boolean states whether the block should be folded by
       * default. **)
  | Link of string * string (** A link and its associated address. **)
  | LinkContinuation of bool * string * (unit -> unit)
      (** A link and its associated continuation.
       * The boolean indicates whether the arrow is forwards. **)
  | LinkFile of string * string * string * bool * (unit -> string)
      (** Creates a link to a file whose content is computed.
       * The first argument is the link text, the second the file name,
       * the third the mime type, and the fifth its content.
       * The fourth indicates wether newlines should be adapted to the
       * host’s operating system or not. **)
  | Table of 'node block list * 'node block list list
    (** A table, with its headers and its content (given line by line). **)
  | Node of 'node (** For all cases where more control is needed,
                  * we can directly send a node. **)

(** Adds the expected spaces between block elements. **)
val add_spaces : 'node block -> 'node block

(** This is the signature specified in this file.
 * It is satisfied by the various files [lib/inOut_*.ml]. **)
module type T = sig

  (** Pauses the program for a short amount of time. **)
  val pause : unit -> unit Lwt.t

  (** Stops the loading animation. **)
  val stopLoading : unit -> unit Lwt.t

  (** Starts the loading animation. **)
  val startLoading : unit -> unit Lwt.t

  (** Fetch a file from an address and returns its content. **)
  val get_file : string -> string Lwt.t

  (** Log the given message. **)
  val log : string -> unit


  (** The local set of accepted languages. **)
  val languages : string list


  (** An abstract type for representing nodes. **)
  type node

  (** Converts the block to a node. **)
  val block_node : node block -> node

  (** Adds the node to the [response] div in the main webpage.
   * If [error] is [true], the node is highlighted as an error. **)
  val print_node : ?error:bool -> node -> unit

  (** A composition of [add_spaces], [block_node], and [print_node]. **)
  val print_block : ?error:bool -> node block -> unit

  (** Clears the [response] div in the main webpage. **)
  val clear_response : unit -> unit

  (** Create a (positive) number input with default value given as argument.
   * It also returns a function reading it. **)
  val createNumberInput : ?min:int -> ?max:int -> int -> node * (unit -> int)

  (** Create a text input with default value given as argument.
   * It also returns a function reading it. **)
  val createTextInput : string -> node * (unit -> string)

  (** Create a range input between [0.] and [1.].
   * As of [createNumberInput], it also returns a function reading it. **)
  val createPercentageInput : float -> node * (unit -> float)

  (** Create a date input.
   * As of [createNumberInput], it also returns a function reading it. **)
  val createDateInput : Date.t -> node * (unit -> Date.t)

  (** Create a switch button.
   * It takes a function that will be called at each state change as argument.
   * It also returns a setter and a getter.
   * The first string is the text associated with the button, to which can be
   * added two facultative texts: one added afterwards for when the button is
   * on, and one when the button is off. **)
  val createSwitch : string -> string option -> string option -> bool -> (unit -> unit) -> node * (bool -> unit) * (unit -> bool)

  (** Create a button to import a file.
   * It takes as argument the list of extension it accepts.
   * It also takes as argument a function called before loading the file into
   * memory.
   * As of the other reading functions, it returns the created element and
   * a reading function.
   * This reading function returns both the file name and its content. **)
  val createFileImport : string list -> (unit -> unit Lwt.t) -> node * (unit -> (string * string) Lwt.t)

end

