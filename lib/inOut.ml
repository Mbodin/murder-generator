
type layout =
  | Normal
  | Centered
  | Inlined

type cell_option = {
    row : int ;
    col : int ;
    classes : string list
  }

let default = {
    row = 1 ;
    col = 1 ;
    classes = []
  }

type 'node block =
  | Div of layout * 'node block list
  | P of 'node block list
  | List of bool * 'node block list
  | Space
  | Text of string
  | FoldableBlock of bool * string * 'node block
  | Link of string * string
  | LinkContinuation of bool * string * (unit -> unit)
  | LinkFile of string * string * string * bool * (unit -> string)
  | Table of string list
             * ('node block * cell_option) list
             * (string list * ('node block * cell_option) list) list
  | Node of 'node

let rec add_spaces =
  let need_space = function
    | Div _ -> false
    | P _ -> false
    | List _ -> false
    | Space -> false
    | Text _ -> true
    | FoldableBlock _ -> true
    | Link _ -> true
    | LinkContinuation _ -> true
    | LinkFile _ -> true
    | Table _ -> false
    | Node _ -> false in
  let rec aux = function
    | [] -> []
    | a :: [] -> a :: []
    | a :: b :: l ->
      if need_space a && need_space b then
        a :: Text " " :: aux (b :: l)
      else a :: aux (b :: l) in
  let aux l = aux (List.map add_spaces l) in function
    | Div (layout, l) -> Div (layout, aux l)
    | P l -> P (aux l)
    | List (visible, l) -> List (visible, List.map add_spaces l)
    | Table (classes, h, l) ->
      Table (classes, List.map (fun (b, o) -> (add_spaces b, o)) h,
             List.map (fun (classes, l) ->
               (classes, List.map (fun (b, o) -> (add_spaces b, o)) l)) l)
    | e -> e

module type T = sig

  val pause : unit -> unit Lwt.t
  val stopLoading : unit -> unit Lwt.t
  val startLoading : unit -> unit Lwt.t

  val set_printing_mode : unit -> unit
  val unset_printing_mode : unit -> unit

  val log : string -> unit

  val get_file : string -> string Lwt.t

  val get_parameters : unit -> (string * string) list
  val set_parameters : (string * string) list -> unit

  val languages : string list

  type node

  val block_node : node block -> node
  val print_node : ?error:bool -> node -> unit
  val print_block : ?error:bool -> node block -> unit
  val clear_response : unit -> unit

  val createNumberOutput : int -> node * (int -> unit)
  val createTextOutput : string -> node * (string -> unit)

  val createNumberInput : ?min:int -> ?max:int -> int -> node * (unit -> int)
  val createTextInput : string -> node * (unit -> string)
  val createSettableTextInput : string -> node * (unit -> string) * (string -> unit)
  val createListInput : (string * 'a) list -> node * (unit -> 'a option)
  val createResponsiveListInput : (string * 'a) list -> string -> (string -> (string * 'a) list) -> node * (unit -> 'a list)
  val createPercentageInput : float -> node * (unit -> float)
  val createDateInput : Date.t -> node * (unit -> Date.t)
  val createSwitch : string -> string option -> string option -> string option -> bool -> (unit -> unit) -> node * (bool -> unit) * (unit -> bool)
  val createFileImport : string list -> (unit -> unit Lwt.t) -> node * (unit -> (string * string) Lwt.t)

end

