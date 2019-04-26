(** Module InOut_native
 * An implementation of [InOut.T] for native programs. **)


let pause = Lwt.return

let stopLoading = pause
let startLoading _ =
  print_newline () ;
  print_endline "..." ;
  pause ()

let get_file fileName =
  let file = open_in fileName in
  let rec aux _ =
    try let str = input_line file in
        str :: aux ()
    with End_of_file -> [] in
  Lwt.return (String.concat "\n" (aux ()))

let log msg =
  print_endline ("Log: " ^ msg)

let languages =
  Option.map_default (fun l -> [l]) [] (Sys.getenv_opt "LANG")

(** A module to print on screen with potential breaking points. **)
module type PrintType =
  sig

    (** The current screen size. **)
    val screen_size : unit -> int

    (** Update the current screen size. **)
    val set_screen_size : int -> unit

    (** Print the following string. **)
    val print : string -> unit

    (** Print a new line. **)
    val newline : unit -> unit

    (** Make sure that the current line is empty by printing a newline if needed. **)
    val clearline : unit -> unit

    (** Insert a breakpoint: if further printing go beyond the
     * length limit, this breakpoint will be replaced by a new line.
     * Two strings can be given: one to be printed at this place
     * when the breakpoint is not replaced by a newline, and one
     * to be printed if it has been replaced by a newline.
     * In the later case, two strings must be provided: one printed
     * before the newline, and one after.
     * No character “\n” or “\r” should appear in these strings. **)
    val breakpoint : ?normal:string -> ?break:(string * string) -> unit -> unit

    (** Insert a space, which may break into a newline. **)
    val space : unit -> unit

    (** Fill the rest of the line with this character. **)
    val separator : char -> unit

    (* TODO: Contexts are meant to start at the *next* line break,
     * which is not what is currently happening. *)
    (** Start a context: any further newlines will start by this string
     * (in addition to the ones provided by outer contexts). **)
    val push_prefix : string -> unit

    (** Add a context: any further content will end by this string. **)
    val push_suffix : string -> unit

    (** Add a context: any further content will be centered. **)
    val push_center : unit -> unit

    (** Remove the last provided context.
     * Each [pop] must corresponds to exactly a [push_*]. **)
    val pop : unit -> unit

  end

(** Implemenation of [PrintType]. **)
module Print : PrintType =
  struct

    let (screen_size, set_screen_size) =
      let screen_size = ref 100 in
      let get _ = !screen_size in
      let set size =
        screen_size := max 0 size in
      (get, set)

    (** A type to store contexts. **)
    type context =
      | Prefix of string (** A prefix is added to each string. **)
      | Suffix of string (** A suffix is added to each string. **)
      | Center (** Each string is centered. **)

    (** The current context, and how much space it takes. **)
    let context = ref ([], 0)

    (** How much space a given context takes. **)
    let context_size = function
      | Prefix str -> String.length str
      | Suffix str -> String.length str
      | Center -> 0

    (** Push a context. **)
    let push ctx =
      let (l, size) = !context in
      context := (ctx :: l, size + context_size ctx)

    let push_prefix str = push (Prefix str)

    let push_suffix str = push (Suffix str)

    let push_center _ = push Center

    let pop _ =
      match fst !context with
      | [] -> assert false
      | ctx :: l ->
        context := (l, snd !context - context_size ctx)

    (** Print the following string, going through all the current context. **)
    let print_line str =
      let rec aux taken = function
        | [] -> str
        | ctx :: l ->
          let str = aux (taken + context_size ctx) l in
          match ctx with
          | Prefix pre -> pre ^ str
          | Suffix suf -> str ^ suf
          | Center ->
            let size = max 0 (screen_size () - taken) in
            String.make (size / 2) ' ' ^ str ^ String.make (size - size / 2) ' ' in
      print_endline (aux 0 (List.rev (fst !context)))

    (** A type for breakpoints. **)
    type breakpoint = {
        normal : string (** What is meant to be displayed if the breakpoint
                         * does not break the line. **) ;
        break_before : string (** If the breakpoint breaks the line, what is
                               * meant to be displayed before the line break. **) ;
        break_after : string (** If the breakpoint breaks the line, what is
                              * meant to be displayed after the line break. **)
      }

    (** An empty breakpoint. **)
    let empty_breakpoint = {
        normal = "" ;
        break_before = "" ;
        break_after = ""
      }

    (** A structure containing the current state of the printer. **)
    type state = {
        text_before : string (** Text written in the current line,
                              * before the current breakpoint. **) ;
        breakpoint : breakpoint (** Current breakpoint. **) ;
        text_after : string (** Text written in the current line,
                             * after the current breakpoint. **)
      }

    (** The state as it is after a new line. **)
    let empty_state = {
        text_before = "" ;
        breakpoint = empty_breakpoint ;
        text_after = ""
      }

    (** The current state of the printer. **)
    let state = ref empty_state

    let separator c =
      let text =
        !state.text_before ^ !state.breakpoint.normal ^ !state.text_after in
      let text =
        let size =
          max 0 (screen_size () - snd !context - String.length text) in
        text ^ String.make size c in
      print_line text ;
      state := empty_state

    (** Indicate that we aim to print a string of the given size on the same
     * line, and break the line if needed and possible. **)
    let reserve_for size =
      if String.length !state.text_before
         + String.length !state.breakpoint.normal
         + String.length !state.text_after
         + size >= screen_size () - snd !context
         && (!state.text_before <> ""
             || !state.breakpoint.break_before <> ""
             || !state.breakpoint.break_after <> "") then (
        print_line (!state.text_before ^ !state.breakpoint.break_before) ;
        state := {
            text_before = "" ;
            breakpoint = empty_breakpoint ;
            text_after = !state.breakpoint.break_after ^ !state.text_after ;
          }
      )

    (** Consider whether the state should be partially printed because its
     * breakpoint has been activated, and do so if needed.
     * This function should be called whenever the state is modified with
     * a non-empty breakpoint. **)
    let normalize _ =
      reserve_for 0

    let print text =
      state := { !state with text_after = !state.text_after ^ text } ;
      normalize ()

    let newline _ =
      print_line
        (!state.text_before ^ !state.breakpoint.normal ^ !state.text_after) ;
      state := empty_state

    let clearline _ =
      if !state.text_before <> ""
         || !state.breakpoint <> empty_breakpoint
         || !state.text_after <> "" then newline ()

    let breakpoint ?(normal = "") ?(break = ("", "")) _ =
      reserve_for (String.length normal) ;
      state := {
          text_before =
            !state.text_before ^ !state.breakpoint.normal ^ !state.text_after ;
          breakpoint = {
            normal = normal ;
            break_before = fst break ;
            break_after = snd break
            } ;
          text_after = ""
        }

    let space _ = breakpoint ~normal:" " ()

  end


(** A node is just a function to print this node.
 * It takes as a argument a link printer: whenever the node item wants
 * to print a link, it calls this function, which will insert a specific
 * string for the user to know how to call this particular link.
 * This function is itself given a function to update the value of
 * the node item. **)
type node =
  ((unit -> unit) -> string) -> unit

type layout =
  | Normal
  | Centered
  | Inlined

type block =
  | Div of layout * block list
  | P of block list
  | List of bool * block list
  | Space
  | Text of string
  | Link of string * string
  | LinkContinuation of bool * string * (unit -> unit)
  | LinkFile of string * string * string * bool * (unit -> string)
  | Table of block list * block list list
  | Node of node

let rec add_spaces =
  let need_space = function
    | Div _ -> false
    | P _ -> false
    | List _ -> false
    | Space -> false
    | Text _ -> true
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
    | Table (h, l) ->
      Table (List.map add_spaces h, List.map (List.map add_spaces) l)
    | e -> e

let rec block_node b link =
  match b with
  | Div (layout, l) ->
    if layout <> Inlined then Print.clearline () ;
    let pop =
      match layout with
      | Normal ->
        Print.push_prefix " " ;
        fun _ ->
          Print.pop () ;
          Print.clearline ()
      | Centered ->
        Print.push_prefix " " ;
        Print.push_center () ;
        fun _ ->
          Print.pop () ;
          Print.pop () ;
          Print.clearline ()
      | Inlined -> Utils.id in
    List.iter (fun b -> block_node b link) l ;
    pop ()
  | P l ->
    Print.clearline () ;
    Print.push_prefix " " ;
    Print.print "  " ;
    List.iter (fun b -> block_node b link) l ;
    Print.pop () ;
    Print.clearline () ;
  | List (drawn, l) ->
    List.iter (fun b ->
      if drawn then (
        Print.clearline () ;
        Print.print "* " ;
        Print.push_prefix "  " ;
        block_node b link ;
        Print.pop ()
      ) else (
        Print.clearline () ;
        block_node b link
      )) l
  | Space ->
    Print.breakpoint ~normal:"  " ()
  | Text str ->
    List.iteri (fun i str ->
      if i <> 0 then Print.space () ;
      Print.print str) (String.split_on_char ' ' str)
  | Link (text, address) ->
    let text_link = link (fun _ -> Print.print address) in
    block_node (Text (text ^ " " ^ text_link)) link
  | LinkContinuation (forward, text, cont) ->
    let text_link = link cont in
    let str =
      if forward then text ^ " " ^ text_link
      else text_link ^ " " ^ text in
    block_node (Text str) link
  | LinkFile (text, fileName, mime, newlines, content) ->
    block_node (LinkContinuation (true, text, fun _ ->
      print_string ("[" ^ fileName ^ "] > ") ;
      flush stdout ;
      let fileName =
        let str = input_line stdin in
        if str = "" then fileName else str in
      let channel = open_out fileName in
      output_string channel (content ()) ;
      close_out channel)) link
  | Table (headers, content) ->
    block_node (Text "<table>") link (* TODO *)
  | Node node -> node link

(** Actions linked to each link. **)
let links = ref []

(** Registered nodes to be printed at each screen. **)
let registered_nodes = ref BidirectionalList.empty

(** The default [link] function. **)
let link f =
  let n = List.length !links in
  links := !links @ [f] ;
  "[" ^ string_of_int n ^ "]"

(** Ask for a number from the user. **)
let numberInput get set =
  print_string (string_of_int (get ()) ^ " -> ") ;
  flush stdout ;
  let str = input_line stdin in
  let v =
    try int_of_string str
    with _ -> print_endline "Invalid value."; get () in
  set v

(** Clear the registered links. **)
let clear_links _ =
  links := [fun _ -> numberInput Print.screen_size Print.set_screen_size]

(** Starting the server. **)
let _ =
  let rec aux _ =
    clear_links () ;
    (** Print all registered nodes. **)
    List.iter (fun b -> b link) (BidirectionalList.to_list !registered_nodes) ;
    (** Wait for user input. **)
    match%lwt Lwt_io.read_line_opt Lwt_io.stdin with
    | None -> Lwt.return ()
    | Some str ->
      let i =
        try int_of_string str
        with _ -> -1 in
      match try List.nth_opt !links i
            with _ -> None with
      | None ->
        print_endline ("Invalid number " ^ string_of_int i ^ ".") ;
        aux ()
      | Some f ->
        print_string ("[" ^ string_of_int i ^ "]: ") ;
        f () ;
        aux () in
  aux ()

let print_node ?(error = false) n =
  let symbol = if error then '!' else '-' in
  let header _ =
    Print.clearline () ;
    Print.separator symbol in
  let n link =
    header () ;
    Print.push_prefix (String.make 1 symbol ^ " ") ;
    n link ;
    Print.pop () ;
    header () in
  registered_nodes := BidirectionalList.add_right !registered_nodes n ;
  n link

let print_block ?(error = false) =
  Utils.compose (print_node ~error) (Utils.compose block_node add_spaces)

let createNumberInput ?min:(mi = 0) ?max:(ma = max_int) n =
  let v = ref n in
  let node link =
    Print.print (" " ^ string_of_int !v ^ " " ^ link (fun _ ->
      numberInput (fun _ -> !v) (fun v' -> v := v') ;
      v := max mi (min !v ma))) in
  (node, fun _ -> !v)

let createTextInput str =
  let txt = ref str in
  let node link =
    Print.newline () ;
    Print.print ("<" ^ !txt ^ "> " ^ link (fun _ ->
      print_string ("<" ^ !txt ^ "> -> ") ;
      flush stdout ;
      let str = input_line stdin in
      txt := str)) in
  (node, fun _ -> !txt)

let createPercentageInput d =
  let d = max 0. (min 1. d) in
  let v = ref (100. *. d) in
  let node link =
    Print.newline () ;
    Print.print (string_of_float !v ^ "% " ^ link (fun _ ->
      print_string (string_of_float !v ^ "% -> ") ;
      flush stdout ;
      let str = input_line stdin in
      let str =
        let len = String.length str in
        if len > 0 && str.[len - 1] = '%' then
          String.sub str 0 (len - 1)
        else str in
      let v' =
        try float_of_string str
        with _ -> print_endline "Invalid value."; !v in
      v := max 0. (min v' 100.))) in
  (node, fun _ -> !v /. 100.)

let createDateInput d =
  let v = ref d in
  let node link =
    Print.newline () ;
    Print.print (Date.iso8601 !v ^ " " ^ link (fun _ ->
      print_string (Date.iso8601 !v ^ " -> ") ;
      flush stdout ;
      let str = input_line stdin in
      let v' =
        try Date.from_iso8601 str
        with _ -> print_endline "Invalid date."; !v in
      v := v')) in
  (node, fun _ -> !v)

let createSwitch text texton textoff b f =
  let b = ref b in
  let node link =
    Print.newline () ;
    let text =
      (if !b then "[X]" else "[ ]") ^ " "
      ^ link (fun _ -> b := not !b ; print_newline () ; f ())
      ^ " " ^ text
      ^ Option.map_default (fun str -> " " ^ str) ""
          (if !b then texton else textoff) in
    block_node (Text text) link in
  (node, (fun v -> b := v), fun _ -> !b)

let createFileImport extensions prepare =
  let file = ref "" in
  let extensions =
    let text =
      String.concat "," (List.map (fun str -> "*." ^ str) extensions) in
    if text = "" then "*" else text in
  let node link =
    Print.newline () ;
    Print.print ("<" ^ (if !file = "" then extensions else !file) ^ "> "
                 ^ link (fun _ ->
      print_string ("<" ^ extensions ^ "> -> ") ;
      flush stdout ;
      file := input_line stdin)) in
  (node, fun _ ->
    prepare () ;%lwt
    let file = !file in
    let%lwt content = get_file file in
    Lwt.return (file, content))

let clear_response _ =
  clear_links () ;
  registered_nodes := BidirectionalList.empty ;
  print_newline () ;
  Print.separator '='

