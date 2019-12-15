(** Module InOut_native
 * An implementation of [InOut.T] for native programs. **)

(** Length of utf-8-encoded string. **)
let unicode_length =
  Uuseg_string.fold_utf_8 `Grapheme_cluster (fun x _ -> x + 1) 0

let pause = Lwt.return

let stopLoading = pause
let startLoading _ =
  print_newline () ;
  print_endline "..." ;
  pause ()
let setLoading =
  let previous = ref 0 in fun p ->
  let p = max 0. p in
  let p = min 1. p in
  let p = 100. *. p in
  let p = int_of_float p in
  if p <> !previous then
    print_endline ("... " ^ string_of_int p ^ "%") ;
  previous := p ;
  pause ()

let set_printing_mode = Utils.id
let unset_printing_mode = Utils.id


let get_file fileName =
  let file = open_in fileName in
  let rec aux _ =
    try let str = input_line file in str :: aux ()
    with End_of_file -> [] in
  Lwt.return (String.concat "\n" (aux ()))

let get_parameters =
  let rec aux = function
    | [] -> []
    | str :: l when String.length str = 0 -> assert false
    | key :: value :: l when key.[0] = '-' ->
      let key = String.sub key 1 (String.length key - 1) in
      (key, value) :: aux l
    | str :: l ->
      match String.split_on_char '=' str with
      | key :: value -> (key, String.concat "=" value) :: aux l
      | _ -> aux l in
  let l = lazy (aux (List.tl (Array.to_list Sys.argv))) in
  fun () -> Lazy.force l
let set_parameters = ignore

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
      | Prefix str -> unicode_length str
      | Suffix str -> unicode_length str
      | Center -> 0

    (** Push a context. **)
    let push ctx =
      let (l, size) = !context in
      context := (ctx :: l, size + context_size ctx)

    let push_prefix str = push (Prefix str)

    let push_suffix str = push (Suffix str)

    let pop _ =
      match fst !context with
      | [] -> assert false
      | ctx :: l ->
        context := (l, snd !context - context_size ctx)

    (** Print the following string, going through all the current context. **)
    let print_line context str =
      let rec aux taken = function
        | [] -> str
        | ctx :: l ->
          let taken = taken + context_size ctx in
          let str = aux taken l in
          let size = max 0 (screen_size () - taken - unicode_length str) in
          match ctx with
          | Prefix pre -> pre ^ str
          | Suffix suf -> str ^ String.make size ' ' ^ suf
          | Center ->
            String.make (size / 2) ' ' ^ str ^ String.make (size - size / 2) ' ' in
      print_endline (aux 0 (List.rev (fst context)))

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
        state_context : context list * int (** Value of [!context] when
                                            * the line started. **) ;
        text_before : string (** Text written in the current line,
                              * before the current breakpoint. **) ;
        breakpoint : breakpoint (** Current breakpoint. **) ;
        text_after : string (** Text written in the current line,
                             * after the current breakpoint. **)
      }

    (** The state as it is after a new line. **)
    let empty_state _ = {
        state_context = !context ;
        text_before = "" ;
        breakpoint = empty_breakpoint ;
        text_after = ""
      }

    (** The current state of the printer. **)
    let state = ref (empty_state ())

    let separator c =
      let text =
        !state.text_before ^ !state.breakpoint.normal ^ !state.text_after in
      let text =
        let size =
          max 0 (screen_size () - snd !state.state_context - unicode_length text) in
        text ^ String.make size c in
      print_line !state.state_context text ;
      state := empty_state ()

    (** Indicate that we aim to print a string of the given size on the same
     * line, and break the line if needed and possible. **)
    let reserve_for size =
      if unicode_length !state.text_before
         + unicode_length !state.breakpoint.normal
         + unicode_length !state.text_after
         + size >= screen_size () - snd !state.state_context
         && (!state.text_before <> ""
             || !state.breakpoint.break_before <> ""
             || !state.breakpoint.break_after <> "") then (
        print_line !state.state_context
          (!state.text_before ^ !state.breakpoint.break_before) ;
        state := {
            state_context = !context ;
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
      print_line !state.state_context
        (!state.text_before ^ !state.breakpoint.normal ^ !state.text_after) ;
      state := empty_state ()

    let clearline _ =
      if !state.text_before <> ""
         || !state.breakpoint <> empty_breakpoint
         || !state.text_after <> "" then newline () ;
      state := { !state with state_context = !context }

    let push_center _ =
      push Center ;
      clearline ()

    let breakpoint ?(normal = "") ?(break = ("", "")) _ =
      reserve_for (unicode_length normal) ;
      state := {
          state_context = !state.state_context ;
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

type ('a, 'b) interaction = {
    node : node ;
    get : unit -> 'b ;
    set : 'a -> unit ;
    onChange : ('a -> unit) -> unit ;
    lock : unit -> unit ;
    unlock : unit -> unit ;
    locked : unit -> bool ;
    onLockChange : (bool -> unit) -> unit
  }

type 'a sinteraction = ('a, 'a) interaction

(** Similar to [lock] and [unlock], but from a boolean. **)
let lockMatch n = function
  | true -> n.lock ()
  | false -> n.unlock ()

let synchronise i1 i2 =
  i2.set (i1.get ()) ;
  i1.onChange i2.set ;
  i2.onChange i1.set ;
  i1.onLockChange (lockMatch i2) ;
  i2.onLockChange (lockMatch i1)

(** Creates a menu and an interaction-creating function.
 * The menu is a wrapper around link to call the functions given to [onChange] each time
 * the link is activated before calling the corresponding function. **)
let createMenu get =
  let l = ref [] in
  let onChange f = l := f :: !l in
  let locked = ref false in
  let onLock = ref [] in
  let onLockChange f = onLock := f :: !onLock in
  let current = ref (get ()) in
  let trigger _ =
    let v = get () in
    if v <> !current then (
      current := v ;
      List.iter (fun f -> f v) !l
    ) in
  let menu link f =
    if !locked then ""
    else
      link (fun _ ->
        f () ;
        trigger ()) in
  let trigger set x =
    set x ;
    trigger () in
  let triggerLock status _ =
    if !locked <> status then (
      locked := status ;
      List.iter (fun f -> f status) !onLock
    ) in
  (menu, fun node get set -> {
       node = node ;
       get = get ;
       set = trigger set ;
       onChange = onChange ;
       lock = triggerLock true ;
       unlock = triggerLock false ;
       locked = (fun _ -> !locked) ;
       onLockChange = onLockChange
     })

let rec block_node b =
  match b with
  | InOut.Div (layout, l) ->
    let l = List.map block_node l in fun link ->
    if layout <> Inlined then Print.clearline () ;
    let pop =
      match layout with
      | InOut.Normal ->
        Print.push_prefix " " ;
        fun _ ->
          Print.pop () ;
          Print.clearline ()
      | InOut.Centered ->
        Print.push_prefix " " ;
        Print.push_center () ;
        fun _ ->
          Print.pop () ;
          Print.pop () ;
          Print.clearline ()
      | InOut.Inlined -> Utils.id in
    List.iter (fun b -> b link) l ;
    pop ()
  | InOut.P l ->
    let l = List.map block_node l in fun link ->
    Print.clearline () ;
    Print.push_prefix " " ;
    Print.print "  " ;
    List.iter (fun b -> b link) l ;
    Print.pop () ;
    Print.clearline () ;
  | InOut.List (drawn, l) ->
    let l = List.map block_node l in fun link ->
    List.iter (fun b ->
      if drawn then (
        Print.clearline () ;
        Print.print "* " ;
        Print.push_prefix "  " ;
        b link ;
        Print.pop ()
      ) else (
        Print.clearline () ;
        b link
      )) l
  | InOut.Space -> fun link ->
    Print.breakpoint ~normal:"  " ()
  | InOut.Text str -> fun link ->
    (* LATER: Use [Uuseg_string.fold_utf_8 `Line_break]
     * instead of [split_on_char]. *)
    List.iteri (fun i str ->
      if i <> 0 then Print.space () ;
      Print.print str) (String.split_on_char ' ' str)
  | InOut.FoldableBlock (visible, title, node) ->
    let visible = ref visible in
    let node = block_node node in fun link ->
    Print.clearline () ;
    Print.print ((if !visible then "-" else "+") ^ " ") ;
    Print.push_prefix "  " ;
    let text_link = link (fun _ -> visible := not !visible; print_newline ()) in
    block_node (InOut.Text (text_link ^ " " ^ title)) link ;
    if !visible then (
      Print.newline () ;
      node link
    ) ;
    Print.pop ()
  | InOut.LinkContinuation (forward, style, text, cont) -> fun link ->
    let text_link = link cont in
    let text =
      match style with
      | InOut.Simple -> text
      | InOut.Button _ -> "[[" ^ text ^ "]]" in
    let str =
      if forward then text ^ " " ^ text_link
      else text_link ^ " " ^ text in
    block_node (InOut.Text str) link
  | InOut.LinkExtern (style, text, address) ->
    let cont _ = print_endline address in
    block_node (InOut.LinkContinuation (true, style, text, cont))
  | InOut.LinkFile (style, text, fileName, mime, newlines, content) -> fun link ->
    block_node (InOut.LinkContinuation (true, style, text, fun _ ->
      print_string ("[" ^ fileName ^ "] > ") ;
      flush stdout ;
      let fileName =
        let str = input_line stdin in
        if str = "" then fileName else str in
      let channel = open_out fileName in
      output_string channel (content ()) ;
      close_out channel)) link
  | InOut.Table (classes, headers, content) -> fun link ->
    let print_line l =
      Print.push_prefix ("-") ;
      Print.push_suffix ("-") ;
      Print.clearline () ;
      Print.separator '-' ;
      List.iter (fun (n, _) ->
        if n <> InOut.Text "" then (
          Print.push_center () ;
          block_node n link ;
          Print.clearline () ;
          Print.pop () ;
          Print.separator '-')) l ;
      Print.pop () ;
      Print.pop () in
    Print.push_prefix (" =") ;
    Print.push_suffix ("= ") ;
    Print.clearline () ;
    Print.separator '=' ;
    print_line headers ;
    Print.clearline () ;
    Print.separator '=' ;
    Print.clearline () ;
    Print.separator '=' ;
    List.iter (fun (classes, line) ->
      print_line line ;
      Print.clearline () ;
      Print.separator '=') content ;
    Print.pop () ;
    Print.pop ()
  | InOut.Node node -> node

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
  Print.push_prefix "=" ;
  Print.push_suffix "=" ;
  let rec aux _ =
    clear_links () ;
    (** Print all registered nodes. **)
    List.iter (fun b -> b link) (BidirectionalList.to_list !registered_nodes) ;
    (** Wait for user input. **)
    match%lwt Lwt_io.read_line_opt Lwt_io.stdin with
    | None -> exit 0
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
    Print.push_suffix (String.make 1 symbol) ;
    n link ;
    Print.pop () ;
    Print.pop () ;
    header () in
  registered_nodes := BidirectionalList.add_right !registered_nodes n ;
  n link

let print_block ?(error = false) =
  Utils.compose (print_node ~error) (Utils.compose block_node InOut.add_spaces)

let createNumberInput ?min:(mi = 0) ?max:(ma = max_int) n =
  let v = ref (max mi (min ma n)) in
  let get _ = !v in 
  let (menu, create) = createMenu get in
  let node link =
    Print.print (" " ^ string_of_int !v ^ " " ^ menu link (fun _ ->
      numberInput (fun _ -> !v) (fun v' -> v := v') ;
      v := max mi (min !v ma))) in
  let set n = v := (max mi (min ma n)) in
  create node get set

let createTextInput str =
  let txt = ref str in
  let get _ = !txt in 
  let (menu, create) = createMenu get in
  let node link =
    Print.print (" <" ^ !txt ^ "> " ^ menu link (fun _ ->
      print_string ("<" ^ !txt ^ "> -> ") ;
      flush stdout ;
      let str = input_line stdin in
      txt := str) ^ " ") in
  let set str = txt := str in
  create node get set

let createListInput l =
  if l = [] then
    let node link = Print.print " <>" in {
      node = node ;
      get = (fun _ -> None) ;
      set = (fun _ -> invalid_arg "createListInput: set on an empty list.") ;
      onChange = (fun _ -> invalid_arg "createListInput: onChange on an empty list.") ;
      lock = ignore ;
      unlock = ignore ;
      locked = (fun _ -> true) ;
      onLockChange = (fun _ -> invalid_arg "createListInput: onLockChange on an empty list.")
    }
  else
    let index = ref 0 in
    let get _ = List.nth_opt l !index in
    let get_str _ =
      match List.nth_opt l !index with
      | Some (k, _) -> k
      | None -> invalid_arg "createListInput: onChange with a selected index too big." in
    let (menu, create) = createMenu get_str in
    let node link =
      let txt = get_str () in
      Print.print (" <" ^ txt ^ "> " ^ menu link (fun _ ->
        List.iteri (fun i (txt, _) ->
          print_endline (string_of_int i ^ ": " ^ txt)) l ;
        flush stdout ;
        numberInput (fun _ -> !index) (fun i ->
          if i >= 0 && i < List.length l then
            index := i
          else print_endline "Invalid number.")) ^ " ") in
    let set k =
      match Utils.list_associ_opt k l with
      | None -> invalid_arg "createListInput: set on an non-existing element."
      | Some (i, _) -> index := i in
    create node get set

let synchroniseListInput i1 i2 =
  (match i1.get () with
   | None -> ()
   | Some (k, _) -> i2.set k) ;
  i1.onChange i2.set ;
  i2.onChange i1.set ;
  i1.onLockChange (lockMatch i2) ;
  i2.onLockChange (lockMatch i1)


let createResponsiveListInput default _ get_possibilities =
  let l = ref default in
  let remove txt =
    l := List.filter (fun (txt', _) -> txt <> txt') !l in
  let get _ = !l in
  let (menu, create) = createMenu get in
  let print link =
    String.concat " " (List.map (fun (txt, _) ->
      "<" ^ txt ^ " [X]" ^ menu link (fun _ -> remove txt) ^ ">") !l) in
  let rec add _ =
    print_string ("<> -> ") ;
    flush stdout ;
    let str = input_line stdin in
    let possibilities = get_possibilities str in
    print_endline "[0] -> <>" ;
    List.iteri (fun i (txt, _) ->
      print_endline ("[" ^ string_of_int (1 + i) ^ "] " ^ txt)) possibilities ;
    print_string "-> " ;
    flush stdout ;
    let response = input_line stdin in
    try
      let v = int_of_string response in
      if v = 0 then add ()
      else
        let (txt, e) = List.nth possibilities (v - 1) in
        remove txt ;
        l := (txt, e) :: !l
    with _ -> print_endline "Invalid value." in
  let node link =
    Print.print (" <" ^ print link ^ "> " ^ menu link add ^ " ") in
  let set l' = l := l' in
  create node get set

let createPercentageInput d =
  let d = max 0. (min 1. d) in
  let v = ref (100. *. d) in
  let set d =
    let d = max 0. (min 1. d) in
    v := 100. *. d in
  let get _ = !v /. 100. in
  let (menu, create) = createMenu get in
  let node link =
    Print.print (" " ^ string_of_float !v ^ "% " ^ menu link (fun _ ->
      print_string (string_of_float !v ^ "% -> ") ;
      flush stdout ;
      let str = input_line stdin in
      let str =
        let len = unicode_length str in
        if len > 0 && str.[len - 1] = '%' then
          String.sub str 0 (len - 1)
        else str in
      let v' =
        try float_of_string str
        with _ -> print_endline "Invalid value."; !v in
      v := max 0. (min v' 100.)) ^ " ") in
  create node get set

let createDateInput d =
  let v = ref d in
  let get _ = !v in
  let (menu, create) = createMenu get in
  let node link =
    Print.print (" " ^ Date.iso8601 !v ^ " " ^ menu link (fun _ ->
      print_string (Date.iso8601 !v ^ " -> ") ;
      flush stdout ;
      let str = input_line stdin in
      let v' =
        try Date.from_iso8601 str
        with _ -> print_endline "Invalid date."; !v in
      v := v') ^ " ") in
  let set d = v := d in
  create node get set

let createSwitch text descr texton textoff b =
  let b = ref b in
  let get _ = !b in
  let (menu, create) = createMenu get in
  let node link =
    let text =
      " [" ^ (if !b then "X" else " ") ^ "] "
      ^ menu link (fun _ -> b := not !b ; print_newline ())
      ^ (if text <> "" then " " ^ text else "")
      ^ Option.map_default (fun str -> " " ^ str) "" descr
      ^ Option.map_default (fun str -> " " ^ str) ""
          (if !b then texton else textoff) ^ " " in
    block_node (InOut.Text text) link in
  let set b' = b := b' in
  create node get set

let createFileImport extensions prepare =
  let file = ref "" in
  let extensions =
    let text =
      String.concat "," (List.map (fun str -> "*." ^ str) extensions) in
    if text = "" then "*" else text in
  let node link =
    Print.print (" <" ^ (if !file = "" then extensions else !file) ^ "> "
                 ^ link (fun _ ->
      print_string ("<" ^ extensions ^ "> -> ") ;
      flush stdout ;
      file := input_line stdin) ^ " ") in
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

let createTextOutput str =
  let txt = ref str in
  let node link = Print.print !txt in
  (node, fun str -> txt := str)

let createNumberOutput n =
  let (node, set) = createTextOutput (string_of_int n) in
  (node, fun n -> set (string_of_int n))

