(** Module InOut_js
 * An implementation of [InOut.T] for JavaScript. **)

open Js_of_ocaml
open Js_of_ocaml_lwt


let pause _ = Lwt_js.sleep 0.02

(** Whether the loading animation is currently running. **)
let loading = ref true
let loadingProgress = ref 0.2

let stopLoading _ =
  if !loading then (
    ignore (Js.Unsafe.fun_call (Js.Unsafe.js_expr "stopLoading") [||]) ;
    loading := false) ;
  Lwt_js.yield ()

let callStartLoading _ =
  ignore (Js.Unsafe.fun_call (Js.Unsafe.js_expr "startLoading")
            [| Js.Unsafe.inject (Js.number_of_float !loadingProgress) |])

let startLoading _ =
  if not !loading then (
    callStartLoading () ;
    loading := true) ;
  pause ()

let setLoading p =
  if !loadingProgress <> p && !loading then callStartLoading () ;
  loadingProgress := p ;
  pause ()


let log msg = Firebug.console##log (Js.string msg)

let get_body _ =
  let l = Dom_html.document##getElementsByTagName (Js.string "body") in
  if l##.length = 0 then
    failwith "No [body] element found in the webpage." ;
  if l##.length > 1 then
    log ("More than one [body] element found in the webpage."
         ^ "  Taking the first one.") ;
  match Js.Opt.to_option (l##item 0) with
  | None -> assert false
  | Some body -> body

let set_printing_mode _ =
  (get_body ())##.classList##add (Js.string "printable")

let unset_printing_mode _ =
  (get_body ())##.classList##remove (Js.string "printable")


let get_file url =
  let (res, w) = Lwt.task () in
  let request = XmlHttpRequest.create () in
  request##_open (Js.string "GET") (Js.string url) Js._true ;
  request##overrideMimeType (Js.string "text/plain") ;
  request##.onreadystatechange :=
    Js.wrap_callback (fun _ ->
      if request##.readyState = XmlHttpRequest.DONE then (
        if request##.status = 200 then (
          match Js.Opt.to_option request##.responseText with
          | Some txt -> Lwt.wakeup_later w (Js.to_string txt)
          | None ->
            Lwt.wakeup_later_exn w (Invalid_argument
                 ("Error when fetching " ^ url
                  ^ ": correct status request, but no provided response: "
                  ^ Js.to_string request##.statusText))
        ) else
          Lwt.wakeup_later_exn w (Invalid_argument
               ("Error when fetching " ^ url ^ ". "
                ^ string_of_int request##.status ^ " :"
                ^ Js.to_string request##.statusText)))) ;
  request##send Js.null ;
  res

let url_replacements = [
    ('_', 'o') (** Must be the first one as we use it to encode all the rest. **) ;
    (';', 's') ;
    ('=', 'e') ;
    ('+', 'p') ;
    ('-', 'm') ;
    ('&', 'j') ;
    (':', 'c') ;
    (' ', 'l') ;
    ('%', 'n') ;
    ('#', 'd') ;
    ('(', 'b') ;
    (')', 'k') ;
    ('\'', 'q') ;
    ('"', 'u') ;
    ('!', 'i') ;
    ('?', 'y') ;
    ('*', 'r') ;
    ('@', 'a') ;
    (',', 'v') ;
    ('/', 'h') ;
    ('\\', 'x') ;
    ('[', 'f') ;
    (']', 'g') ;
    ('~', 'w') ;
    ('.', 't') ;
  ]

let _ =
  assert (List.for_all (fun ((c1, r1), (c2, r2)) ->
            r1 <> r2 || c1 = c2) (Utils.list_square url_replacements url_replacements))

let get_parameters _ =
  let decode str =
    let replace str = Re.Str.global_replace (Re.Str.regexp_string str) in
    List.fold_left (fun str (c, r) ->
      replace ("_" ^ String.make 1 r) (String.make 1 c) str) str (List.rev url_replacements) in
  let str = Url.Current.get_fragment () in
  let str = Url.urldecode str in
  let str = decode str in
  let l = String.split_on_char '&' str in
  Utils.list_map_filter (fun str ->
    match String.split_on_char '=' str with
    | key :: value :: [] -> Some (decode key, decode value)
    | _ -> None) l

let set_parameters l =
  let encode str =
    let replace str = Re.Str.global_replace (Re.Str.regexp_string str) in
    List.fold_left (fun str (c, r) ->
      replace (String.make 1 c) ("_" ^ String.make 1 r) str) str url_replacements in
  let l =
    List.map (fun (key, value) ->
      encode key ^ "=" ^ encode value) l in
  Url.Current.set_fragment (Url.urlencode (encode (String.concat "&" l)))

let languages =
  let navigator = Dom_html.window##.navigator in
  let to_list o =
    match Js.Optdef.to_option o with
    | None -> []
    | Some a -> [Js.to_string a] in
  to_list navigator##.language @ to_list navigator##.userLanguage

type node = Dom_html.element Js.t

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

let document = Dom_html.window##.document

(** Return a list of CSS classes corresponding to the style of a button. **)
let link_to_class = function
  | InOut.Simple -> []
  | InOut.Button b ->
    [if b then "mainButton" else "secondaryButton"]

(** Add the CSS classes to a node. **)
let add_class_link a s =
  List.iter (fun c -> a##.classList##add (Js.string c)) (link_to_class s)

let rec block_node =
  let appendChilds f e =
    List.iter (fun b -> Dom.appendChild e (f (block_node b))) in
  function
  | InOut.Div (layout, l) ->
    let div = Dom_html.createDiv document in
    let _ =
      match layout with
      | InOut.Normal -> ()
      | InOut.Centered ->
        div##.className := Js.string "center"
      | Inlined ->
        div##.className := Js.string "inlined" in
    appendChilds Utils.id div l ;
    (div :> Dom_html.element Js.t)
  | InOut.P l ->
    let p = Dom_html.createP document in
    appendChilds Utils.id p l ;
    (p :> Dom_html.element Js.t)
  | InOut.List (visible, l) ->
    let ul = Dom_html.createUl document in
    ul##.className := Js.string (if visible then "bullet" else "bulletless") ;
    appendChilds (fun n ->
      let li = Dom_html.createLi document in
      Dom.appendChild li n ;
      li) ul l ;
    (ul :> Dom_html.element Js.t)
  | InOut.Space ->
    let span = Dom_html.createSpan document in
    span##.classList##add (Js.string "space") ;
    (span :> Dom_html.element Js.t)
  | InOut.Text text ->
    let span = Dom_html.createSpan document in
    Dom.appendChild span (Dom_html.document##createTextNode (Js.string text)) ;
    (span :> Dom_html.element Js.t)
  | InOut.FoldableBlock (visible, title, node) ->
    let div = Dom_html.createDiv document in
    div##.classList##add (Js.string "foldable") ;
    let text = Dom_html.document##createTextNode (Js.string title) in
    let title = Dom_html.createH3 document in
    Dom.appendChild title text ;
    Dom.appendChild div title ;
    let inner = Dom_html.createDiv document in
    let visible = ref visible in
    let set _ =
      title##.classList##remove
        (Js.string (if !visible then "folded" else "unfolded")) ;
      title##.classList##add
        (Js.string (if !visible then "unfolded" else "folded")) in
    set () ;
    Lwt.async (fun _ ->
      Lwt_js_events.clicks title (fun _ _ ->
        visible := not !visible ;
        set () ;
        Lwt.return ())) ;
    Dom.appendChild div inner ;
    Dom.appendChild inner (block_node node) ;
    (div :> Dom_html.element Js.t)
  | InOut.LinkExtern (style, text, link) ->
    let a = Dom_html.createA document in
    add_class_link a style ;
    let text = Dom_html.document##createTextNode (Js.string text) in
    Dom.appendChild a text ;
    a##.href := Js.string link ;
    (a :> Dom_html.element Js.t)
  | InOut.LinkContinuation (forwards, style, text, cont) ->
    let a = block_node (InOut.LinkExtern (style, text, "javascript:void(42)")) in
    if not forwards then
      a##.classList##add (Js.string "previous") ;
    Lwt.async (fun _ ->
      Lwt_js_events.clicks a (fun _ _ -> Lwt.return (cont ()))) ;
    a
  | InOut.LinkFile (style, text, fileName, mime, native, cont) ->
    let endings = if native then `Native else `Transparent in
    let blob = File.blob_from_string ~contentType:mime ~endings:endings (cont ()) in
    let url = Dom_html.window##._URL##createObjectURL blob in
    let a = block_node (InOut.LinkExtern (style, text, Js.to_string url)) in
    add_class_link a style ;
    ignore (a##setAttribute (Js.string "download") (Js.string fileName)) ;
    a
  | InOut.Table (classes, headers, content) ->
    let apply_classes n =
      List.iter (fun str -> n##.classList##add (Js.string str)) in
    let apply_options o c =
      assert (o.InOut.row >= 1) ;
      if o.InOut.row > 1 then
        c##.rowSpan := o.InOut.row ;
      assert (o.InOut.col >= 1) ;
      if o.InOut.col > 1 then
        c##.colSpan := o.InOut.col ;
      apply_classes c o.InOut.classes ;
      c in
    let appendChilds_options f e =
      List.iter (fun (b, o) ->
        let n = apply_options o (f (block_node b)) in
        Dom.appendChild e n) in
    let table = Dom_html.createTable document in
    apply_classes table classes ;
    let thead = Dom_html.createThead document in
    Dom.appendChild table thead ;
    let tbody = Dom_html.createTbody document in
    Dom.appendChild table tbody ;
    let header = Dom_html.createTr document in
    Dom.appendChild thead header ;
    appendChilds_options (fun n ->
      let th = Dom_html.createTh document in
      Dom.appendChild th n ;
      th) header headers ;
    List.iter (fun (classes, l) ->
      let line = Dom_html.createTr document in
      Dom.appendChild tbody line ;
      appendChilds_options (fun n ->
        let td = Dom_html.createTd document in
        Dom.appendChild td n ;
        apply_classes td classes ;
        td) line l) content ;
    (table :> Dom_html.element Js.t)
  | InOut.Node n -> n

(** Return the [response] div from the main webpage. **)
let get_response _ =
  Js.Opt.get (Dom_html.document##getElementById (Js.string "response")) (fun _ ->
    failwith "The element [response] has not been found in the webpage.")

(** Remove all the child of a node. **)
let rec clear_node n =
  match Js.Opt.to_option n##.firstChild with
  | Some c ->
    ignore (n##removeChild c) ;
    clear_node n
  | None -> ()

let clear_response _ =
  clear_node (get_response ())

let print_node ?(error = false) n =
  let response = get_response () in
  let div = Dom_html.createDiv document in
  div##.className := Js.string (if error then "error" else "block") ;
  Dom.appendChild div n ;
  Dom.appendChild response div

let print_block ?(error = false) =
  Utils.compose (print_node ~error) (Utils.compose block_node InOut.add_spaces)


let createTextOutput txt =
  let span = Dom_html.createSpan document in
  let add txt =
    Dom.appendChild span (Dom_html.document##createTextNode (Js.string txt)) in
  add txt ;
  ((span :> Dom_html.element Js.t), fun txt -> clear_node span ; add txt)

let createNumberOutput n =
  let (node, set) = createTextOutput (string_of_int n) in
  (node, fun n -> set (string_of_int n))

(** Given a DOM node, an input [input] an internal [get] function, the actual [get] function,
 * a [set] function, and a [lock] and [unlock] functions, create an interaction.
 * The triggerring of the [onChange] functions are dealt automatically. **)
let createInteraction node input get actual_get set lock unlock =
  let l = ref [] in
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
  let onChange f =
    if !l = [] then (
      (** To avoid placing too many event listeners, we only add it once we know that there is
       * at least one function. **)
      input##.onchange :=
        Dom_html.handler (fun _ ->
          trigger () ;
          Js._false)
    ) ;
    l := f :: !l in
  let trigger set x =
    set x ;
    trigger () in
  let triggerLock status f _ =
    if !locked <> status then (
      locked := status ;
      f () ;
      List.iter (fun f -> f status) !onLock
    ) in {
    node = (node :> Dom_html.element Js.t) ;
    get = actual_get ;
    set = trigger set ;
    onChange = onChange ;
    lock = triggerLock true lock ;
    unlock = triggerLock false unlock ;
    locked = (fun _ -> !locked) ;
    onLockChange = onLockChange
  }

(** Variant for the case where [node] has been created using [Dom_html.createInput],
 * and for which we can define a default [lock] and [unlock] function. **)
let createInputInteraction (node : Dom_html.inputElement Js.t) get actual_get set =
  let setLock status = node##.disabled := Js.bool status in
  let lock _ = setLock true in
  let unlock _ = setLock false in
  createInteraction node node get actual_get set lock unlock

let createNumberInput ?min:(mi = 0) ?max:(ma = max_int) d =
  let input = Dom_html.createInput ~_type:(Js.string "number") document in
  ignore (input##setAttribute (Js.string "min") (Js.string (string_of_int mi))) ;
  ignore (input##setAttribute (Js.string "max") (Js.string (string_of_int ma))) ;
  let set d =
    let d = min ma (max mi d) in
    input##.value := Js.string (string_of_int d) in
  set d ;
  let get _ = min ma (max mi (int_of_string (Js.to_string input##.value))) in
  createInputInteraction input get get set

let createTextInput txt =
  let input = Dom_html.createInput ~_type:(Js.string "text") document in
  input##.value := Js.string txt ;
  let get _ = Js.to_string input##.value in
  let set str = input##.value := Js.string str in
  createInputInteraction input get get set

let createListInput l =
  let input = Dom_html.createSelect document in
  let (get, set) =
    if l = [] then (
      input##.disabled := Js.bool true ;
      ((fun _ -> None), (function
        | None -> ()
        | Some _ -> invalid_arg "createListInput: set on an empty list."))
    ) else (
      List.iteri (fun i (txt, _) ->
        let i = "option_" ^ string_of_int i in
        let o = Dom_html.createOption document in
        o##.value := Js.string i ;
        Dom.appendChild o (Dom_html.document##createTextNode (Js.string txt)) ;
        Dom.appendChild input o) l ;
      ((fun _ ->
         let i = input##.selectedIndex in
         List.nth_opt l i),
       (function
        | None -> input##.selectedIndex := -1
        | Some k ->
          match Utils.list_associ_opt k l with
          | None -> invalid_arg "createListInput: set on an non-existing element."
          | Some (i, _) -> input##.selectedIndex := i))
    ) in
  let get_stro _ =
    let i = input##.selectedIndex in
    if i < 0 then None
    else Option.map fst (List.nth_opt l i) in
  let lock _ = input##.disabled := Js.bool true in
  let unlock _ = if l <> [] then input##.disabled := Js.bool false in
  let i = createInteraction input input get_stro get set lock unlock in
  { i with
      set = (fun a -> i.set (Some a)) ;
      onChange =
        (fun f ->
          i.onChange (fun a ->
            let a = Utils.assert_option __LOC__ a in
            f a)) }

let synchroniseListInput i1 i2 =
  (match i1.get () with
   | None -> ()
   | Some (k, _) -> i2.set k) ;
  i1.onChange i2.set ;
  i2.onChange i1.set ;
  i1.onLockChange (lockMatch i2) ;
  i2.onLockChange (lockMatch i1)

let createResponsiveListInput default placeholder get =
  let main = Dom_html.createDiv document in
  main##.classList##add (Js.string "autocomplete") ;
  let l = ref default in
  let remove txt =
    l := List.filter (fun (txt', _) -> txt <> txt') !l in
  let ul = Dom_html.createUl document in
  Dom.appendChild main ul ;
  let rec update_list _ =
    clear_node ul ;
    List.iter (fun (str, _) ->
      let li = Dom_html.createLi document in
      Dom.appendChild ul li ;
      Dom.appendChild li (block_node (InOut.Text str)) ;
      let close = Dom_html.createButton document in
      close##.onclick :=
        Dom_html.handler (fun _ ->
          remove str ;
          update_list () ;
          Js._true) ;
      close##.classList##add (Js.string "autocomplete-close") ;
      Dom.appendChild li close) !l in
  update_list () ;
  let form = Dom_html.createForm document in
  ignore (form##setAttribute (Js.string "autocomplete") (Js.string "off")) ;
  Dom.appendChild main form ;
  let input = Dom_html.createInput ~_type:(Js.string "text") document in
  input##.placeholder := Js.string placeholder ;
  Dom.appendChild form input ;
  let div = Dom_html.createDiv document in
  div##.classList##add (Js.string "autocomplete-items") ;
  Dom.appendChild form div ;
  let close_all_list _ =
    let l = document##getElementsByClassName (Js.string "autocomplete-items") in
    let rec aux n =
      if n = l##.length then ()
      else (
        match Js.Opt.to_option (l##item n) with
        | None -> assert false
        | Some e ->
          clear_node e ;
          aux (n + 1)
      ) in
    aux 0 in
  let current_focus = ref None in
  let add str v =
    close_all_list () ;
    remove str ;
    l := (str, v) :: !l ;
    update_list () in
  let create_autocompletions _ =
    close_all_list () ;
    let autocompletions =
      List.map (fun (str, v) ->
        let item = Dom_html.createDiv document in
        let apply _ =
          input##.value := Js.string "" ;
          add str v in
        item##.onmousedown :=
          Dom_html.handler (fun _ ->
            apply () ;
            Js._true) ;
        Dom.appendChild item (block_node (InOut.Text str)) ;
        (item, apply)) (get (Js.to_string input##.value)) in
    List.iter (Dom.appendChild div) (List.rev_map fst autocompletions) ;
    autocompletions in
  input##.oninput :=
    Dom_html.handler (fun _ ->
      current_focus := None ;
      ignore (create_autocompletions ()) ;
      Js._true) ;
  input##.onfocus :=
    Dom_html.handler (fun _ ->
      ignore (create_autocompletions ()) ;
      Js._true) ;
  input##.onblur :=
    Dom_html.handler (fun _ ->
      Lwt.async (fun _ ->
        pause () ;%lwt
        Lwt.return (close_all_list ())) ;
      Js._true) ;
  let update_focus l =
    Option.may (fun i ->
      (fst (Utils.assert_option __LOC__ (List.nth_opt l i)))##.classList##add
        (Js.string "autocomplete-active")) !current_focus in
  input##.onkeydown :=
    Dom_html.handler (fun e ->
      match Dom_html.Keyboard_code.of_event e with
      | Dom_html.Keyboard_code.ArrowUp ->
        let autocompletions = create_autocompletions () in
        let length = List.length autocompletions in
        current_focus :=
          (match !current_focus with
           | None ->
             if length > 0 then Some 0 else None
           | Some j ->
             let j = j + 1 in
             if j >= length then
               if length > 0 then Some 0 else None
             else Some j) ;
        update_focus autocompletions ;
        Js._true
      | Dom_html.Keyboard_code.ArrowDown ->
        let autocompletions = create_autocompletions () in
        let length = List.length autocompletions in
        current_focus :=
          (match !current_focus with
           | None | Some 0 ->
             let i = length - 1 in
             if i >= 0 then Some i else None
           | Some j ->
             if j >= length then
               let i = length - 1 in
               if i >= 0 then Some i else None
             else Some (j - 1)) ;
        update_focus autocompletions ;
        Js._true
      | Dom_html.Keyboard_code.Enter ->
        let autocompletions = create_autocompletions () in
        Option.may (fun i ->
          snd (Utils.assert_option __LOC__ (List.nth_opt autocompletions i)) ()) !current_focus ;
        Js._false
      | _ -> Js._true) ;
  let get _ = !l in
  let set l' =
    l := l' ;
    update_list () in
  let lock _ =
    input##.disabled := Js.bool true ;
    ul##.classList##add (Js.string "autocomplete-disabled") in
  let unlock _ =
    input##.disabled := Js.bool false ;
    ul##.classList##remove (Js.string "autocomplete-disabled") in
  createInteraction main input get get set lock unlock

let createPercentageInput d =
  let maxv = 1_000_000 in
  let maxvf = float_of_int maxv in
  let input = Dom_html.createInput ~_type:(Js.string "range") document in
  ignore (input##setAttribute (Js.string "min") (Js.string "0")) ;
  ignore (input##setAttribute (Js.string "max") (Js.string (string_of_int maxv))) ;
  let set d =
    let d = max 0. (min 1. d) in
    input##.value := Js.string (string_of_int (int_of_float (maxvf *. d))) in
  set d ;
  let get _ = (max 0. (min maxvf (float_of_string (Js.to_string input##.value)))) /. maxvf in
  createInputInteraction input get get set

let createDateInput d =
  let input = Dom_html.createInput ~_type:(Js.string "date") document in
  let set d =
    input##.value := Js.string (Date.iso8601 d) in
  set d ;
  let get _ = Date.from_iso8601 (Js.to_string input##.value) in
  createInputInteraction input get get set

let createSwitch text descr texton textoff b =
  let label = Dom_html.createLabel document in
  label##.classList##add (Js.string "switch") ;
  let input = Dom_html.createInput ~_type:(Js.string "checkbox") document in
  Dom.appendChild label input ;
  let span = Dom_html.createSpan document in
  span##.classList##add (Js.string "slider") ;
  Dom.appendChild label span ;
  let text = block_node (InOut.Text text) in
  text##.classList##add (Js.string "switch_text") ;
  Dom.appendChild label text ;
  Option.may (fun text ->
    Dom.appendChild label (block_node (InOut.Text " ")) ;
    Dom.appendChild label (block_node (InOut.Text text))) descr ;
  let addText textClass =
    Option.may (fun text ->
      Dom.appendChild label (block_node (InOut.Text " ")) ;
      let node = block_node (InOut.Text text) in
      node##.classList##add (Js.string textClass) ;
      Dom.appendChild label node) in
  addText "textswitchon" texton ;
  addText "textswitchoff" textoff ;
  let set b = (Js.Unsafe.coerce input)##.checked := Js.bool b in
  set b ;
  let get _ = Js.to_bool (Js.Unsafe.coerce input)##.checked in
  let lock _ = input##.disabled := Js.bool true in
  let unlock _ = input##.disabled := Js.bool false in
  createInteraction label input get get set lock unlock

let createFileImport extensions prepare =
  let input = Dom_html.createInput ~_type:(Js.string "file") document in
  if extensions <> [] then
    input##.accept := Js.string (String.concat ", " (List.map (fun e -> "." ^ e) extensions)) ;
  ((input :> Dom_html.element Js.t), fun _ ->
    prepare () ;%lwt
    match Js.Optdef.to_option input##.files with
    | None -> Lwt.return ("", "")
    | Some files ->
      let rec aux l n =
        if n = files##.length then
          Lwt.return (List.rev l)
        else
          match Js.Opt.to_option (files##item n) with
          | None -> aux l (n + 1)
          | Some f ->
            let reader = new%js Js_of_ocaml.File.fileReader in
            let (cont, w) = Lwt.task () in
            reader##.onload := Dom.handler (fun _ ->
              let str =
                Js.to_string (Utils.assert_option __LOC__
                  (Js.Opt.to_option (File.CoerceTo.string (reader##.result)))) in
              Lwt.wakeup_later w (fun _ ->
                aux ((Js.to_string f##.name, str) :: l) (n + 1)) ;
              Js._true) ;
            reader##readAsText f ;
            let%lwt cont = cont in cont () in
      let%lwt l = aux [] 0 in
      Lwt.return (String.concat "," (List.map fst l),
                  String.concat "" (List.map snd l)))

