
let get_file url =
  let (res, w) = Lwt.task () in
  let request = XmlHttpRequest.create () in
  request##_open (Js.string "GET") (Js.string url) Js._true ;
  request##.onreadystatechange :=
    Js.wrap_callback (fun _ ->
      if request##.readyState = XmlHttpRequest.DONE then (
        if request##.status = 200 then
          Lwt.wakeup w (Js.to_string request##.responseText)
        else
          Lwt.wakeup_exn w (Invalid_argument
               ("Error when fetching " ^ url ^ ". "
                ^ string_of_int request##.status ^ " :"
                ^ Js.to_string request##.statusText)))) ;
  request##send Js.null ;
  res


type block =
  | Div of block list
  | P of block list
  | Text of string
  | Link of string * string
  | LinkContinuation of string * (unit -> unit)
  | Node of Dom_html.element Js.t

let rec add_spaces =
  let need_space = function
    | Div _ -> false
    | P _ -> false
    | Text _ -> true
    | Link _ -> true
    | LinkContinuation _ -> true
    | Node _ -> false in
  let rec aux = function
    | [] -> []
    | a :: [] -> a :: []
    | a :: b :: l ->
      if need_space a && need_space b then
        a :: Text " " :: aux (b :: l)
      else a :: aux (b :: l) in
  function
    | Div l ->
      let l = List.map add_spaces l in
      Div (aux l)
    | P l ->
      let l = List.map add_spaces l in
      P (aux l)
    | e -> e

let document = Dom_html.window##.document

let rec block_node = function
  | Div l ->
    let div = Dom_html.createDiv document in
    List.iter (fun b ->
      ignore (Dom.appendChild div (block_node b))) l ;
    (div :> Dom_html.element Js.t)
  | P l ->
    let p = Dom_html.createP document in
    List.iter (fun b ->
      ignore (Dom.appendChild p (block_node b))) l ;
    (p :> Dom_html.element Js.t)
  | Text text ->
    let span = Dom_html.createSpan document in
    Dom.appendChild span (Dom_html.document##createTextNode (Js.string text)) ;
    (span :> Dom_html.element Js.t)
  | Link (text, link) ->
    let a = Dom_html.createA document in
    let text = Dom_html.document##createTextNode (Js.string text) in
    ignore (Dom.appendChild a text) ;
    ignore (a##setAttribute (Js.string "href") (Js.string link)) ;
    (a :> Dom_html.element Js.t)
  | LinkContinuation (text, cont) ->
    let a = Dom_html.createA document in
    let text = Dom_html.document##createTextNode (Js.string text) in
    ignore (Dom.appendChild a text) ;
    ignore (a##setAttribute (Js.string "href") (Js.string "void(42)")) ;
    Lwt.async (fun _ ->
      Lwt_js_events.clicks a (fun _ _ -> Lwt.return (cont ()))) ;
    (a :> Dom_html.element Js.t)
  | Node n -> n

(** Returns the [response] div from the main webpage. **)
let get_response _ =
  Js.Opt.get (Dom_html.document##getElementById (Js.string "responseDiv")) (fun _ ->
    failwith "The element [responseDiv] has not been found in the webpage.")

let clear_response _ =
  let response = get_response () in
  let rec aux _ =
    match Js.Opt.to_option (response##.firstChild) with
    | Some n -> ignore (response##removeChild n) ; aux ()
    | None -> () in
  aux ()

let print_node n =
  let response = get_response () in
  let div = Dom_html.createDiv document in
  div##setAttribute (Js.string "class") (Js.string "block") ;
  ignore (Dom.appendChild div n) ;
  ignore (Dom.appendChild response div)

let print_block =
  Utils.compose print_node (Utils.compose block_node add_spaces)

