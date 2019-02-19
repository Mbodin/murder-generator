open Js_of_ocaml


let get_file url =
  let (res, w) = Lwt.task () in
  let request = XmlHttpRequest.create () in
  request##_open (Js.string "GET") (Js.string url) Js._true ;
  request##.onreadystatechange :=
    Js.wrap_callback (fun _ ->
      if request##.readyState = XmlHttpRequest.DONE then (
        if request##.status = 200 then
          Lwt.wakeup_later w (Js.to_string request##.responseText)
        else
          Lwt.wakeup_later_exn w (Invalid_argument
               ("Error when fetching " ^ url ^ ". "
                ^ string_of_int request##.status ^ " :"
                ^ Js.to_string request##.statusText)))) ;
  request##send Js.null ;
  res


type block =
  | Div of block list
  | P of block list
  | CenterP of block list
  | Text of string
  | Link of string * string
  | LinkContinuation of string * (unit -> unit)
  | Node of Dom_html.element Js.t

let rec add_spaces =
  let need_space = function
    | Div _ -> false
    | P _ -> false
    | CenterP _ -> false
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
    | CenterP l ->
      let l = List.map add_spaces l in
      CenterP (aux l)
    | e -> e

let document = Dom_html.window##.document

let rec block_node =
  let appendChilds e =
    List.iter (fun b -> ignore (Dom.appendChild e (block_node b))) in
  function
  | Div l ->
    let div = Dom_html.createDiv document in
    appendChilds div l ;
    (div :> Dom_html.element Js.t)
  | P l ->
    let p = Dom_html.createP document in
    appendChilds p l ;
    (p :> Dom_html.element Js.t)
  | CenterP l ->
    let p = Dom_html.createP document in
    p##.className := Js.string "center" ;
    appendChilds p l ;
    (p :> Dom_html.element Js.t)
  | Text text ->
    let span = Dom_html.createSpan document in
    Dom.appendChild span (Dom_html.document##createTextNode (Js.string text)) ;
    (span :> Dom_html.element Js.t)
  | Link (text, link) ->
    let a = Dom_html.createA document in
    let text = Dom_html.document##createTextNode (Js.string text) in
    ignore (Dom.appendChild a text) ;
    a##.href := Js.string link ;
    (a :> Dom_html.element Js.t)
  | LinkContinuation (text, cont) ->
    let a = Dom_html.createA document in
    let text = Dom_html.document##createTextNode (Js.string text) in
    ignore (Dom.appendChild a text) ;
    a##.href := Js.string "javascript:void(42)" ;
    Lwt.async (fun _ ->
      Lwt_js_events.clicks a (fun _ _ -> Lwt.return (cont ()))) ;
    (a :> Dom_html.element Js.t)
  | Node n -> n

(** Returns the [response] div from the main webpage. **)
let get_response _ =
  Js.Opt.get (Dom_html.document##getElementById (Js.string "response")) (fun _ ->
    failwith "The element [response] has not been found in the webpage.")

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
  div##.className := Js.string "block" ;
  ignore (Dom.appendChild div n) ;
  ignore (Dom.appendChild response div)

let print_block =
  Utils.compose print_node (Utils.compose block_node add_spaces)


let createNumberInput d =
  let input = Dom_html.createInput ~_type:(Js.string "number") document in
  ignore (input##setAttribute (Js.string "min") (Js.string "0")) ;
  ignore (input##setAttribute (Js.string "max")
           (Js.string (string_of_int max_int))) ;
  input##.value := Js.string (string_of_int d) ;
  ((input :> Dom_html.element Js.t), fun _ ->
    max 0 (int_of_string (Js.to_string input##.value)))

let createPercentageInput d =
  let input = Dom_html.createInput ~_type:(Js.string "range") document in
  ignore (input##setAttribute (Js.string "min") (Js.string "0")) ;
  ignore (input##setAttribute (Js.string "max") (Js.string "1000")) ;
  input##.value := Js.string (string_of_float (1000. *. d)) ;
  ((input :> Dom_html.element Js.t), fun _ ->
    (max 0. (min 1000. (float_of_string (Js.to_string input##.value)))) /. 1000.)

