(** Module main.
 * This file is the one compiled to JavaScript, then fetched and executed
 * to run the whole program. **)

type translations = {
    iso639 : string ;
    name : string ;
    there : string ;
    underConstruction : string ;
    participate : string ;
    description : string ;
    openSource : string ;
    error : string ;
    report : string ;
    howManyPlayers : string ;
    experience : string ;
    beginner : string ;
    experienced : string
  } [@@deriving json] (* FIXME: This deriving is not that great:
                         it imposes OCaml’s marshalling’s scheme
                         which is not human readable. *)

(** The translations needed to print error messages. **)
let errorTranslations =
  ref ("An error occurred!", "Please report it", "there", "Error details:")

(** Whether the loading animation is running. **)
let loading = ref true

(** Stops the loading animation. **)
let stopLoading _ =
  assert%lwt !loading ;%lwt
  ignore (Js.Unsafe.fun_call (Js.Unsafe.js_expr "stopLoading") [||]) ;
  loading := false ;
  Lwt.return ()

(** Starts the loading animation. **)
let startLoading _ =
  assert%lwt !loading ;%lwt
  ignore (Js.Unsafe.fun_call (Js.Unsafe.js_expr "startLoading") [||]) ;
  loading := true ;
  Lwt.return ()

(** The main script. **)
let _ =
  try%lwt
    InOut.clear_response () ;
    InOut.print_block (InOut.Text "Just a test.") ;
    Dom_html.window##alert (Js.string "Test: A") ;
    let%lwt txt = InOut.get_file "web/translations.json" in
    Dom_html.window##alert (Js.string "Test: B") ;
    let all =
      try
        Deriving_Json.from_string [%derive.json: translations array] txt
      with e ->
        Dom_html.window##alert (Js.string "Test: F") ;
        Dom_html.window##alert (Js.string ("Test: G; " ^ Printexc.to_string e)) ;
        raise e in
    Dom_html.window##alert (Js.string "Test: C") ;
    let all = Array.to_list all in
    let all = Utils.shuffle all in
    List.iter (fun t ->
      InOut.print_block (InOut.P [InOut.LinkContinuation (t.name, fun _ ->
        InOut.print_block (InOut.P [InOut.Text t.underConstruction]))])) all ;
    stopLoading ()
  with e ->
    Dom_html.window##alert (Js.string "Test: D") ;
    Dom_html.window##alert (Js.string ("Test: E; " ^ Printexc.to_string e)) ;
    let (errorOccurred, reportIt, there, details) = !errorTranslations in
    InOut.print_block (InOut.Div [
        InOut.P [
            InOut.Text errorOccurred ; InOut.Text reportIt ;
            InOut.Link (there, "https://github.com/Mbodin/murder-generator/issues")
          ] ;
        InOut.P [
            InOut.Text details ;
            InOut.Text (Printexc.to_string e)
          ]
      ]) ;
    if%lwt Lwt.return !loading then stopLoading () ;%lwt
    Lwt.return ()

