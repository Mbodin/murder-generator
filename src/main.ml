(** Module main.
 * This file is the one compiled to JavaScript, then fetched and executed
 * to run the whole program. **)

open Js_of_ocaml
open Lwt

let get_file url f =
  let request = XmlHttpRequest.create () in
  request##_open (Js.string "GET") (Js.string url) Js._true ;
  request##.onreadystatechange :=
    Js.wrap_callback (fun _ ->
      if request##.readyState = XmlHttpRequest.DONE then (
        if request##.status = 200 then
          f (Js.to_string request##.responseText)
        else failwith ("Error when fetching " ^ url ^ ". "
               ^ string_of_int request##.status ^ " :"
               ^ Js.to_string request##.statusText))) ;
  request##send Js.null

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
  } [@@deriving json]

let _ =
  get_file "web/translations.json" (fun txt ->
    let all = Deriving_Json.from_string [%derive.json: translations list] txt in
      failwith ("This is actually just a test.  Please do not report it. Test: "
                ^ string_of_int (List.length all)))

