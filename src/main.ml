(** Module main.
 * This file is the one compiled to JavaScript, then fetched and executed
 * to run the whole program. **)

open Js_of_ocaml

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
  } [@@deriving json] (* FIXME: This deriving is not that great… *)

let _ =
  let%lwt txt = get_file "web/translations.json" in
  let all = Deriving_Json.from_string [%derive.json: translations array] txt in
  failwith ("This is actually just a test.  Please do not report it. Test: "
             ^ string_of_int (Array.length all))

