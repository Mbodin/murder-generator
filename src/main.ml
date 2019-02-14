(** Module main.
 * This file is the one compiled to JavaScript, then fetched and executed
 * to run the whole program. **)

open Js_of_ocaml
open Lwt

let get_file url =
  let request = XmlHttpRequest.create () in
  request##_open (Js.string "GET") (Js.string url) Js._true ;
  (* TODO: Add a continuation. *)
  request##send Js.null

let _ =
  failwith "This is a test.  Please do not report it."

