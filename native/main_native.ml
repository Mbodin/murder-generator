(** Module Main_native.
 * Instantiates Module Main for the native outputs. **)

module Main = Main.Main (InOut_native)

let _ =
  Lwt_main.run Main.main

