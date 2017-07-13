
type t = unit (* TODO *)

let print s =
  print_string s ;
  print_newline ()

let update _ _ = () (* TODO *)

let prints l =
  let r = List.map print_string l in
  print_newline () ;
  r

let message s =
  print s

let should_not_happen s =
  message ("An assertion have failed: " ^ s ^ ". Please report.")

