(* ocamlfind ocamlopt -linkpkg -package cmdliner -o revolt revolt.ml *)

open Arg

let print_test () = 
  Printf.printf "coucou les copaings"

let options = [("-mesgrossescouilles", Unit print_test, "ne fait rien pour l'instant")]

let read_option () =
  let args = ref [] in
  Arg.parse options
  (fun s -> args := s :: !args)
  "coucou la team ça gase ?!!"

let () = read_option ()


(* 
let empty_t = Term.(const print_test $ const ())
let cmd = Cmd.v (Cmd.info "mesgrossescouilles") empty_t

let () = exit (Cmd.eval cmd) *)