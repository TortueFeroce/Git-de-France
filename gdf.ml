(* ocamlfind ocamlopt -linkpkg -package cmdliner -o revolt revolt.ml *)

open Arg

let print_test () = 
  Printf.printf "coucou les copaings \n"

let compute_init () =
  (* Fonction qui est appelée lorsque la commande init est saisie *)
  
  Printf.printf "pas implémenté"

let options = ref [("-mesgrossescouilles", Unit print_test, "ne fait rien pour l'instant");
                   ("-init", Unit compute_init, "initialise un projet")]

let read_option () =
  let args = ref [] in
  Arg.parse_dynamic options
  (fun s -> args := s :: !args)
  "Voici les commandes disponibles pour git-de-France :"

let () = read_option ()



(* 
let empty_t = Term.(const print_test $ const ())
let cmd = Cmd.v (Cmd.info "mesgrossescouilles") empty_t

let () = exit (Cmd.eval cmd) *)