
(* ocamlfind ocamlopt -linkpkg -package cmdliner -o revolt revolt.ml *)

open Arg
open Libgdf

let doWrite = ref false
let objType = ref "blob"
let type_rev = ref ""

let options = ref []

let findOptions str = match str with
  | "initialiser" -> []
  | "hacher-objet" -> [("-w", Set doWrite, "écrit l'objet"); ("-t", Set_string objType, "pour donner le type de l'objet")]
  | "concatener-fichier" -> []
  | "enregistrer" -> []
  | "verifier" -> []
  | "montrer-references" -> []
  | "etiqueter" -> []
  | "analyser-revision" -> [] (* TO DO : j'ai pas compris son truc de wyag-type *)
  | "enumerer-fichiers" -> []
  | "verifier-ignorer" -> []
  | "statut" -> []
  | "supprimer" -> [] 
  | "ajouter" -> []
  | "commettre" -> []
  | "configuere-utilisateur" -> []
  | "branche" -> []
  | "test" -> []
  | _ -> raise (GdfError "Cette commande n'existe pas")

let isSubcomm = ref true

let read_option () =
  let args = ref [] in 
  let command_name = ref "" in
  Arg.parse_dynamic options
  (fun s -> if !isSubcomm then (options := findOptions s; 
                                isSubcomm := false;
                                command_name := s) 
            else args := s :: !args)
  "Voici les commandes disponibles pour git-de-France :";
  (!args, !command_name)
  
let () = try (Printexc.record_backtrace true;
  let found_args, c_name = read_option () in
  match c_name, found_args with
    | "initialiser", x::[] -> compute_init x
    | "initialiser", [] -> compute_init "."
    | "hacher-objet", x::[] -> let sha = hash_file !doWrite !objType x
                              in print_string sha
    | "concatener-fichier", sha :: typ :: [] -> cat_file typ sha
    | "enregistrer", x :: [] -> compute_log x
    | "verifier", dir :: sha :: [] -> compute_checkout sha dir true
    | "verifier", nom_branche :: [] -> compute_checkout_branche nom_branche
    | "montrer-references", [] -> print_refs ()
    | "etiqueter", obj :: name :: [] -> compute_etiquette name obj
    | "etiqueter", name :: [] -> compute_etiquette name "TETE"
    | "etiqueter", [] -> print_etiquette ()
    | "analyser-revision", name :: [] -> compute_rev_parse name !type_rev
    | "enumerer-fichiers", [] -> print_indice_files ()
    | "verifier-ignorer", l -> compute_check_ignore l
    | "statut", [] -> compute_status ()
    | "supprimer", l -> compute_rm l true false
    | "ajouter", l -> compute_add l false true (* TO DO : faire mieux *)
    | "commettre", m::[] -> compute_commettre m
    | "branche", name::[] -> branche_create name
    | "branche", [] -> print_branches ()
    | "test", [] -> f_test ()
    | "configurer-utilisateur", email :: name :: [] -> set_user name email
    | _ -> failwith "Mauvais arguments")
  with (GdfError str) -> Printf.printf "Erreur : %s\n" str

(* Test pour le parser des commettres :
let () = (let commettre = commettre_parser "24commettre_test" in
          print_newline ();
          Printf.printf "baliveau : %s" (commettre.baliveau);
          print_newline ();
          print_string "parent : "; List.iter print_string (commettre.parent);
          print_newline ();
          Printf.printf "gpgsig : %s" (commettre.gpgsig);
          print_newline ();
          Printf.printf "name : %s" (commettre.name)) *)



(* 
let empty_t = Term.(const print_test $ const ())
let cmd = Cmd.v (Cmd.info "mesgrossescouilles") empty_t

let () = exit (Cmd.eval cmd) *)