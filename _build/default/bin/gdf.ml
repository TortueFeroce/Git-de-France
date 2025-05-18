
(* ocamlfind ocamlopt -linkpkg -package cmdliner -o revolt revolt.ml *)

open Arg
open Libgdf

let doWrite = ref false
let objType = ref "blob"
let type_rev = ref ""
let lesCommandes =
  "> initialiser\tinitialise un entrepôt git-de-France (.gdf) à l'endroit spécifié, par défaut, initialise ici

> hacher-objet [-w] [-t]\tdonne le haché de l'objet

> concatener-fichier\taffiche un objet

> enregistrer\trenvoie l'arbre des parents d'un commettre en format dot

> verifier\tsi un seul argument est passé, place la tête sur la branche donnée, si deux arguments sont passés, rétabli le commettre dans le dossier indiqué (qui doit être vide)

> montrer-references\taffiche toutes les références

> etiqueter\tcrée une référence

> analyser-revision\taffiche le haché d'une référence

> verifier-ignorer\tprends une liste de fichier et donne lesquels sont ignorés par les .gdfignore et exlus

> statut\taffiche le statut des changements

> supprimer\tsupprime un fichier de l'indice et du baliveau de travail

> ajouter\tajoute un fichier à l'indice

> commettre\tcommet

> configurer-utilisateur\tajoute l'utilisateur au fichier config

> branche\tcrée une branche ou affiche toutes les branches si rien n'est passé en argument\n"
let options = ref [("-help", Unit (fun () -> Printf.printf "%s" lesCommandes), "Montre les commandes disponibles")]

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
  | "configurer-utilisateur" -> []
  | "branche" -> []
  | _ -> raise (GdfError "Cette commande n'existe pas")

let isSubcomm = ref true

let read_option () =
  (* Printf.printf "usage message : %s\n" usage_msg; *)
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
    | "configurer-utilisateur", email :: name :: [] -> set_user name email
    | "", _ -> ()
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