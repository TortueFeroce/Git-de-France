
(* ocamlfind ocamlopt -linkpkg -package cmdliner -o revolt revolt.ml *)

open Arg
open Libgdf

let force = ref false
let doWrite = ref false
let objType = ref "blob"
let not_light = ref false
let type_rev = ref ""

let options = ref []

let findOptions str = match str with
  | "init" -> [("-f", Set force, "placeholder")]
  | "hash-object" -> [("-w", Set doWrite, "placeholder"); ("-t", Set_string objType, "placeholder")]
  | "cat-file" -> []
  | "log" -> []
  | "checkout" -> []
  | "show-refs" -> []
  | "tag" -> [("-a", Set not_light, "placeholder")]
  | "rev-parse" -> [("-t", Set_string type_rev, "placeholder")] (* TO DO : j'ai pas compris son truc de wyag-type *)
  | "ls-files" -> []
  | "check-ignore" -> []
  | "status" -> []
  | "test" -> []
  | _ -> failwith "loserrrr"

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
  
let () = (Printexc.record_backtrace true;
  let found_args, c_name = read_option () in
  match c_name, found_args with
    | "init", x::[] -> compute_init x
    | "hash-object", x::[] -> let sha = hash_file !doWrite !objType x
                              in print_string sha
    | "test", [] -> cmd_status_branch ()
    | "cat-file", sha :: typ :: [] -> cat_file typ sha
    | "log", x :: [] -> compute_log x
    | "checkout", dir :: sha :: [] -> compute_checkout sha dir
    | "show-refs", [] -> print_refs ()
    | "tag", obj :: name :: [] -> compute_tag name obj
    | "tag", name :: [] -> compute_tag name "HEAD"
    | "tag", [] -> print_tag ()
    | "rev-parse", name :: [] -> compute_rev_parse name !type_rev
    | "ls-files", [] -> print_index_files ()
    | "check-ignore", l -> compute_check_ignore l
    | "status", [] -> ()
    | _ -> failwith "commande pas implémentée")

(* Test pour le parser des commits :
let () = (let commit = commit_parser "24commit_test" in
          print_newline ();
          Printf.printf "tree : %s" (commit.tree);
          print_newline ();
          print_string "parent : "; List.iter print_string (commit.parent);
          print_newline ();
          Printf.printf "gpgsig : %s" (commit.gpgsig);
          print_newline ();
          Printf.printf "name : %s" (commit.name)) *)



(* 
let empty_t = Term.(const print_test $ const ())
let cmd = Cmd.v (Cmd.info "mesgrossescouilles") empty_t

let () = exit (Cmd.eval cmd) *)