(* ocamlfind ocamlopt -linkpkg -package cmdliner -o revolt revolt.ml *)

open Arg
open Libgdf

let force = ref false
let doWrite = ref false
let objType = ref ""

let options = ref []

let findOptions str = match str with
  | "init" -> [("-f", Set force, "placeholder")]
  | "hash-object" -> [("-w", Set doWrite, "placeholder"); ("-t", Set_string objType, "placeholder")]
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

let () = 
  let found_args, c_name = read_option () in
  match c_name, found_args with
    | "init", x::[] -> compute_init x
    | "hash_object", x::[] -> hash_file !doWrite !objType x
    | _ -> failwith "pas implémenté"



(* 
let empty_t = Term.(const print_test $ const ())
let cmd = Cmd.v (Cmd.info "mesgrossescouilles") empty_t

let () = exit (Cmd.eval cmd) *)