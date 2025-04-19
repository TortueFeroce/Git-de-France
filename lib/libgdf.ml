(* open Arg
open Unix
open Filename
open Sys
open Sha1 *)

exception GdfError of string

type path = string list

type worktree = path

type git_directory = path

type obj =
  | Blob of string * string

let perm_base = 0o777

let check_init s =
  (* Fonction qui regarde si les conditions sont remplies pour
  initialiser un projet git *)
  print_string s


let compute_init s =
  (* Fonction qui est appelée lorsque la commande init est saisie *)
  (try
    Unix.mkdir s perm_base;
    print_string "je suis là";
  with Unix.Unix_error(_) -> () );
  Unix.mkdir (s^"/.git") perm_base;
  Unix.mkdir (s^"/.git/objects") perm_base;
  Unix.mkdir (s^"/.git/refs") perm_base;
  let config_channel = open_out (s^"/.git/config") in
    output_string config_channel "[core]\n\trepositoryformatversion = 0\n\tfilemode = false\n\tbare = false";
    close_out config_channel


let repo_find () = (
  (* Fonction qui ... *)
  let rec aux () = (
    let cur_name = Unix.getcwd () in
    try
      let _ = Unix.stat ".git" in cur_name
    with _ -> (
      let () = Unix.chdir "../" in
      let new_name = Unix.getcwd () in
      if new_name = cur_name then
        raise (GdfError "Pas un repo git");
      aux ()))
  in aux ())

let hash_file _ _ _ =
  failwith "pas encore implémenté"
  
let add_char_to_str c s = (s^(Char.escaped c))
  
let chan_to_string chan size =
  let data = ref "" in
  for _ = 0 to size - 1 do
    add_char_to_str (Gzip.input_char chan) (!data)
  done;
  !data
  
  let add_char_until n f_channel = 
    let is_n = ref true in
    let acc = ref "" in
    while !is_n do
      let c = Gzip.input_char f_channel in
      if c = (Char.chr n) then is_n := false
      else acc := add_char_to_str c !acc
    done; !acc

let serialize chan typ size =
  let data = chan_to_string chan size in
  match typ with
    | "blob" -> let file_name = String.split_on_cah

let read_object sha = (
  let len_sha = String.length sha in
  let obj_name = String.sub sha 2 (len_sha - 2) in
  let obj_path = (repo_find ())^"/.git/objects" in
  let () = Unix.chdir obj_path in
  let bit0 = sha.[0] in
  let bit1 = sha.[1] in
  let dir_sha = (Char.escaped bit0)^(Char.escaped bit1) in
  (try
    Unix.chdir dir_sha;
  with _ -> raise (GdfError "le haché ne correspond à aucun fichier"));
  let file_channel = Gzip.open_in obj_name in
  let header_file = add_char_until 0x20 file_channel in
  let _ = add_char_until 0x00 file_channel in
  (* TO DO : checker si la taille correspond *)
  match header_file with
    | "blob" -> PreBlob(file_channel,)
    | _ -> failwith "les autres types de header ne sont pas implémentés"
  )

let write_object obj =


let f_test () =
  (* fonction de test *)
  failwith "rien dans la fonction de test"
