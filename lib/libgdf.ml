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
  Unix.mkdir (s^"/.gdf") perm_base;
  Unix.mkdir (s^"/.gdf/objects") perm_base;
  Unix.mkdir (s^"/.gdf/refs") perm_base;
  let config_channel = open_out (s^"/.gdf/config") in
    output_string config_channel "[core]\n\trepositoryformatversion = 0\n\tfilemode = false\n\tbare = false";
    close_out config_channel


let repo_find () = (
  (* Fonction qui ... *)
  let rec aux () = (
    let cur_name = Unix.getcwd () in
    try
      let _ = Unix.stat ".gdf" in cur_name
    with _ -> (
      let () = Unix.chdir "../" in
      let new_name = Unix.getcwd () in
      if new_name = cur_name then
        raise (GdfError "Pas un repo gdf");
      aux ()))
  in aux ())

let hash_file _ _ _ =
  failwith "pas encore implémenté"
  
let add_char_to_str c s = (s^(String.make 1 c))
  
let chan_to_string chan size =
  let data = ref "" in
  for _ = 0 to size - 1 do
    data := add_char_to_str (Gzip.input_char chan) (!data)
  done;
  !data

let read_str_until_eof chan =
  let has_ended = ref true and data = ref "" in
    while !has_ended do
      try 
        data := add_char_to_str (Gzip.input_char chan) (!data)
      with _ -> has_ended := false
    done;
    !data

let write_str chan str = (*on pourrait utiliser output_substring mais parait il c'est pas bien*)
  for i = 0 to (String.length str) - 1 do
    Gzip.output_char chan str.[i]
  done
  
let add_char_until n f_channel = 
  let is_n = ref true in
  let acc = ref "" in
  while !is_n do
    let c = Gzip.input_char f_channel in
    if c = (Char.chr n) then is_n := false
    else acc := add_char_to_str c !acc
  done; !acc

let deserialize str =
  let data = String.split_on_char ('\n') str in
  match data with
    | "blob" :: size :: file_name :: q ->
                let file_data = String.concat "" q in
                assert ((String.length file_data) = (int_of_string size));
                Blob(file_name, file_data)
    | _ -> failwith "non implémenté"

let serialize obj =
  match obj with
    | Blob(file_name, file_data) ->
        let size = string_of_int (String.length file_data) in
        String.concat "\n" ["blob"; size; file_name; file_data]

let read_object sha =
  let len_sha = String.length sha in
  let dir_sha = String.sub sha 0 2
  and obj_name = String.sub sha 2 (len_sha - 2) in
  let obj_path = (repo_find ())^"/.gdf/objects/" in
  let file_channel = Gzip.open_in (obj_path^dir_sha^"/"^obj_name) in
  let pre_obj = read_str_until_eof file_channel in
  Gzip.close_in file_channel;
  deserialize pre_obj

let write_object obj do_write =
  let serialized_obj = serialize obj in
  let sha = Sha1.to_hex (Sha1.string serialized_obj) in
  (if do_write then
    let first_sha = String.sub sha 0 2 
    and last_sha = String.sub sha 2 ((String.length sha) - 2) in
      let repo_path = repo_find () in
        (if not (Sys.file_exists (repo_path^"/.gdf/objects/"^first_sha)) 
          then Unix.mkdir (repo_path^"/.gdf/objects/"^first_sha) perm_base); (*crée le repertoire ou on range l'objet s'il existe pas encore*)
        let file_channel = Gzip.open_out (repo_path^"/.gdf/objects/"^first_sha^"/"^last_sha) in
          write_str file_channel serialized_obj;
          Gzip.close_out file_channel);
  sha



let f_test () =
  (* fonction de test *)
  let sha = write_object (Blob("test", "test test test")) true in
  match read_object sha with
    | Blob(a,b) -> print_string a; print_newline (); print_string b;