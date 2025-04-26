(* open Arg
open Unix
open Filename
open Sys
open Sha1 *)

exception GdfError of string

type path = string list

type worktree = path

type git_directory = path

type user = string*string
(* Potentiellement, je pensais faire un truc genre :
{ name : string;
  mail : string }
}
  
mais pour l'instant je reste sur string*string tant que j'ai pas fini
mon parser à commit *)

type commit = {
  tree : string;
  parent : string list;
  author : user*string;
  committer : user*string;
  gpgsig : string;
  name : string
}

type obj =
  | Blob of string * string

let perm_base = 0o777

let check_init s =
  (* Fonction qui regarde si les conditions sont remplies pour
  initialiser un projet git *)
  print_string s
  
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

let read_str_until_eof_stdlib chan =
(* Eh oui parce que ce serait trop simple s'il n'y avait qu'une fonction *)
let has_ended = ref true and data = ref "" in
while !has_ended do
  try 
    data := add_char_to_str (Stdlib.input_char chan) (!data)
  with _ -> has_ended := false
done;
!data
  
  
  let compute_init s =
    (* Fonction qui est appelée lorsque la commande init est saisie *)
    (try
      Unix.mkdir s perm_base;
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


let compute_object_name name = 
  (* Donne le nom global de l'objet ie. depuis le repo principal *)
  let repo_name = repo_find () in
  let len_name = String.length name in
  let dir = String.sub name 0 2 in
  let obj_name = String.sub name 2 (len_name - 2) in
  (repo_name^"/.gdf/objects/"^dir^"/"^obj_name)

let extract_data obj =
  (* Renvoie le contenu d'un fichier sous forme de string *)
  let abs_path = compute_object_name obj in
  let f_channel = Stdlib.open_in abs_path in
  (read_str_until_eof_stdlib f_channel)

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
                let file_data = String.concat "\n" q in 
                (*l'ajout du \n est important pour la comparaison des tailles, sinon la taille
                d'entrée ne correspond pas à la taille de l'objet reconcaténé *)
                assert ((String.length file_data) = (int_of_string size));
                Blob(file_name, file_data)
    | _ -> failwith "non implémenté"

let serialize obj = (*le serialize du mr ne met pas le header. raph dit que c'est cringe. a voir...*)
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

let cat_file _ sha = (*le pelo fait des trucs bizarres avec object find, a mediter. le type ne sert a rien, voir doc git*)
  let obj = read_object sha in 
    match obj with
      | Blob(_, str) -> Printf.printf "%s" str (*thibault utilise serialize. apres discussion avec raph, on en a (il en a) conclu que c'est débile*)

let hash_file do_write typfile f_name =
  let f_channel = Stdlib.open_in f_name in
  let data = read_str_until_eof_stdlib f_channel in
  match typfile with
    | "blob" -> write_object (Blob(f_name, data)) do_write
    | _ -> failwith "hash_file à faire pour les autres types"

let f_test () =
  (* fonction de test *)
  let sha = write_object (Blob("test", "test test test")) true in
  match read_object sha with
    | Blob(a,b) -> print_string a; print_newline (); print_string b

let commit_parser name = (
  let obj_content = extract_data name in
  let data_list = String.split_on_char '\n' obj_content in
  List.iter (fun x -> Printf.printf "|%s" x) data_list;
  print_newline ();
  let tree = ref "" in
  let parent = ref [] in
  let author = ref (("",""),"") in
  let committer = ref (("",""),"") in
  let gpgsig = ref "" in 
  let name = ref "" in
  let rec compute_data lst cur_section = (match lst with
    | [] -> ()
    | x::q -> let line_list = String.split_on_char ' ' x in
              if line_list = [""] then compute_data q cur_section
              else if List.hd line_list <> "" then begin
                (match line_list with 
                  | "tree"::s::[]                           ->
                    tree := s;
                    compute_data q "tree"
                  | "tree"::_                               ->
                    raise (GdfError "mauvais format pour un tree")
                  | "parent"::s::[]                         -> 
                    parent := s::[];
                    compute_data q "parent"
                  | "parent"::_                             ->
                    raise (GdfError "mauvais format pour un parent")
                  | "author"::_                             ->
                    (* TO DO [URGENT] : le faire *)
                    compute_data q "author"
                  | "committer"::_                          ->
                    (* TO DO [URGENT] : le faire *)
                    compute_data q "committer"
                  | "gpgsig"::_                             ->
                    compute_data q "gpgsig"
                  | "-----END"::"PGP"::"SIGNATURE-----"::[] -> 
                    compute_data q "name"
                  | _                                       ->
                    name := String.concat "\n" (x::q))
              end
              else begin
                let data = (match line_list with
                  | ""::x::[] -> x
                  | x::[] -> x
                  | ""::"-----END"::"PGP"::"SIGNATURE-----"::[] -> 
                    "endsig"
                  | _ -> failwith ("impossible mais le compilateur ocaml est un peu
                  con sur les bords (c'est pas le couteau le plus aiguisé du tiroir si
                  vous voulez mon avis), data : ")) in
                let data_len = String.length data in
                if data_len >= 1 then
                (match cur_section with
                  | "parent" -> parent := data::(!parent);
                                compute_data q "parent"
                  | "gpgsig" -> gpgsig := (!gpgsig)^data;
                                compute_data q "gpgsig";
                  | "name"   -> name := String.concat "\n" (x::q)
                  | "endsig" -> compute_data q "name"
                  | _        -> failwith "oula c'est pas normal")
              end)
in compute_data data_list "tree";

let commit = {
  tree = !tree;
  parent = !parent;
  author = !author;
  committer = !committer;
  gpgsig = !gpgsig;
  name = !name
} in commit)
