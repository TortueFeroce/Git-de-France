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
mon parser à commit donc faudrait peut-être le changer en dessous mais flemme
pour l'instant *)

type commit = {
  tree : string;
  parent : string list;
  author : string;
  committer : string;
  gpgsig : string;
  name : string;
  size : int
}

type entries = {
  i_creation : int;
  i_last_modif : int;
  i_device : int;
  i_inode : int;
  i_perms : int;
  i_uid : int;
  i_gid : int;
  i_size : int;
  i_sha : string;
  i_name : string;
}

type index = {
  entries : entries list;
  version : int
}  

type ignoring_rules = Usual | Excl | Comment

type obj =
  | Blob of string * string
  | Commit of commit (* ah ouais en fait c'est pas très beau ce que j'ai fait là :'( *)
  | Tree of (string * string * string) list 

let perm_base = 0o777

let chr0 = Char.chr 0

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

let write_str_stdlib chan str =
  (* tout pareil qu'au dessus mais sans le zip *)
  for i = 0 to String.length str - 1 do
    Stdlib.output_char chan str.[i]
  done
  
let compute_init s =
  (* Fonction qui est appelée lorsque la commande init est saisie *)
  (try
    Unix.mkdir s perm_base;
  with Unix.Unix_error(_) -> () );
  Unix.mkdir (s^"/.gdf") perm_base;
  Unix.mkdir (s^"/.gdf/objects") perm_base;
  Unix.mkdir (s^"/.gdf/refs") perm_base;
  Unix.mkdir (s^"/.gdf/refs/tags") perm_base;
  Unix.mkdir (s^"/.gdf/refs/heads") perm_base; (* pour mettre les branches *)
  Unix.mkdir (s^"/.gdf/info") perm_base;
  let head_channel = Stdlib.open_out (s^"/.gdf/HEAD") in
    write_str_stdlib head_channel "ref: refs/heads/master";
  let index_channel = Stdlib.open_out (s^"/.gdf/index") in
    write_str_stdlib index_channel "DIRC0000000000000000";
  let exclude_channel = Stdlib.open_out (s^"/.gdf/info/exclude") in
    write_str_stdlib exclude_channel "";
  let config_channel = open_out (s^"/.gdf/config") in
    output_string config_channel "[core]\n\trepositoryformatversion = 0\n\tfilemode = false\n\tbare = false";
    close_out config_channel; close_out index_channel; close_out head_channel

  
let repo_find () = (
  (* Fonction qui renvoie le dossier dans lequel il y a le .gdf *)
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
  (* Renvoie le contenu d'un fichier qui n'est pas dans objects sous forme de string *)
  let f_channel = Stdlib.open_in obj in
  let str = read_str_until_eof_stdlib f_channel in
  str

let extract_data_zip obj =
  (* Renvoie le contenu d'un fichier qui n'est pas dans objects sous forme de string *)
  let f_channel = Gzip.open_in obj in
  read_str_until_eof f_channel

let compile_sig s =
  (* Fonction qui prend une signature stockée sous la forme
  ligne1 <espace> ligne2 <espace> ... 
  et renvoie la signature sous forme de string : ligne1 ligne2 ... 
  
  En gros ça recolle les bouts*)
  List.fold_left (fun acc x -> acc^(String.sub x 1 ((String.length x) - 1))) "" s

let computed_list_sig s =
  let list_sig = String.split_on_char ' ' s in
  let rec aux l =
  match l with
    | ""::q -> aux q
    | a::q -> (" "^a) :: aux q
    | [] -> []
  in aux list_sig

let concat_list_commit c =
  let concat_p_list = match c.parent with
  | [] -> ["parent "]
  | x::q -> 
  ["parent "^x]@(List.map (fun e -> " "^e) q) in
  ["tree "^(c.tree)]@concat_p_list@["author "^(c.author);"committer "^(c.committer);
  "gpgsig -----BEGIN PGP SIGNATURE-----";""]@(computed_list_sig c.gpgsig)@
  [" -----END PGP SIGNATURE-----";"";(c.name)]
  (* miam *)

let write_str chan str = (*on pourrait utiliser output_substring mais parait il c'est pas bien*)
  for i = 0 to (String.length str) - 1 do
    Gzip.output_char chan str.[i]
  done
  
let add_char_until c f_channel = 
  let data = read_str_until_eof f_channel in
  List.hd (String.split_on_char c data)
  (* let is_c = ref true in
  let acc = ref "" in
  while !is_c do
    let read_char = Gzip.input_char f_channel in
    if read_char = c then is_c := false
    else acc := add_char_to_str c !acc
  done; !acc *)
  
let commit_parser obj_content =
  (* Parser pour les commits, dsl c'est immonde *) (
  let data_list = String.split_on_char '\n' obj_content in
  let size = String.length obj_content in
  let tree = ref "" in
  let parent = ref [] in
  let author = ref "" in
  let committer = ref "" in
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
                    raise (GdfError "Mauvais format pour un tree")
                  | "parent"::s::[]                         -> 
                    parent := s::[];
                    compute_data q "parent"
                  | "parent"::_                             ->
                    raise (GdfError "Mauvais format pour un parent")
                  | "author"::a                             -> author := String.concat " " a;
                    compute_data q "author"
                  | "committer"::c                          -> committer := String.concat " " c;
                    compute_data q "committer"
                  | "gpgsig"::_                             ->
                    compute_data q "gpgsig"
                  | ""::"-----END"::"PGP"::"SIGNATURE-----"::[] -> 
                    compute_data q "name"
                  | _                                       ->
                    assert (cur_section = "name"); name := String.concat "\n" (x::q))
              end
              else begin
                let data = (match line_list with
                  | ""::y::[] -> y
                  | y::[] -> y
                  | ""::"-----END"::"PGP"::"SIGNATURE-----"::[] ->
                      let () = compute_data q "name" in ""  
                  | _ -> failwith ("impossible mais le compilateur ocaml est un peu
                  con sur les bords (c'est pas le couteau le plus aiguisé du tiroir si
                  vous voulez mon avis)")) in
                let data_len = String.length data in
                if data_len >= 1 then
                (match cur_section with
                  | "parent" -> parent := data::(!parent);
                                compute_data q "parent"
                  | "gpgsig" -> gpgsig := (!gpgsig)^(" "^data);
                                compute_data q "gpgsig";
                  | "name"   -> name := String.concat "\n" (x::q)
                  | _        -> failwith "oula c'est pas normal")
              end)
  in compute_data data_list "tree";

  let commit = {
    tree = !tree;
    parent = !parent;
    author = !author;
    committer = !committer;
    gpgsig = !gpgsig;
    name = !name;
    size = size
  } in commit)

let parse_pregzip_commit c =
  let obj_content = extract_data c in
  commit_parser obj_content

let tree_parser data = 
  let rec aux x = match x with
    | [] -> []
    | mode :: sha :: path :: q -> (mode, sha, path) :: aux q
    | _ -> raise (GdfError "Mauvais format pour un tree")
  in Tree(aux data)

let deserialize str =
  let data = String.split_on_char ('\n') str in
    match data with
      | "blob" :: size :: file_name :: q ->
                  let file_data = String.concat "\n" q in 
                  (*l'ajout du \n est important pour la comparaison des tailles, sinon la taille
                  d'entrée ne correspond pas à la taille de l'objet reconcaténé *)
                  assert ((String.length file_data) = (int_of_string size));
                  Blob(file_name, file_data)
      | "commit" :: _ :: q ->
                  (* let file_data = String.concat "\n" q in *)
                  (* TO DO : la taille ça marche pas *)
                  Commit(commit_parser (String.concat "\n" q))
      | "tree" :: _ :: q -> tree_parser q (*il peut pas y avoir de \n random dans un tree normalement donc la liste q c'est exactement ce qu'on veut*)
      | _ -> Printf.printf "data : %s\n" str; raise (GdfError "Mauvais format de données")

let serialize obj = (*le serialize du mr ne met pas le header. raph dit que c'est cringe. a voir...*)
  match obj with
    | Blob(file_name, file_data) ->
        let size = string_of_int (String.length file_data) in
        String.concat "\n" ["blob"; size; file_name; file_data]
    | Commit(c) ->
        let size = string_of_int c.size in
        let l_commit = concat_list_commit c in
        String.concat "\n" (["commit"; size]@l_commit)
    | Tree(l) -> 
      let contents_list = List.fold_left (fun acc (s,ss, sss) -> s :: ss :: sss :: acc) [] l in
      let tree_contents = String.concat "\n" contents_list in
      let size = string_of_int (String.length tree_contents) in
      String.concat "\n" ["tree"; size; tree_contents]
    (*| _ -> failwith "attends 2s connard"*)

let read_object sha =
  (* Fonction qui renvoie l'objet associé au haché sha *)
  let len_sha = String.length sha in
  let dir_sha = String.sub sha 0 2
  and obj_name = String.sub sha 2 (len_sha - 2) in
  let obj_path = (repo_find ())^"/.gdf/objects/" in
  let file_channel = Gzip.open_in (obj_path^dir_sha^"/"^obj_name) in
  let pre_obj = read_str_until_eof file_channel in
  Gzip.close_in file_channel;
  deserialize pre_obj

let find_type sha = 
  (* Prend un sha et renvoie le bon type *)
  let len_sha = String.length sha in
  let dir_sha = String.sub sha 0 2
  and obj_name = String.sub sha 2 (len_sha - 2) in
  let obj_path = (repo_find ())^"/.gdf/objects/" in
  let file_channel = Gzip.open_in (obj_path^dir_sha^"/"^obj_name) in
  let c_type = add_char_until '\n' file_channel in
  Gzip.close_in file_channel;
  c_type

let rec ref_resolve ref =
  (* Fonction qui prend une ref et qui renvoie le haché
  correspondant finalement à cette ref *)
  let path = (repo_find ())^("/.gdf/")^ref in
  try (let data = extract_data path in
  let first_bits = String.sub data 0 5 in
  let lasts_bits = String.sub data 5 (String.length data - 5) in
  match first_bits with
    | "ref: " -> ref_resolve lasts_bits
    | _       -> List.hd (String.split_on_char '\n' data))
  with _ -> raise (GdfError "Le nom donné ne correspond pas à une ref")


let object_resolve name = match name with
(* Fonction qui renvoie les possibilités pour un nom donné, un tag ou un sha
   sous forme de liste de string *)
    | "" -> raise (GdfError "Le nom est vide et ne peut donc pas correspondre
    à un haché")
    | "HEAD" -> [("head",ref_resolve "HEAD")]
    | _ -> (let possibilities = ref [] in
          let name_len = String.length name in
          if name_len >= 4 then begin
            let prefix = String.sub name 0 2 in
            let suffix = String.sub name 2 (name_len - 2) in
            let path = (repo_find ())^"/.gdf/objects/"^prefix in
            let files_here = Sys.readdir path in
            Array.iter (fun x -> if not (Sys.is_directory (path^"/"^x)) 
                                && (String.sub x 0 (name_len - 2) = suffix)
                                then possibilities := ("blob",prefix^x)::(!possibilities)) files_here
          end;(
          try let sha_tag = ref_resolve ("refs/tags/"^name)
              in possibilities := ("tag",sha_tag)::(!possibilities) 
          with _ -> ();
          try let sha_tag = ref_resolve ("refs/heads/"^name)
              in possibilities := ("head",sha_tag)::(!possibilities) 
          with _ -> ());
          (* try let sha_tag = ref_resolve ("refs/remotes/"^name)
              in possibilities := sha_tag::(!possibilities) 
          with _ -> ();
          pour l'instant on ne le met pas mais ça peut servir dans la suite*)
          !possibilities)

let object_find name fmt =
  (* fonction qui renvoie le sha d'un objet dont le nom est name *)
  let list_resolve = object_resolve name in
  if fmt = "" then begin
    if List.length list_resolve <> 1
    then raise (GdfError ("Le nom : "^name^" ne correspond pas à une occurence valide
  ou correspond à plusieurs occurences")) (* TO DO : faire une meilleure erreur *)
    else let (_,y) = List.hd list_resolve in y end
  else begin
    let rec aux list_resolve fmt = match list_resolve, fmt with
    | [],_ -> raise (GdfError "Aucune occurence trouvée")
    | (_,x)::_,_ when (find_type x = fmt) -> x
    | ("tag",x)::_,_ -> ref_resolve ("tags/"^x)
    | (_,x)::_,"tree" when (find_type x = "commit") -> (*c'est pas tres beau mais ça doit marcher*)
        let obj = read_object x in (match obj with
          | Commit c -> c.tree
          | _ -> raise (GdfError "Mauvais type de fichier"))
    | ("head",x)::_,_ -> Printf.printf "type : %s\nx : %s\n" (find_type x) x;x
    | _::q,_ -> aux q fmt
  in aux list_resolve fmt
  end


let write_object obj do_write =
  let serialized_obj = serialize obj in
  let pre_sha = (Sha1.string serialized_obj) in
  let sha = Sha1.to_hex pre_sha in
  (if do_write then (
    let first_sha = String.sub sha 0 2 
    and last_sha = String.sub sha 2 ((String.length sha) - 2) in
      let repo_path = repo_find () in
        (if not (Sys.file_exists (repo_path^"/.gdf/objects/"^first_sha)) 
          then Unix.mkdir (repo_path^"/.gdf/objects/"^first_sha) perm_base); (*crée le repertoire ou on range l'objet s'il existe pas encore*)
        let file_channel = Gzip.open_out (repo_path^"/.gdf/objects/"^first_sha^"/"^last_sha) in
          write_str file_channel serialized_obj;
          Gzip.close_out file_channel));
  sha

let cat_file fmt sha = (*le pelo fait des trucs bizarres avec object find, a mediter. le type ne sert a rien, voir doc git*)
  let obj = read_object (object_find sha fmt) in 
    match obj with
      | Blob(_, str) -> Printf.printf "%s" str (*thibault utilise serialize. apres discussion avec raph, on en a (il en a) conclu que c'est débile*)
      | Commit(c) -> Printf.printf "%s" (String.concat "\n" (concat_list_commit c))
      | Tree(l) -> 
        let contents_list = List.fold_left (fun acc (s,ss, sss) -> s :: ss :: sss :: acc) [] l in
        let tree_contents = String.concat "\n" contents_list in (*nos trees seront illisibles...*)
        Printf.printf "%s" tree_contents
      (*| _ -> failwith "dune t'es vraiment casse couille quand tu t'y mets"*)

let hash_file do_write typfile f_name =
  (* renvoie le sha d'un fichier *)
  let f_channel = Stdlib.open_in f_name in
  let data = read_str_until_eof_stdlib f_channel in
  close_in f_channel;
  match typfile with
    | "blob" -> write_object (Blob(f_name, data)) do_write
    | "commit" -> write_object (Commit(parse_pregzip_commit f_name)) do_write
    | "tree" -> 
      let tree_contents = String.split_on_char '\n' data in
      write_object (tree_parser tree_contents) do_write
    | _ -> raise (GdfError (typfile ^ " n'est pas un type de fichier valide"))

let compute_log name = 
  (*alors la, il faut qu'on en parle. cette fonction donne l'arbre des commits en format .dot direct dans la console. okok pas de soucis. sauf que 1) on donne pas tout le log juste l'historique des commits passés en argument, 2) on peut passer qu'un seul commit en argument, 3) ya pas de merge. ?????? c'est juste une ligne ton log??? fin je vois pas l'interet de se casser les couilles avec graphviz pour faire juste une liste dans l'ordre. au passage, l'abscence de merge rends plein de trucs obsolètes, style la possibilité d'avoir >1 parents. apres, si thibault polge demande moi j'execute. mais ça sert a rien. en vrai peut etre on peut donner la possibilté d'avoir une liste d'arguments plus tard? ça serait rigolo au moins un peu. ou alors peut etre je suis juste con et j'ai mal compris. au passage tu sais ce que c'est une mite à l'envers? c'est une co-mite (commit). c'est pas grave si t'as pas compris je sais que mon humour est un peu trop subtil pour beaucoup de gens. bon allez je vais me log la gueule c'est tipar (parti en verlan)*)
  let sha = object_find name "commit" in
  (* Printf.printf "name : %s sha : %s\n" name sha; *)
  (* Printf.printf "sha : %s\n" sha; *)
  Printf.printf "digraph wyaglog{\n\tnode[shape=rect]";
  let seen_commits = Hashtbl.create 64 in (*hmm, c'est pas beau. ya surement un module mieux mais raph est pas la pour me dire que en fait c'est pas comme ça qu'on fait*)
  let rec log_graphviz comm = if not (Hashtbl.mem seen_commits comm.name) then
      Hashtbl.add seen_commits comm.name true;
      match comm.parent with
        | [] -> ()
        | ""::[] -> ()
        | l -> List.iter (fun x -> 
            let truc = read_object x in match truc with
              | Commit(c) -> 
                  Printf.printf "%s -> %s\n" c.name comm.name;
                  log_graphviz c
              | _ -> raise (GdfError "L'objet n'est pas un commit")) l
  in match read_object sha with (*le prochain match que je dois ecrire ou ya un seul cas qui fonctionne je me defenestre*)
    | Commit(coucou) -> log_graphviz coucou; Printf.printf "}\n"
    | _ -> raise (GdfError (sha ^ " n'est pas un commit"))

let rec tree_checkout tree path = 
  let item (_, sha, file) =
    let obj = read_object sha in
    let dest = (path ^ "/" ^ file) in
    match obj with
      | Tree(_) ->
        Unix.mkdir dest perm_base;
        tree_checkout obj dest
      | Blob(_, file_data) -> 
        let channel = Stdlib.open_out dest in
        Stdlib.output_string channel file_data; Stdlib.close_out channel
      | _ -> raise (GdfError "Mauvais type de fichier pour un objet dans un tree")
  in match tree with
    | Tree(l) -> List.iter item l
    | _ -> raise (GdfError "Mauvais type d'objet - un tree était attendu")

let compute_checkout name dir =
  let obj = read_object (object_find name "commit") in
  let tree = match obj with
    | Commit(c) -> read_object c.tree
    | _ -> raise (GdfError (name ^ " is not a commit"))
  (*c'est pas tres beau trois if de suite mais ça permet de gérer les erreurs un peu mieux*)
  in (if (Sys.file_exists dir) then(
    if not (Sys.is_directory dir) then raise (GdfError ("Le fichier " ^ dir ^ " n'est pas un dossier"));
    if Sys.readdir dir <> [||] then raise (GdfError ("Le dossier " ^ dir ^ " n'est pas vide")))
  else Unix.mkdir dir perm_base);
  tree_checkout tree (Unix.realpath dir)
  

let print_refs () =
  Unix.chdir ((repo_find ())^"/.gdf/");
  let ref_list = ref [] in
  let path0 = "refs/" in
  let rec aux path =
    let f_list = Sys.readdir path in
    Array.iter (fun x -> if Sys.is_directory (path^x) then aux (path^x^"/")
                        else begin
                          ref_list := (extract_data (path^x),path)::(!ref_list) end) f_list
  in aux path0;
  List.iter (fun (sha, path) -> Printf.printf "%s %s\n" sha path) (!ref_list)

let print_tag () =
  Unix.chdir ((repo_find ())^"/.gdf/refs/tags");
  let file_list = Sys.readdir "." in
  Array.iter (fun x -> Printf.printf "%s\n" x) file_list

let compute_tag name obj =
  (* Fonction qui crée un tag dont le nom est <name> et qui
  est relié à l'objet <obj> *)
  let data_obj = extract_data obj in
  let sha = Sha1.to_hex (Sha1.string data_obj) in
  Unix.chdir ((repo_find ())^"/.gdf/refs/tags");
  if Sys.file_exists name then raise (GdfError ("Le tag "^name^" existe déjà"))
  else begin
  let f_channel = Stdlib.open_out name in
  write_str_stdlib f_channel sha;
  close_out f_channel end


let compute_rev_parse name fmt =
  Printf.printf "%s" (object_find name fmt)

let bit_string_to_int s =
  (* Passe du binaire à la base 10 *)
  let n = String.length s in
  let res = ref 0 in
  for i = 0 to n-1 do
    res := 2*(!res) + int_of_string (Char.escaped s.[i])
  done;
  !res

let int_to_bit_string n =
  let rec aux i s = match i with
    | 0 -> s
    | _ -> let r = i mod 2 in
           if r = 0 then aux (i/2) ("0"^s)
           else aux (i/2) ("1"^s)
  in let base_str = aux n "" in
  let len_base = String.length base_str in
  assert (len_base <= 8);
  let paddington = String.make (8 - len_base) '0' in
  paddington ^ base_str

let entries_parser entry =
  let content_list = String.split_on_char chr0 entry in
  match content_list with
    | creation ::
      last_modif ::
      device ::
      inode ::
      perms ::
      uid ::
      gid ::
      size ::
      sha ::
      name :: []
      -> {i_creation = int_of_string creation;
      i_last_modif = int_of_string last_modif;
      i_device = int_of_string device;
      i_inode = int_of_string inode;
      i_perms = int_of_string perms;
      i_uid = int_of_string uid;
      i_gid = int_of_string gid;
      i_size = int_of_string size;
      i_sha = sha;
      i_name = name}
    | _ -> raise (GdfError "Mauvais format pour une entrée de l'index")

let index_parser () = 
  (* fonction qui parse le fichier index dans .gdf
  plus précisémment, renvoie un type index 
  
  ATTENTION : dans le fichier index les bits de poids le plus fort sont le plus 
  à gauche, ie. (1000)_2 = (4)_10 *)

  Unix.chdir (repo_find ());
  let data_ind = extract_data ".gdf/index" in (* on suppose que le fichier
index n'est pas zippé pour l'instant, il faudra changer cette ligne si on
choisit de le zipper *)
  let len_ind = String.length data_ind in
  let version = bit_string_to_int (String.sub data_ind 4 8) in
  let _ = bit_string_to_int (String.sub data_ind 12 8) in
  if len_ind = 20 then
  {
    entries = [];
    version = version
  }
  else begin let entries = String.split_on_char '\n' (String.sub data_ind 21 (len_ind - 21)) in
  {
    entries = List.map (fun e -> entries_parser e) entries;
    version = version
  } end

let get_index_files () =
  (* Fonction qui renvoie les fichiers qui ont été staged dans index *)
  List.map (fun e -> e.i_name) ((index_parser ()).entries)

let print_index_files () =
  (* Fonction qui affiche les noms des fichiers dans index *)
  List.iter (fun x -> Printf.printf "%s\n" x) (get_index_files ())

let gitignore_parse lines =
  List.map (fun line -> 
  if String.length line = 0 then (Comment,"")
  else begin match (String.sub line 0 1) with
    | "#" -> (Comment,"")
    | "!" -> (Excl, String.sub line 1 (String.length line - 1))
    | _ -> (Usual, line)
end) lines

let get_rules_from_file file =
  let data = extract_data file in
  if data = "" then [] else begin
  let lines = String.split_on_char '\n' data in
  gitignore_parse lines end

let go_into_dir_where_file_is file =
  let lst = String.split_on_char '/' file in
  let rec aux l = match l with
    | _::[] -> []
    | x::q -> x::(aux q)
    | _ -> raise (GdfError "Le fichier n'est pas un fichier") (*ça veut rien dire...*)
  in String.concat "/" (aux lst)

let get_rules_for_file file =
  let repo = repo_find () in
  Unix.chdir (repo^(go_into_dir_where_file_is file));
  let rec aux () =
    if Unix.getcwd () = repo then begin
      if Sys.file_exists ".gdfignore" then
      [get_rules_from_file (repo^"/.gdf/info/exclude");get_rules_from_file ".gdfignore"]
      else [get_rules_from_file (repo^"/.gdf/info/exclude")]
    end
    else begin
      Unix.chdir ("../"^(Unix.getcwd ()));
      if Sys.file_exists ".gdfignore" then
        (aux ())@[get_rules_from_file ".gdfignore"]
      else aux ()
    end
  in aux ()

let rec check_ignore1 rules file = match rules with
  | (v,pat)::_ when Core_unix.fnmatch ~pat:pat file && v != Comment -> v
  | _::q -> check_ignore1 q file
  | [] -> Comment

let check_ignore file =
  let lst_rules = get_rules_for_file file in
  let rec aux l = match l with
    | [] -> Comment
    | x::q -> let res = check_ignore1 x file in
              match res with
                | Comment -> aux q
                | _ -> res
  in aux lst_rules

let rec give_list_check_ignore file_lst = match file_lst with
  | [] -> []
  | x::q -> (match check_ignore x with
              | Usual -> x::(give_list_check_ignore q)
              | _ -> give_list_check_ignore q)

let compute_check_ignore file_lst =
  List.iter (fun x -> Printf.printf "%s\n" x) (give_list_check_ignore file_lst)

let are_we_on_branch () =
  let repo = repo_find () in
  let head_data = extract_data (repo^("/.gdf/HEAD")) in
  let begin_head_data = String.sub head_data 0 16 in
  let end_head_data = String.sub head_data 16 (String.length head_data - 16) in
  match begin_head_data with
    | "ref: refs/heads/" -> end_head_data
    | _ -> ""

let cmd_status_branch () =
  (* Donne le début du status *)
  let branch = are_we_on_branch () in
  match branch with
    | "" -> Printf.printf "HEAD detached at %s\n" (object_find "HEAD" "")
    | _ -> Printf.printf "On branch %s\n" branch

let is_subtree path =
  try Sys.is_directory path
with _ -> false

let tree_to_dict ref =
  (* Transorme un tree en Hashtbl dont les clefs sont les noms de fichiers
   et les valeurs sont les hachés des fichiers *)
  let table = Hashtbl.create 16 in
  let rec aux ref =
    let tree_sha = object_find ref "tree" in
    (* Printf.printf "sha_t : %s\n" tree_sha; *)
    let tree = read_object tree_sha in
    let rec compute_tree t = match t with
      | [] -> ()
      | (_,sha,path)::q -> (*Printf.printf "path : %s sha : %s\n" path sha;*)
                              if is_subtree path then aux sha
                              else Hashtbl.add table path sha; compute_tree q
    in match tree with
      | Tree(t) -> compute_tree t
      | _ -> failwith "ça n'arrivera pas sauf si un connard essaie de nous faire une
      mauvaise blague, et alors là il va entendre de quel bois je me chauffe ..."
  in aux ref; table

let cmd_status_head_index () =
  Printf.printf "\nChanges to be committed:\n";
  let head = tree_to_dict "HEAD" in
  let index = index_parser () in
  List.iter (fun e -> try let sha_in_head = Hashtbl.find head (e.i_name) in
                          (* Printf.printf "name : %s\n" e.i_name;
                          Printf.printf "sha_in_head : %s sha : %s\n" sha_in_head e.i_sha; *)
                          if sha_in_head <> e.i_sha 
                          then Printf.printf "\tmodified:%s\n" e.i_name;
                          Hashtbl.remove head e.i_name
                        with _ -> Printf.printf "\tadded:\t%s\n" e.i_name) index.entries;
  Hashtbl.iter (fun k _ -> Printf.printf "\tdeleted: %s\n" k) head

let cmd_status_index_worktree () =
  Printf.printf "\nChanges not staged for commit:\n";

  let index = index_parser () in
  List.iter (fun e -> if not (Sys.file_exists e.i_name)
                      then Printf.printf "\tdeleted: %s\n" e.i_name
                      else begin
                        let stats = Unix.stat e.i_name in
                        (* Printf.printf "creation : %f ctime : %f last_modif : %f mtime : %f"
                      e.i_creation stats.st_ctime e.i_last_modif stats.st_mtime; *)
                        if (e.i_creation <> (int_of_float stats.st_ctime)) ||
                           (e.i_last_modif <> (int_of_float stats.st_mtime))
                        then begin
                          let new_sha = hash_file false "blob" e.i_name in
                          (* Printf.printf "name : %s new_sha : %s old_sha : %s\n" e.i_name new_sha e.i_sha; *)
                          if new_sha <> e.i_sha then
                            Printf.printf "\tmodified:%s\n" e.i_name
                        end
                      end
              ) index.entries;
  Printf.printf "\nUntracked files:\n";
  let all_files = Array.to_list (Sys.readdir (repo_find ())) in
  List.iter (fun x -> Printf.printf "\t%s\n" x) (give_list_check_ignore all_files)

let compute_status () =
  cmd_status_branch ();
  cmd_status_head_index ();
  cmd_status_index_worktree ()

let create_entry path _ =
  (* Fonction qui prend un path et un format et qui
  renvoie une entrée qui correspond à ce fichier *)
  let stats = Unix.stat path in
  let repo = (repo_find ())^"/" in
  let len_repo = String.length repo in
  let len_path = String.length path in
  {
    i_creation = int_of_float stats.st_ctime;
    i_last_modif = int_of_float stats.st_mtime;
    i_device = stats.st_dev;
    i_inode = stats.st_ino;
    i_perms = stats.st_perm;
    i_uid = stats.st_uid;
    i_gid = stats.st_gid;
    i_size = stats.st_size;
    i_sha = hash_file true "blob" path;
    i_name = String.sub path len_repo (len_path - len_repo);
  }

let write_entry channel entry =
  let write_0 () = Stdlib.output_char channel chr0 in
  Stdlib.output_char channel '\n';
  write_str_stdlib channel (string_of_int entry.i_creation);
  write_0 ();
  write_str_stdlib channel (string_of_int entry.i_last_modif);
  write_0 ();
  write_str_stdlib channel (string_of_int entry.i_device);
  write_0 ();
  write_str_stdlib channel (string_of_int entry.i_inode);
  write_0 ();
  write_str_stdlib channel (string_of_int entry.i_perms);
  write_0 ();
  write_str_stdlib channel (string_of_int entry.i_uid);
  write_0 ();
  write_str_stdlib channel (string_of_int entry.i_gid);
  write_0 ();
  write_str_stdlib channel (string_of_int entry.i_size);
  write_0 ();
  write_str_stdlib channel entry.i_sha;
  write_0 ();
  write_str_stdlib channel entry.i_name

let index_write index = 
  (*écrit l'index. ça djoufara l'index d'avant, fais gaffe ma gueule*)
  let index_channel = Stdlib.open_out ".gdf/index" in
    write_str_stdlib index_channel "DIRC";
    write_str_stdlib index_channel (int_to_bit_string index.version);
    write_str_stdlib index_channel (int_to_bit_string (List.length index.entries));
    List.iter (write_entry index_channel) index.entries; (*poualala la curryfication ça me donne envie de manger indien*)
    Stdlib.close_out index_channel (*je l'ai pas oublié cette fois ci quel boss*)

let compute_rm path_list do_delete skip_missing =
  (* fonction qui supprime des fichiers *)
  let index = index_parser () in
  let repo = (repo_find ()) ^ "/" in
  let abs_paths = Hashtbl.create 16 in
  List.iter (fun path -> 
    let absolute = Unix.realpath path in
      assert (String.starts_with ~prefix:repo absolute);
      Hashtbl.add abs_paths absolute true) path_list;
  let kept_entries = ref [] in
  let remove = ref [] in
  List.iter (fun entry -> let full_path = repo ^ entry.i_name in
    if Hashtbl.mem abs_paths full_path then
      (remove := full_path :: !remove;
      Hashtbl.remove abs_paths full_path)
    else
      kept_entries := entry :: !kept_entries) index.entries;
  if (Hashtbl.length abs_paths > 0) && (not skip_missing) then raise (GdfError "Impossible de supprimer des fichiers absents de l'index");
  (if do_delete then
    List.iter Unix.unlink !remove);
  index_write ({entries = !kept_entries; version = index.version})

let compute_add paths delete skip_missing =
  (* ajoute les fichiers dans paths dans l'index *)
  compute_rm paths delete skip_missing;
  let repo = repo_find () in
  let list_clean = ref [] in
  List.iter (fun path -> let abs_path = repo^"/"^path in
                         if not (Sys.file_exists abs_path)
                         then raise (GdfError ("Le fichier "^path^" n'existe pas
                         ou n'est pas dans le worktree"))
                         else list_clean := abs_path::(!list_clean)) paths;
  let cur_index = index_parser () in
  let entries = ref cur_index.entries in
  List.iter (fun x ->
    let entry = create_entry x "blob" in
    entries := entry::(!entries)) !list_clean;
  let new_index = {
    entries = !entries;
    version = cur_index.version
  } in index_write new_index

let true_dirname file = 
  match (Filename.dirname file) with
    | "." -> ""
    | unautrestringnimportelequel -> unautrestringnimportelequel

let tree_from_index index =
  let contents = Hashtbl.create 32 in
  Hashtbl.add contents "" [];

  let add_to_contents entry =
    (* Fonction qui rentre les entries dans la table de hachage *)
    let dirname = ref (true_dirname entry.i_name) in
    while !dirname <> "" do
      (* Printf.printf "dirname : %s\n" !dirname; *)
      (if not (Hashtbl.mem contents !dirname) then
        Hashtbl.add contents !dirname []);
      dirname := true_dirname !dirname;
    done;
    let dir = true_dirname entry.i_name
    in let entry_list = Hashtbl.find contents dir in
    Hashtbl.replace contents dir (entry :: entry_list)
  in List.iter add_to_contents index.entries;

  let sorted_paths = List.rev (List.sort compare (List.of_seq (Hashtbl.to_seq_keys contents))) in (*quelle horreur. mais ça marche (normalement...)*)
  let sha = ref "" in

  (* Hashtbl.iter (fun a l -> Printf.printf "path : %s\n" a;
                          List.iter (fun x -> Printf.printf "\te_name : %s\n" x.i_name) l) contents; *)

  let create_tree path =
    let make_leaf entry =
      let leaf_mode = entry.i_perms in
      (string_of_int leaf_mode, entry.i_sha, Filename.basename entry.i_name) (*TODO : ecrire les perms en octal, pas en decimal comme je le fais la*)
    in let tree = Tree (List.map make_leaf (Hashtbl.find contents path)) in
    sha := write_object tree true;
    let fake_entry = {
      i_creation = 0;
      i_last_modif = 0;
      i_device = 0;
      i_inode = 0;
      i_perms = 0o40000;
      i_uid = 0;
      i_gid = 0;
      i_size = 0;
      i_sha = !sha;
      i_name = Filename.basename path;
    } in
    let entry_list = Hashtbl.find contents (true_dirname path) in
    Hashtbl.replace contents (true_dirname path) (fake_entry :: entry_list)

  in List.iter create_tree sorted_paths;
  !sha

let commit_create tree parent author message =
  let new_commit = {
    tree = tree;
    parent = parent;
    author = author;
    committer = author;
    gpgsig = ""; (* on pourrait peut-être en mettre une mais c'est pas le plus urgent *)
    name = message;
    size = 0 (* TO DO : changer ça *)
  } in
  write_object (Commit(new_commit)) true
  
let compute_commit message =
  let repo = repo_find () in
  let index = index_parser () in
  let tree = tree_from_index index in
  (* Printf.printf "tree : %s\n" tree; *)
  let commit = 
    try (let ref_head = object_find "HEAD" "" in
        commit_create tree [ref_head] "" message)
    with _ -> (
      let commit2 = commit_create tree [] "" message in
      Printf.printf "commit : %s\n" (String.escaped commit2);
      let master_channel = Stdlib.open_out (repo^"/.gdf/refs/heads/master") in
      write_str_stdlib master_channel commit2;
      Stdlib.close_out master_channel;
      commit2) in
  let branch = are_we_on_branch () in 
  match branch with
    | "" ->let branch_channel = Stdlib.open_out (repo^"/.gdf/HEAD") in
            write_str_stdlib branch_channel "\n";
            Stdlib.close_out branch_channel
    | _ -> let branch_channel = Stdlib.open_out (repo^"/.gdf/refs/heads/"^branch) in
            write_str_stdlib branch_channel (commit^"\n");
            Stdlib.close_out branch_channel

let f_test () =
  (* fonction de test *)
  let repo = repo_find () in
  let chan = Stdlib.open_out (repo^"/fichier_test") in
  write_str_stdlib chan "79d5e527bc66b195b63f7267992e3dcffa3de016";
  Stdlib.close_out chan 