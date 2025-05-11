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
  i_creation : float;
  i_last_modif : float;
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

module StringSet = Set.Make(String)

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
  let head_channel = Stdlib.open_out "/.gdf/HEAD" in
    write_str_stdlib head_channel "ref: refs/heads/master\n";
  let index_channel = Stdlib.open_out "/.gdf/index" in
    write_str_stdlib index_channel "DIRC0000000000000000\n";
  let exclude_channel = Stdlib.open_out "/.gdf/info/exclude" in
    write_str_stdlib exclude_channel "";
  let config_channel = open_out (s^"/.gdf/config") in
    output_string config_channel "[core]\n\trepositoryformatversion = 0\n\tfilemode = false\n\tbare = false";
    close_out config_channel

  
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
  read_str_until_eof_stdlib f_channel

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
  let is_c = ref true in
  let acc = ref "" in
  while !is_c do
    let read_char = Gzip.input_char f_channel in
    if read_char = c then is_c := false
    else acc := add_char_to_str c !acc
  done; !acc
  
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
                    raise (GdfError "mauvais format pour un tree")
                  | "parent"::s::[]                         -> 
                    parent := s::[];
                    compute_data q "parent"
                  | "parent"::_                             ->
                    raise (GdfError "mauvais format pour un parent")
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
    | _ -> failwith "ya un souci qqpart a mon avis"
  in Tree(aux data)

let find_type sha = 
  let len_sha = String.length sha in
  let dir_sha = String.sub sha 0 2
  and obj_name = String.sub sha 2 (len_sha - 2) in
  let obj_path = (repo_find ())^"/.gdf/objects/" in
  let file_channel = Gzip.open_in (obj_path^dir_sha^"/"^obj_name) in
  add_char_until '\n' file_channel

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
      | _ -> failwith "ta gueule le compilateur ocaml, ce cas n'arrive jamais (en fait si si tu tapes un sha qui existe pas mais la c'est toi qui trolle)"

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
      | Commit(c) -> Printf.printf "%s" (String.concat "\n" (concat_list_commit c))
      | Tree(l) -> 
        let contents_list = List.fold_left (fun acc (s,ss, sss) -> s :: ss :: sss :: acc) [] l in
        let tree_contents = String.concat "\n" contents_list in (*nos trees seront illisibles...*)
        Printf.printf "%s" tree_contents
      (*| _ -> failwith "dune t'es vraiment casse couille quand tu t'y mets"*)

let hash_file do_write typfile f_name =
  let f_channel = Stdlib.open_in f_name in
  let data = read_str_until_eof_stdlib f_channel in
  match typfile with
    | "blob" -> write_object (Blob(f_name, data)) do_write
    | "commit" -> write_object (Commit(parse_pregzip_commit f_name)) do_write
    | "tree" -> 
      let tree_contents = String.split_on_char '\n' data in
      write_object (tree_parser tree_contents) do_write
    | _ -> failwith "hash_file à faire pour les autres types"

let compute_log sha = 
  (*alors la, il faut qu'on en parle. cette fonction donne l'arbre des commits en format .dot direct dans la console. okok pas de soucis. sauf que 1) on donne pas tout le log juste l'historique des commits passés en argument, 2) on peut passer qu'un seul commit en argument, 3) ya pas de merge. ?????? c'est juste une ligne ton log??? fin je vois pas l'interet de se casser les couilles avec graphviz pour faire juste une liste dans l'ordre. au passage, l'abscence de merge rends plein de trucs obsolètes, style la possibilité d'avoir >1 parents. apres, si thibault polge demande moi j'execute. mais ça sert a rien. en vrai peut etre on peut donner la possibilté d'avoir une liste d'arguments plus tard? ça serait rigolo au moins un peu. ou alors peut etre je suis juste con et j'ai mal compris. au passage tu sais ce que c'est une mite à l'envers? c'est une co-mite (commit). c'est pas grave si t'as pas compris je sais que mon humour est un peu trop subtil pour beaucoup de gens. bon allez je vais me log la gueule c'est tipar (parti en verlan)*)
  Printf.printf "digraph wyaglog{\n\tnode[shape=rect]";
  let seen_commits = ref StringSet.empty in (*hmm, c'est pas beau. ya surement un module mieux mais raph est pas la pour me dire que en fait c'est pas comme ça qu'on fait*)
  let rec log_graphviz comm = if not (StringSet.mem comm.name !seen_commits) then
      seen_commits := StringSet.add comm.name !seen_commits;
      match comm.parent with
        | [] -> ()
        | l -> List.iter (fun x -> 
            let truc = deserialize x in match truc with
              | Commit(c) -> 
                  Printf.printf "%s -> %s" c.name comm.name;
                  log_graphviz c
              | _ -> failwith "not a commit") l
  in match deserialize sha with (*le prochain match que je dois ecrire ou ya un seul cas qui fonctionne je me defenestre*)
    | Commit(coucou) -> log_graphviz coucou; Printf.printf "}"
    | _ -> failwith "not a commit, sorry :)"

let rec tree_checkout tree path = 
  let item (_, sha, file) =
    let obj = read_object sha in
    match obj with
      | Tree(_) -> tree_checkout obj (path ^ "/" ^ file)
      | Blob(file_name, file_data) -> 
        let channel = Stdlib.open_out file_name in
        Stdlib.output_string channel file_data
      | _ -> failwith "nope"
  in match tree with
    | Tree(l) -> List.iter item l
    | _ -> failwith "alors celle la d'erreur si elle arrive je me coupe une couille"

let compute_checkout sha dir =
  let obj = read_object sha in
  let tree = match obj with
    | Commit(c) -> read_object c.tree
    | _ -> failwith "t'es malin toi"
  (*c'est pas tres beau trois if de suite mais ça permet de gérer les erreurs un peu mieux*)
  in if (Sys.file_exists dir) then
    if not (Sys.is_directory dir) then failwith "File isn't a directory";
    if Sys.readdir dir <> [||] then failwith "Directory isn't empty"
  else Unix.mkdir dir perm_base;
  tree_checkout tree (Unix.realpath dir)
  
let rec ref_resolve ref =
  (* Fonction qui prend une ref et qui renvoie le haché
  correspondant finalement à cette ref *)
  let path = (repo_find ())^("/.gdf/")^ref in
  try (let data = extract_data path in
  let first_bits = String.sub data 0 5 in
  let lasts_bits = String.sub data 5 (String.length data - 5) in
  match first_bits with
    | "ref: " -> ref_resolve lasts_bits
    | _       -> data)
  with _ -> raise (GdfError "le nom donné ne correspond pas à une ref")

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
  if Sys.file_exists name then raise (GdfError ("le tag "^name^" existe déjà"))
  else begin
  let f_channel = Stdlib.open_out name in
  write_str_stdlib f_channel sha end

let object_resolve name = match name with
(* Fonction qui renvoie les possibilités pour un nom donné, un tag ou un sha
   sous forme de liste de string *)
    | "" -> raise (GdfError "le nom est vide et ne peut donc pas correspondre
    à un haché")
    | "HEAD" -> [("head",ref_resolve "HEAD")]
    | _ -> (let possibilities = ref [] in
          let name_len = String.length name in
          if name_len >= 4 then begin
            let prefix = String.sub name 0 2 in
            let suffix = String.sub name 2 (name_len - 2) in
            let path = (repo_find ())^"/objects/"^prefix in
            let files_here = Sys.readdir path in
            Array.iter (fun x -> if not (Sys.is_directory x) 
                                && (String.sub x 0 (name_len - 2) = suffix)
                                then possibilities := ("blob",x)::(!possibilities)) files_here
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
    then raise (GdfError ("le nom : "^name^" ne correspond pas à une occurence valide
  ou correspond à plusieurs occurences")) (* TO DO : faire une meilleure erreur *)
    else let (_,y) = List.hd list_resolve in y end
  else begin
    let rec aux list_resolve fmt = match list_resolve, fmt with
    | [],_ -> raise (GdfError "aucune occurence trouvée")
    | ("head",x)::_,_ -> x
    | (_,x)::_,_ when (find_type x = fmt) -> x
    | ("tag",x)::_,_ -> ref_resolve ("tags/"^x)
    | (_,x)::_,"tree" when (find_type x = "commit") -> (*c'est pas tres beau mais ça doit marcher*)
      let obj = read_object x in (match obj with
        | Commit c -> c.tree
        | _ -> failwith "peut pas arriver ou alors ya un gros souci qqpart")
    | _::q,_ -> aux q fmt
  in aux list_resolve fmt
  end

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
      -> {i_creation = float_of_string creation;
      i_last_modif = float_of_string last_modif;
      i_device = int_of_string device;
      i_inode = int_of_string inode;
      i_perms = int_of_string perms;
      i_uid = int_of_string uid;
      i_gid = int_of_string gid;
      i_size = int_of_string size;
      i_sha = sha;
      i_name = name}
    | _ -> failwith "wrong format for an entry"

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
  let entries = String.split_on_char '\n' (String.sub data_ind 21 (len_ind - 21)) in
  {
    entries = List.map (fun e -> entries_parser e) entries;
    version = version
  }

let print_index_files () =
  (* Fonction qui affiche les noms des fichiers dans index *)
  let files = List.map (fun e -> e.i_name) ((index_parser ()).entries) in
  List.iter (fun x -> Printf.printf "%s\n" x) files

let gitignore_parse lines =
  List.map (fun line -> 
  if String.length line = 0 then (Comment,"")
  else begin match (String.sub line 0 1) with
    | "#" -> (Comment,"")
    | "!" -> (Excl, String.sub line 1 (String.length line - 1))
    | _ -> (Usual, String.sub line 1 (String.length line - 1))
end) lines

let get_rules_from_file file =
  let data = extract_data file in
  let lines = String.split_on_char '\n' data in
  gitignore_parse lines

let go_into_dir_where_file_is file =
  let lst = String.split_on_char '/' file in
  let rec aux l = match l with
    | _::[] -> []
    | x::q -> x::(aux q)
    | _ -> failwith "le fichier n'est pas un fichier"
  in String.concat "/" (aux lst)

let get_rules_for_file file =
  let repo = repo_find () in
  Unix.chdir (go_into_dir_where_file_is file);
  let rec aux () =
    if Unix.getcwd () = repo then begin
      if Sys.file_exists ".gdfignore" then
      (get_rules_from_file (repo^"/.gdf/info/exclude"))@(get_rules_from_file ".gdfignore")
      else (get_rules_from_file (repo^"/.gdf/info/exclude"))
    end
    else begin
      Unix.chdir ("../"^(Unix.getcwd ()));
      if Sys.file_exists ".gdfignore" then
        (aux ())@(get_rules_from_file ".gdfignore")
      else aux ()
    end
  in aux ()


let f_test () =
  (* fonction de test *)
  let sha = write_object (Blob("test", "test test test")) true in
  match read_object sha with
    | Blob(a,b) -> print_string a; print_newline (); print_string b
    | _ -> failwith "pas encore fait mais dune ne veut pas qu'il y ait des partial-matching"