(* open Arg
open Unix
open Filename
open Sys
open Sha1 *)

exception GdfError of string

type path = string list

type workbaliveau = path

type git_directory = path

type commettre = {
  baliveau : string;
  aieul : string list;
  auteur : string;
  commetteur : string;
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
  i_name : string
}

type indice = {
  entries : entries list;
  version : int
}  

type utilisateur = {
  u_email : string;
  u_name : string
  }

type configuration = {
  utilisateur : utilisateur
}

type ignoring_rules = Usual | Excl | Comment

type obj =
  | Blob of string * string
  | Commettre of commettre (* ah ouais en fait c'est pas très beau ce que j'ai fait là :'( *)
  | Baliveau of (string * string * string) list 

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
  Unix.mkdir (s^"/.gdf/objets") perm_base;
  Unix.mkdir (s^"/.gdf/refs") perm_base;
  Unix.mkdir (s^"/.gdf/refs/etiquettes") perm_base;
  Unix.mkdir (s^"/.gdf/refs/tetes") perm_base; (* pour mettre les branches *)
  Unix.mkdir (s^"/.gdf/info") perm_base;
  let tete_channel = Stdlib.open_out (s^"/.gdf/TETE") in
    write_str_stdlib tete_channel "ref: refs/tetes/maitre";
  let indice_channel = Stdlib.open_out (s^"/.gdf/indice") in
    write_str_stdlib indice_channel "DIRC0000000000000000";
  let exclus_channel = Stdlib.open_out (s^"/.gdf/info/exclus") in
    write_str_stdlib exclus_channel "";
  let config_channel = open_out (s^"/.gdf/config") in
    output_string config_channel "";
    close_out config_channel; close_out indice_channel; close_out tete_channel

  
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
        raise (GdfError "Pas un entrepôt git-de-France");
        aux ()))
      in aux ())

let compute_object_name name = 
  (* Donne le nom global de l'objet ie. depuis le repo principal *)
  let repo_name = repo_find () in
  let len_name = String.length name in
  let dir = String.sub name 0 2 in
  let obj_name = String.sub name 2 (len_name - 2) in
  (repo_name^"/.gdf/objets/"^dir^"/"^obj_name)

let extract_data obj =
  (* Renvoie le contenu d'un fichier qui n'est pas dans objets sous forme de string *)
  let f_channel = Stdlib.open_in obj in
  let str = read_str_until_eof_stdlib f_channel in
  str

let extract_data_zip obj =
  (* Renvoie le contenu d'un fichier qui n'est pas dans objets sous forme de string *)
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

let concat_list_commettre c =
  let concat_p_list = match c.aieul with
  | [] -> ["aieul "]
  | x::q -> 
  ["aieul "^x]@(List.map (fun e -> " "^e) q) in
  ["baliveau "^(c.baliveau)]@concat_p_list@["auteur "^(c.auteur);"commetteur "^(c.commetteur);
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

let make_config_readable l = 
  (*gives entries the format [[category-name];[entry1;entry2;...;entryn]], easy to parse and manipulate while   allowing the config file to be readable*)
  let trimmed_list = List.map String.trim l in
  let entries_list = List.map (String.split_on_char '\n') trimmed_list in
  List.map (List.map String.trim) entries_list


let parse_user l =
  let name = ref "" and email = ref "" in
  let treat entry = 
    let clean_entry = List.map String.trim (String.split_on_char '=' entry) in
    match clean_entry with
      | "nom" :: q -> name := String.concat "=" q
      | "courriel" :: q -> email := String.concat "=" q
      | _ -> ()
  in List.iter treat l;
  {u_name = !name; u_email = !email}

let config_parser () =
  let repo = repo_find () in
  let config_data = extract_data (repo ^ "/.gdf/config") in
  let pre_fields = String.split_on_char '[' config_data in 
  let fields = List.map (String.split_on_char ']') pre_fields in (*Useless right now - it's there for flexibility, it's very easy to add fields*)
  let config_list = List.map make_config_readable fields in
  let user = ref {u_name = ""; u_email = ""} in
  let parse x = match x with
    | ["utilisateur"] :: q :: [] -> user := parse_user q
    | _ -> ()
  in List.iter parse config_list;
  {utilisateur = !user}

let set_user name email =
  let repo = repo_find () in
  let config_data = extract_data (repo ^ "/.gdf/config") in
  let config_channel = Stdlib.open_out (repo ^ "/.gdf/config") in
  Stdlib.output_string config_channel config_data;
  Stdlib.output_string config_channel ("\n[utilisateur]\n\tnom = " ^ name ^ "\n\tcourriel = " ^ email);
  Stdlib.close_out config_channel

let commettre_parser obj_content =
  (* Parser pour les commettres, dsl c'est immonde *) (
  let data_list = String.split_on_char '\n' obj_content in
  let size = String.length obj_content in
  let baliveau = ref "" in
  let aieul = ref [] in
  let auteur = ref "" in
  let commetteur = ref "" in
  let gpgsig = ref "" in 
  let name = ref "" in
  let rec compute_data lst cur_section = (match lst with
    | [] -> ()
    | x::q -> let line_list = String.split_on_char ' ' x in
              if line_list = [""] then compute_data q cur_section
              else if List.hd line_list <> "" then begin
                (match line_list with 
                  | "baliveau"::s::[]                           ->
                    baliveau := s;
                    compute_data q "baliveau"
                  | "baliveau"::_                               ->
                    raise (GdfError "Mauvais format pour un baliveau")
                  | "aieul"::s::[]                         -> 
                    aieul := s::[];
                    compute_data q "aieul"
                  | "aieul"::_                             ->
                    raise (GdfError "Mauvais format pour un aïeul")
                  | "auteur"::a                             -> auteur := String.concat " " a;
                    compute_data q "auteur"
                  | "commetteur"::c                          -> commetteur := String.concat " " c;
                    compute_data q "commetteur"
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
                  | _ -> failwith ("ne devrait pas arriver si le reste du code est correct")) in
                let data_len = String.length data in
                if data_len >= 1 then
                (match cur_section with
                  | "aieul" -> aieul := data::(!aieul);
                                compute_data q "aieul"
                  | "gpgsig" -> gpgsig := (!gpgsig)^(" "^data);
                                compute_data q "gpgsig";
                  | "name"   -> name := String.concat "\n" (x::q)
                  | _        -> failwith "oula c'est pas normal")
              end)
  in compute_data data_list "baliveau";

  let commettre = {
    baliveau = !baliveau;
    aieul = !aieul;
    auteur = !auteur;
    commetteur = !commetteur;
    gpgsig = !gpgsig;
    name = !name;
    size = size
  } in commettre)

let parse_pregzip_commettre c =
  let obj_content = extract_data c in
  commettre_parser obj_content

let baliveau_parser data = 
  let rec aux x = match x with
    | [] -> []
    | mode :: sha :: path :: q -> (mode, sha, path) :: aux q
    | _ -> raise (GdfError "Mauvais format pour un baliveau")
  in Baliveau(aux data)

let deserialize str =
  let data = String.split_on_char ('\n') str in
    match data with
      | "blob" :: size :: file_name :: q ->
                  let file_data = String.concat "\n" q in 
                  (*l'ajout du \n est important pour la comparaison des tailles, sinon la taille
                  d'entrée ne correspond pas à la taille de l'objet reconcaténé *)
                  assert ((String.length file_data) = (int_of_string size));
                  Blob(file_name, file_data)
      | "commettre" :: _ :: q ->
                  (* let file_data = String.concat "\n" q in *)
                  (* TO DO : la taille ça marche pas *)
                  Commettre(commettre_parser (String.concat "\n" q))
      | "baliveau" :: _ :: q -> baliveau_parser q (*il peut pas y avoir de \n random dans un baliveau normalement donc la liste q c'est exactement ce qu'on veut*)
      | _ -> Printf.printf "data : %s\n" str; raise (GdfError "Mauvais format de données")

let serialize obj = (*le serialize du mr ne met pas le TÊTEer. raph dit que c'est cringe. a voir...*)
  match obj with
    | Blob(file_name, file_data) ->
        let size = string_of_int (String.length file_data) in
        String.concat "\n" ["blob"; size; file_name; file_data]
    | Commettre(c) ->
        let size = string_of_int c.size in
        let l_commettre = concat_list_commettre c in
        String.concat "\n" (["commettre"; size]@l_commettre)
    | Baliveau(l) -> 
      let contents_list = List.fold_left (fun acc (s,ss, sss) -> s :: ss :: sss :: acc) [] l in
      let baliveau_contents = String.concat "\n" contents_list in
      let size = string_of_int (String.length baliveau_contents) in
      String.concat "\n" ["baliveau"; size; baliveau_contents]
    (*| _ -> failwith "attends 2s connard"*)

let read_object sha =
  (* Fonction qui renvoie l'objet associé au haché sha *)
  let len_sha = String.length sha in
  let dir_sha = String.sub sha 0 2
  and obj_name = String.sub sha 2 (len_sha - 2) in
  let obj_path = (repo_find ())^"/.gdf/objets/" in
  let file_channel = Gzip.open_in (obj_path^dir_sha^"/"^obj_name) in
  let pre_obj = read_str_until_eof file_channel in
  Gzip.close_in file_channel;
  deserialize pre_obj

let find_type sha = 
  (* Prend un sha et renvoie le bon type *)
  let len_sha = String.length sha in
  let dir_sha = String.sub sha 0 2
  and obj_name = String.sub sha 2 (len_sha - 2) in
  let obj_path = (repo_find ())^"/.gdf/objets/" in
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
  with _ -> raise (GdfError "Le nom donné ne correspond pas à une référence")


let object_resolve name = match name with
(* Fonction qui renvoie les possibilités pour un nom donné, un etiquette ou un sha
   sous forme de liste de string *)
    | "" -> raise (GdfError "Le nom est vide et ne peut donc pas correspondre
    à un haché")
    | "TETE" -> [("tete",ref_resolve "TETE")]
    | _ -> (let possibilities = ref [] in
          let name_len = String.length name in
          if name_len >= 4 then begin
            let prefix = String.sub name 0 2 in
            let suffix = String.sub name 2 (name_len - 2) in
            let path = (repo_find ())^"/.gdf/objets/"^prefix in
            try (let files_here = Sys.readdir path in
            Array.iter (fun x -> if not (Sys.is_directory (path^"/"^x)) 
                                && (String.sub x 0 (name_len - 2) = suffix)
                                then possibilities := ("blob",prefix^x)::(!possibilities)) files_here)
            with _ -> ()
          end;(
          try let sha_etiquette = ref_resolve ("refs/etiquettes/"^name)
              in possibilities := ("etiquette",sha_etiquette)::(!possibilities) 
          with _ -> ();
          try let sha_etiquette = ref_resolve ("refs/tetes/"^name)
              in possibilities := ("tete",sha_etiquette)::(!possibilities) 
          with _ -> ());
          (* try let sha_etiquette = ref_resolve ("refs/remotes/"^name)
              in possibilities := sha_etiquette::(!possibilities) 
          with _ -> ();
          pour l'instant on ne le met pas mais ça peut servir dans la suite*)
          !possibilities)

let object_find name fmt =
  (* fonction qui renvoie le sha d'un objet dont le nom est name *)
  let list_resolve = object_resolve name in
  if fmt = "" then begin
    if List.length list_resolve = 0
    then raise (GdfError ("Le nom : "^name^" ne correspond à aucune occurence valide"))
    else if List.length list_resolve > 1
    then raise (GdfError ("Le nom : "^name^"correspond à plusieurs occurences")) 
    else let (_,y) = List.hd list_resolve in y end
  else begin
    let rec aux list_resolve fmt = match list_resolve, fmt with
    | [],_ -> raise (GdfError "Aucune occurence trouvée")
    | ("tete",x)::_,_ when fmt = "" -> x
    | (_,x)::_,_ when (find_type x = fmt) -> x
    | ("etiquette",x)::_,_ -> ref_resolve ("etiquettes/"^x)
    | (_,x)::_,"baliveau" when (find_type x = "commettre") -> (*c'est pas tres beau mais ça doit marcher*)
        let obj = read_object x in (match obj with
          | Commettre c -> c.baliveau
          | _ -> raise (GdfError "Mauvais type d'objet"))
    | ("tete",x)::_,_ -> x
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
        (if not (Sys.file_exists (repo_path^"/.gdf/objets/"^first_sha)) 
          then Unix.mkdir (repo_path^"/.gdf/objets/"^first_sha) perm_base); (*crée le repertoire ou on range l'objet s'il existe pas encore*)
        let file_channel = Gzip.open_out (repo_path^"/.gdf/objets/"^first_sha^"/"^last_sha) in
          write_str file_channel serialized_obj;
          Gzip.close_out file_channel));
  sha

let cat_file fmt sha = (*le pelo fait des trucs bizarres avec object find, a mediter. le type ne sert a rien, voir doc git*)
  let obj = read_object (object_find sha fmt) in 
    match obj with
      | Blob(_, str) -> Printf.printf "%s\n" str (*thibault utilise serialize. apres discussion avec raph, on en a (il en a) conclu que c'est débile*)
      | Commettre(c) -> Printf.printf "%s\n" (String.concat "\n" (concat_list_commettre c))
      | Baliveau(l) -> 
        let contents_list = List.fold_left (fun acc (s,ss, sss) -> s :: ss :: sss :: acc) [] l in
        let baliveau_contents = String.concat "\n" contents_list in (*nos baliveaus seront illisibles...*)
        Printf.printf "%s\n" baliveau_contents
      (*| _ -> failwith "dune t'es vraiment casse couille quand tu t'y mets"*)

let hash_file do_write typfile f_name =
  (* renvoie le sha d'un fichier *)
  let f_channel = Stdlib.open_in f_name in
  let data = read_str_until_eof_stdlib f_channel in
  close_in f_channel;
  match typfile with
    | "blob" -> write_object (Blob(f_name, data)) do_write
    | "commettre" -> write_object (Commettre(parse_pregzip_commettre f_name)) do_write
    | "baliveau" -> 
      let baliveau_contents = String.split_on_char '\n' data in
      write_object (baliveau_parser baliveau_contents) do_write
    | _ -> raise (GdfError (typfile ^ " n'est pas un type d'objet valide"))

let compute_log name = 
  (*alors la, il faut qu'on en parle. cette fonction donne l'arbre des commettres en format .dot direct dans la console. okok pas de soucis. sauf que 1) on donne pas tout le log juste l'historique des commettres passés en argument, 2) on peut passer qu'un seul commettre en argument, 3) ya pas de merge. ?????? c'est juste une ligne ton log??? fin je vois pas l'interet de se casser les couilles avec graphviz pour faire juste une liste dans l'ordre. au passage, l'abscence de merge rends plein de trucs obsolètes, style la possibilité d'avoir >1 aieuls. apres, si thibault polge demande moi j'execute. mais ça sert a rien. en vrai peut etre on peut donner la possibilté d'avoir une liste d'arguments plus tard? ça serait rigolo au moins un peu. ou alors peut etre je suis juste con et j'ai mal compris. au passage tu sais ce que c'est une mite à l'envers? c'est une co-mite (commettre). c'est pas grave si t'as pas compris je sais que mon humour est un peu trop subtil pour beaucoup de gens. bon allez je vais me log la gueule c'est tipar (parti en verlan)*)
  let sha = object_find name "commettre" in
  (* Printf.printf "name : %s sha : %s\n" name sha; *)
  (* Printf.printf "sha : %s\n" sha; *)
  Printf.printf "digraph wyaglog{\n\tnode[shape=rect]";
  let seen_commettres = Hashtbl.create 64 in (*hmm, c'est pas beau. ya surement un module mieux mais raph est pas la pour me dire que en fait c'est pas comme ça qu'on fait*)
  let rec log_graphviz comm = if not (Hashtbl.mem seen_commettres comm.name) then
      Hashtbl.add seen_commettres comm.name true;
      match comm.aieul with
        | [] -> ()
        | ""::[] -> ()
        | l -> List.iter (fun x -> 
            let truc = read_object x in match truc with
              | Commettre(c) -> 
                  Printf.printf "%s -> %s\n" c.name comm.name;
                  log_graphviz c
              | _ -> raise (GdfError "L'objet n'est pas un commettre")) l
  in match read_object sha with (*le prochain match que je dois ecrire ou ya un seul cas qui fonctionne je me defenestre*)
    | Commettre(coucou) -> log_graphviz coucou; Printf.printf "}\n"
    | _ -> raise (GdfError (sha ^ " n'est pas un commettre"))

let rec baliveau_checkout baliveau path = 
  let item (_, sha, file) =
    let obj = read_object sha in
    let dest = (path ^ "/" ^ file) in
    match obj with
      | Baliveau(_) ->
        Unix.mkdir dest perm_base;
        baliveau_checkout obj dest
      | Blob(_, file_data) -> 
        let channel = Stdlib.open_out dest in
        Stdlib.output_string channel file_data; Stdlib.close_out channel
      | _ -> raise (GdfError "Mauvais type de fichier pour un objet dans un baliveau")
  in match baliveau with
    | Baliveau(l) -> List.iter item l
    | _ -> raise (GdfError "Mauvais type d'objet - un baliveau était attendu")
    
    
    let print_refs () =
  Unix.chdir ((repo_find ())^"/.gdf/");
  let ref_list = ref [] in
  let path0 = "refs/" in
  let rec aux path =
    let f_list = Sys.readdir path in
    Array.iter (fun x -> if Sys.is_directory (path^x) then aux (path^x^"/")
                        else begin
                          ref_list := (extract_data (path^x),path^x)::(!ref_list) end) f_list
  in aux path0;
  List.iter (fun (sha, path) -> Printf.printf "%s %s\n" sha path) (!ref_list)

let print_etiquette () =
  Unix.chdir ((repo_find ())^"/.gdf/refs/etiquettes");
  let file_list = Sys.readdir "." in
  Array.iter (fun x -> Printf.printf "%s\n" x) file_list

  let compute_etiquette name obj =
  (* Fonction qui crée un etiquette dont le nom est <name> et qui
  est relié à l'objet <obj> *)
  let sha = object_find obj "" in
  Unix.chdir ((repo_find ())^"/.gdf/refs/etiquettes");
  if Sys.file_exists name then raise (GdfError ("L'étiquette "^name^" existe déjà"))
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
      | _ -> raise (GdfError "Mauvais format pour une entrée de l'indice")
      
let indice_parser () = 
  (* fonction qui parse le fichier indice dans .gdf
  plus précisémment, renvoie un type indice 
  
  ATTENTION : dans le fichier indice les bits de poids le plus fort sont le plus 
  à gauche, ie. (1000)_2 = (4)_10 *)

  Unix.chdir (repo_find ());
  let data_ind = extract_data ".gdf/indice" in (* on suppose que le fichier
  indice n'est pas zippé pour l'instant, il faudra changer cette ligne si on
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
    
let get_indice_files () =
  (* Fonction qui renvoie les fichiers qui ont été setiquetteed dans indice *)
  List.map (fun e -> e.i_name) ((indice_parser ()).entries)

let print_indice_files () =
  (* Fonction qui affiche les noms des fichiers dans indice *)
  List.iter (fun x -> Printf.printf "%s\n" x) (get_indice_files ())

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
      [get_rules_from_file (repo^"/.gdf/info/exclus");get_rules_from_file ".gdfignore"]
      else [get_rules_from_file (repo^"/.gdf/info/exclus")]
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
  let tete_data = extract_data (repo^("/.gdf/TETE")) in
  let begin_tete_data = String.sub tete_data 0 16 in
  let end_tete_data = String.sub tete_data 16 (String.length tete_data - 16) in
  match begin_tete_data with
    | "ref: refs/tetes/" -> end_tete_data
    | _ -> ""

let cmd_status_branch () =
  (* Donne le début du status *)
  let branch = are_we_on_branch () in
  match branch with
    | "" -> Printf.printf "TETE détaché à %s\n" (object_find "TETE" "")
    | _ -> Printf.printf "Sur la branche %s\n" branch

let is_subbaliveau path =
  try Sys.is_directory path
with _ -> false

let baliveau_to_dict ref =
  (* Transorme un baliveau en Hashtbl dont les clefs sont les noms de fichiers
   et les valeurs sont les hachés des fichiers *)
  let table = Hashtbl.create 16 in
  let rec aux ref =
    let baliveau_sha = object_find ref "baliveau" in
    (* Printf.printf "sha_t : %s\n" baliveau_sha; *)
    let baliveau = read_object baliveau_sha in
    let rec compute_baliveau t = match t with
      | [] -> ()
      | (_,sha,path)::q -> (*Printf.printf "path : %s sha : %s\n" path sha;*)
                              if is_subbaliveau path then aux sha
                              else Hashtbl.add table path sha; compute_baliveau q
    in match baliveau with
      | Baliveau(t) -> compute_baliveau t
      | _ -> failwith "le baliveau donné n'est pas vraiment un baliveau"
  in aux ref; table

let cmd_status_tete_indice () =
  Printf.printf "\nChangements à commettre:\n";
  let tete = baliveau_to_dict "TETE" in
  let indice = indice_parser () in
  List.iter (fun e -> try let sha_in_tete = Hashtbl.find tete (e.i_name) in
                          (* Printf.printf "name : %s\n" e.i_name;
                          Printf.printf "sha_in_tete : %s sha : %s\n" sha_in_tete e.i_sha; *)
                          if sha_in_tete <> e.i_sha 
                          then Printf.printf "\tmodifié:%s\n" e.i_name;
                          Hashtbl.remove tete e.i_name
                        with _ -> Printf.printf "\tajouté:\t%s\n" e.i_name) indice.entries;
  Hashtbl.iter (fun k _ -> Printf.printf "\tsupprimé: %s\n" k) tete

let cmd_status_indice_workbaliveau () =
  Printf.printf "\nChangments non mis en scène pour commettre:\n";

  let indice = indice_parser () in
  List.iter (fun e -> if not (Sys.file_exists e.i_name)
                      then Printf.printf "\tsupprimé: %s\n" e.i_name
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
                            Printf.printf "\tmodifié:%s\n" e.i_name
                        end
                      end
              ) indice.entries;
  Printf.printf "\nFichiers non pistés:\n";
  let all_files = Array.to_list (Sys.readdir (repo_find ())) in
  List.iter (fun x -> Printf.printf "\t%s\n" x) (give_list_check_ignore all_files)

let compute_status () =
  cmd_status_branch ();
  cmd_status_tete_indice ();
  cmd_status_indice_workbaliveau ()

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

let indice_write indice = 
  (*écrit l'indice. ça djoufara l'indice d'avant, fais gaffe ma gueule*)
  let indice_channel = Stdlib.open_out ".gdf/indice" in
    write_str_stdlib indice_channel "DIRC";
    write_str_stdlib indice_channel (int_to_bit_string indice.version);
    write_str_stdlib indice_channel (int_to_bit_string (List.length indice.entries));
    List.iter (write_entry indice_channel) indice.entries; (*poualala la curryfication ça me donne envie de manger indien*)
    Stdlib.close_out indice_channel (*je l'ai pas oublié cette fois ci quel boss*)

let compute_erase () =
  (* Fonction qui supprime tous les fichiers d'un dossier qui sont aussi dans l'index *)
  List.iter (fun x -> if Sys.file_exists x then Sys.remove x) (get_indice_files ())

let compute_checkout name dir is_empty =
  let obj = read_object (object_find name "commettre") in
  let baliveau = match obj with
    | Commettre(c) -> read_object c.baliveau
    | _ -> raise (GdfError (name ^ " n'est pas un commettre"))
  (*c'est pas tres beau trois if de suite mais ça permet de gérer les erreurs un peu mieux*)
  in (if (Sys.file_exists dir) then(
    if not (Sys.is_directory dir) then raise (GdfError ("Le fichier " ^ dir ^ " n'est pas un dossier"));
    if is_empty && (Sys.readdir dir <> [||]) then raise (GdfError ("Le dossier " ^ dir ^ " n'est pas vide"));
    if not is_empty then compute_erase ())
  else Unix.mkdir dir perm_base);
  baliveau_checkout baliveau (Unix.realpath dir)

let compute_rm path_list do_delete skip_missing =
  (* fonction qui supprime des fichiers *)
  let indice = indice_parser () in
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
      kept_entries := entry :: !kept_entries) indice.entries;
  if (Hashtbl.length abs_paths > 0) && (not skip_missing) then raise (GdfError "Impossible de supprimer des fichiers absents de l'indice");
  (if do_delete then
    List.iter Unix.unlink !remove);
  indice_write ({entries = !kept_entries; version = indice.version})

let compute_add paths delete skip_missing =
  (* ajoute les fichiers dans paths dans l'indice *)
  compute_rm paths delete skip_missing;
  let repo = repo_find () in
  let list_clean = ref [] in
  List.iter (fun path -> let abs_path = repo^"/"^path in
                         if not (Sys.file_exists abs_path)
                         then raise (GdfError ("Le fichier "^path^" n'existe pas
                         ou n'est pas dans le baliveau de travail"))
                         else list_clean := abs_path::(!list_clean)) paths;
  let cur_indice = indice_parser () in
  let entries = ref cur_indice.entries in
  List.iter (fun x ->
    let entry = create_entry x "blob" in
    entries := entry::(!entries)) !list_clean;
  let new_indice = {
    entries = !entries;
    version = cur_indice.version
  } in indice_write new_indice

let true_dirname file = 
  match (Filename.dirname file) with
    | "." -> ""
    | unautrestringnimportelequel -> unautrestringnimportelequel

let baliveau_from_indice indice =
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
  in List.iter add_to_contents indice.entries;

  let sorted_paths = List.rev (List.sort compare (List.of_seq (Hashtbl.to_seq_keys contents))) in (*quelle horreur. mais ça marche (normalement...)*)
  let sha = ref "" in

  (* Hashtbl.iter (fun a l -> Printf.printf "path : %s\n" a;
                          List.iter (fun x -> Printf.printf "\te_name : %s\n" x.i_name) l) contents; *)

  let create_baliveau path =
    let make_leaf entry =
      let leaf_mode = entry.i_perms in
      (string_of_int leaf_mode, entry.i_sha, Filename.basename entry.i_name) (*TODO : ecrire les perms en octal, pas en decimal comme je le fais la*)
    in let baliveau = Baliveau (List.map make_leaf (Hashtbl.find contents path)) in
    sha := write_object baliveau true;
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

  in List.iter create_baliveau sorted_paths;
  !sha

let commettre_create baliveau aieul auteur message =
  let new_commettre = {
    baliveau = baliveau;
    aieul = aieul;
    auteur = auteur;
    commetteur = auteur;
    gpgsig = ""; (* on pourrait peut-être en mettre une mais c'est pas le plus urgent *)
    name = message;
    size = 0 (* TO DO : changer ça *)
  } in
  write_object (Commettre(new_commettre)) true
  
let find_author () =
  let user = (config_parser ()).utilisateur in
  user.u_name ^ " " ^ user.u_email

let compute_commettre message =
  let repo = repo_find () in
  let indice = indice_parser () in
  let baliveau = baliveau_from_indice indice in
  (* Printf.printf "baliveau : %s\n" baliveau; *)
  let commettre = 
    try (let ref_tete = object_find "TETE" "" in
        commettre_create baliveau [ref_tete] (find_author ()) message)
    with _ -> (
      let commettre2 = commettre_create baliveau [] (find_author ()) message in
      let maitre_channel = Stdlib.open_out (repo^"/.gdf/refs/tetes/maitre") in
      write_str_stdlib maitre_channel commettre2;
      Stdlib.close_out maitre_channel;
      commettre2) in
  let branch = are_we_on_branch () in 
  match branch with
    | "" ->let branch_channel = Stdlib.open_out (repo^"/.gdf/TETE") in
            write_str_stdlib branch_channel "\n";
            Stdlib.close_out branch_channel
    | _ -> let branch_channel = Stdlib.open_out (repo^"/.gdf/refs/tetes/"^branch) in
            write_str_stdlib branch_channel (commettre^"\n");
            Stdlib.close_out branch_channel

let branche_create name =
  let get_TETE_sha = object_find "TETE" "" in
  let repo = repo_find () in
  let path = repo^"/.gdf/refs/tetes/"^name in
  if not (Sys.file_exists path) then begin
    let nouvelle_branche_channel = Stdlib.open_out path in
    write_str_stdlib nouvelle_branche_channel get_TETE_sha;
    Stdlib.close_out nouvelle_branche_channel
  end
  else raise (GdfError ("la branche "^name^" existe déjà"))

let find_branche_tete () =
  (* Fonction qui renvoie le nom de la branche sur laquelle la tête pointe *)
  let rec aux ref acc = 
    let path = (repo_find ())^("/.gdf/")^ref in
    try (let data = extract_data path in
    let first_bits = String.sub data 0 5 in
    let lasts_bits = String.sub data 5 (String.length data - 5) in
    match first_bits with
      | "ref: " -> aux lasts_bits (List.hd (List.rev (String.split_on_char '/' lasts_bits)))
      | _       -> acc)
    with _ -> raise (GdfError "Le nom donné ne correspond pas à une référence")
  in aux "TETE" ""

let print_branches () =
  let repo = ((repo_find ())^"/.gdf/refs/tetes") in
  let files = Sys.readdir repo in
  let ref_tete = find_branche_tete () in
  Array.iter (fun x -> if x = ref_tete then Printf.printf "\t* %s\n" x
                      else Printf.printf "\t  %s\n" x) files

let compute_checkout_branche nom_branche =
  let repo = repo_find () in
  let path = repo^"/.gdf/refs/tetes" in
  if not (Sys.file_exists (path^"/"^nom_branche)) then raise (GdfError ("la branche "^nom_branche^" n'existe pas"))
  else begin
    let path_tete = (repo^"/.gdf/TETE") in
    let tete_channel = Stdlib.open_out path_tete in
    write_str_stdlib tete_channel ("ref: refs/tetes/"^nom_branche);
    Stdlib.close_out tete_channel;
    compute_checkout nom_branche "." false
  end

let f_test () =
  (* fonction de test *)
  let repo = repo_find () in
  let chan = Stdlib.open_out (repo^"/fichier_test") in
  write_str_stdlib chan "79d5e527bc66b195b63f7267992e3dcffa3de016";
  Stdlib.close_out chan 