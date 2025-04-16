(* open Arg
open Unix
open Filename
open Sys
open Sha1 *)

exception GdfError of string

type path = string list

type worktree = path

type git_directory = path


let perm_base = 0o777

let check_init s =
  (* Fonction qui regarde si les conditions sont remplies pour
  initialiser un projet git *)
  print_string s


let compute_init s =
  (* Fonction qui est appelée lorsque la commande init est saisie *)
  (try
    Unix.mkdir s perm_base;
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

let () = (
let () = Unix.chdir ".git/objects" in
let a = repo_find () in print_string a)