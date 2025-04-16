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
    close_out config_channel;