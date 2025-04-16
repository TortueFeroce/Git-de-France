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

    (Unix.mkdir s perm_base;
    Unix.mkdir (s^"/.git") perm_base;
    Unix.mkdir (s^"/.git/objects") perm_base;
    Unix.mkdir (s^"/.git/refs") perm_base;
    let _ = Unix.openfile (s^"/.git/config") [O_CREAT] perm_base in ())
  (* with Unix.Unix_error(_) ->
    Unix.mkdir (s^"/.git") perm_base;
    Unix.mkdir (s^"/.git/objects") perm_base;
    Unix.mkdir (s^"/.git/refs") perm_base *)