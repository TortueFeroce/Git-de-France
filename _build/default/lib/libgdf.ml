(* open Arg
open Unix
open Filename
open Sys
open Sha1 *)


type path = string list

type worktree = path

type git_directory = path


let perm_base = 0o777

let compute_init s =
  (* Fonction qui est appelée lorsque la commande init est saisie *)
  Unix.mkdir s perm_base;
  Unix.mkdir (s^"/.git") perm_base
