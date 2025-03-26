open Arg
open Cmdliner
open Unix
open Filename
open Sys
open Sha

type gitRepository = {
    worktree : string;
    gitdir : string;
    conf : string;
}

(* la fonction is_directory du module Sys renvoie un booléen pour savoir si le string passé en argument est un dir *)

let repo_path repo path =
    (* Concatène repo et path pour avoir le chemin complet vers path *)
    Filename.concat (repo.gitdir) path

let repo_path_list repo p_list =
    (* Concatène une liste *)
    repo_path repo (List.fold_left (fun ongoing_path p -> Filename.concat ongoing_path p) "" p_list)