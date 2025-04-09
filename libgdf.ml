open Arg
open Cmdliner
open Unix
open Filename
open Sys
open Sha
open Camlzip

type path = string list

type worktree = path

type git_directory = path

