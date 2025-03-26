ocamlfind ocamlopt -linkpkg -package cmdliner -o revolt revolt.ml
open Cmdliner

let empty_t = Term.(const ())
let cmd = Cmd.v (Cmd.info "git-de-france") empty_t

let () = exit (Cmd.eval cmd)