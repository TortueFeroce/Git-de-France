open Libgdf
open Cmdliner

let empty_t = Term.(const ())
let cmd = Cmd.v (Cmd.info "git-de-france") empty_t

let () = exit (Cmd.eval cmd)