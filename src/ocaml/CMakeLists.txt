project("ocaml")

execute_process(COMMAND "opam" "install" "--yes" "owl=1.0.2-1")
execute_process(COMMAND "dune" "build")