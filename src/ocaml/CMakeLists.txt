project("ocaml")

set(ENV{CFLAGS} "-I/usr/include -L/usr/lib/x86_64-linux-gnu")
execute_process(COMMAND "opam" "install" "--yes" "owl=1.1")

message("Starting dune build in ${PROJECT_SOURCE_DIR}")
execute_process(
  COMMAND "dune" "build" "--root=${PROJECT_SOURCE_DIR}"
  RESULT_VARIABLE ret
)
if(NOT (ret EQUAL "0"))
    message( FATAL_ERROR "Bad exit status: ${ret}")
endif()
message("Finished dune build")