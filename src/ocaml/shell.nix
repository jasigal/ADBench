let
  sources = import ./nix/sources.nix; # sources.nix was generated by niv
  pkgs = import sources.nixpkgs { };
in
pkgs.mkShell
{
  shellHooks = ''
    export LIBTORCH=/home/jesse/Repositories/libtorch
    export CAML_LD_LIBRARY_PATH=$CAML_LD_LIBRARY_PATH:${pkgs.ocamlPackages.camlzip.outPath}/lib/ocaml/5.1.1/site-lib/zip:$LIBTORCH/lib
  '';

  # lists all packages in development environment
  buildInputs = with pkgs; [
    bash
    zlib

    ocamlPackages.ocaml
    ocamlPackages.dune_3
    ocamlPackages.findlib
    ocamlPackages.utop
    ocamlPackages.odoc
    ocamlPackages.ocaml-lsp
    ocamlformat

    ocamlPackages.base
    ocamlPackages.core
    ocamlPackages.core_unix
    ocamlPackages.ppx_bench
    ocamlPackages.ppx_inline_test
    ocamlPackages.ppx_jane
    ocamlPackages.ppx_string
    ocamlPackages.stdio
    ocamlPackages.ctypes
    ocamlPackages.ctypes-foreign
    ocamlPackages.dune-configurator
    ocamlPackages.ocaml-compiler-libs
    ocamlPackages.pyml
    ocamlPackages.npy
    ocamlPackages.yaml
    ocamlPackages.cmdliner
    ocamlPackages.camlzip
    ocamlPackages.owl
  ];
}
