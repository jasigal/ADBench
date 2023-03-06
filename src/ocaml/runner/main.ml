open Owl.Dense.Ndarray.Generic
open Adbench_modules.Modules_effect_handlers_gmm

let () =
  let module Loader = Runner_gmm_loader in
  let res = Loader.load_input "/adb/data/gmm/test.txt" false in
  print_endline test;
  print res.alphas;
  print res.means;
  print res.icfs;
  print res.x;
  print_endline (string_of_float res.wishart.gamma);
  print_endline (string_of_int res.wishart.m)