open Owl.Dense.Ndarray.Generic

let () =
  let module Loader = Runner_gmm_loader in
  let res = Loader.load_input "/adb/data/gmm/test.txt" false in
  print res.alphas;
  print res.means;
  print res.icfs;
  print res.x;
  print_endline (string_of_float res.wishart.gamma);
  print_endline (string_of_int res.wishart.m)