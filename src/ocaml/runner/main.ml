open Owl.Dense.Ndarray.Generic
open Adbench_shared.Shared_gmm_objective

let () =
  let module Loader = Adbench_shared.Shared_gmm_loader in
  let res = Loader.load_input "/adb/data/gmm/1k/gmm_d10_K5.txt" false in

  let ans = gmm_objective res in
  print ans;

  (* -31302.540910910437 *)