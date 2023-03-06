open Owl.Dense.Ndarray.Generic

(* Wishart priors *)
type wishart = {
  gamma : float;
  m : int;
}

(* GMM data as arrays *)
type gmm_input = {
  alphas : (float, Bigarray.float64_elt) t;
  means : (float, Bigarray.float64_elt) t;
  icfs : (float, Bigarray.float64_elt) t;
  x : (float, Bigarray.float64_elt) t;
  wishart : wishart;
}