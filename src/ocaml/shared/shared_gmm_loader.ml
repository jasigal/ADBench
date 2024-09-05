open Shared_gmm_data
open Torch.Tensor

let f64 = Torch_core.Kind.T Torch_core.Kind.f64

(* Load a file containing GMM inputs *)
let load_input file_name replicate_point =
  let ic = open_in file_name in

  let split s = String.split_on_char ' ' (String.trim s) in

  let header = split (input_line ic) in
  let d = int_of_string (List.nth header 0) in
  let k = int_of_string (List.nth header 1) in
  let n = int_of_string (List.nth header 2) in
  let icf_sz = Stdlib.(d * (d + 1) / 2) in

  let alphas = empty ~size:[k] ~options:(f64, Torch.Device.Cpu) in
  for ik = 0 to Stdlib.(-) k 1 do
    set_float1 alphas ik (float_of_string (input_line ic))
  done;

  let means = empty ~size:[d; k] ~options:(f64, Torch.Device.Cpu) in
  for ik = 0 to Stdlib.(-) k 1 do
    let line = List.map float_of_string (split (input_line ic)) in
    for id = 0 to Stdlib.(-) d 1 do
      set_float2 means id ik (List.nth line id)
    done;
  done;

  let icfs = empty ~size:[icf_sz; k] ~options:(f64, Torch.Device.Cpu) in
  for ik = 0 to Stdlib.(-) k 1 do
    let line = List.map float_of_string (split (input_line ic)) in
    for i = 0 to Stdlib.(-) icf_sz 1 do
      set_float2 icfs i ik (List.nth line i)
    done;
  done;

  let x = empty ~size:[d; n] ~options:(f64, Torch.Device.Cpu) in
  if replicate_point then
    let line = List.map float_of_string (split (input_line ic)) in
    for ix = 0 to Stdlib.(-) n 1 do
      for id = 0 to Stdlib.(-) d 1 do
        set_float2 x id ix (List.nth line id)
      done;
    done;
  else
    for ix = 0 to Stdlib.(-) n 1 do
      let line = List.map float_of_string (split (input_line ic)) in
      for id = 0 to Stdlib.(-) d 1 do
        set_float2 x id ix (List.nth line id)
      done;
    done;

  let wishart_line = split (input_line ic) in
  let wishart = {
    gamma = float_of_string (List.nth wishart_line 0);
    m = int_of_string (List.nth wishart_line 1);
  } in

  close_in ic;

  {
    alphas = transpose ~dim0:0 ~dim1:1 alphas;
    means = transpose ~dim0:0 ~dim1:1 means;
    icfs = transpose ~dim0:0 ~dim1:1 icfs;
    x = transpose ~dim0:0 ~dim1:1 x;
    wishart = wishart;
  }