open Adbench_runner.Runner_benchmark

let usage_msg = format_of_string (
  "usage: %s test_type module_path input_file_path output_dir minimum_measurable_time nruns_f nruns_j time_limit [-rep]\n"
)

(* ./run-all.ps1 -buildtype "Release" -minimum_measurable_time 0.5 -nruns_f 10 -nruns_J 10 -time_limit 180 -timeout 600 -tmpdir "/adb/temp" -tools @("OCaml") -gmm_d_vals_param @(2,5) *)

let () =
  if Array.length Sys.argv < 9 then begin
    Printf.eprintf usage_msg Sys.argv.(0);
    exit 1
  end;

  let test_type = String.uppercase_ascii Sys.argv.(1) in
  let module_path = Sys.argv.(2) in
  let input_file_path = Sys.argv.(3) in
  let output_prefix = Sys.argv.(4) in
  let minimum_measurable_time = float_of_string Sys.argv.(5) in
  let nruns_f = int_of_string Sys.argv.(6) in
  let nruns_j = int_of_string Sys.argv.(7) in
  let time_limit = float_of_string Sys.argv.(8) in
  let replicate_point =
    (Array.length Sys.argv > 9) && (String.equal Sys.argv.(9) "-rep")
  in

  let params = {
    module_path = module_path;
    input_file_path = input_file_path;
    output_prefix = output_prefix;
    minimum_measurable_time = minimum_measurable_time;
    nruns_f = nruns_f;
    nruns_j = nruns_j;
    time_limit = time_limit;
  } in

  assert (String.equal test_type "GMM");

  let module Benchmark = Make
    (Adbench_modules.Modules_effect_handlers_gmm.GMMTest ())
    (GMM_IO)
  in
  Benchmark.run params {replicate_point = replicate_point}