type shared_parameters = {
  module_path : string;
  input_file_path : string;
  output_prefix : string;
  minimum_measurable_time : float;
  nruns_f : int;
  nruns_j : int;
  time_limit : float;
}

let print_shared_parameters params =
  Printf.printf
"{
  module_path = %s;
  input_file_path = %s;
  output_prefix = %s;
  minimum_measurable_time = %f;
  nruns_f = %n;
  nruns_j = %n;
  time_limit = %f;
}\n"
params.module_path
params.input_file_path
params.output_prefix
params.minimum_measurable_time
params.nruns_f
params.nruns_j
params.time_limit;;

module type BENCHMARK = sig
  type input
  type output
  type parameters

  val run : shared_parameters -> parameters -> unit
end

module type DATA_IO = sig
  type input
  type output
  type parameters

  val save_output_to_file : output -> string -> string -> string -> unit
  val read_input_data : string -> parameters -> input
end

module Output_formatter = struct
  let precision = 12

  let objective_file_name output_prefix input_base_name module_base_name =
    output_prefix ^ input_base_name ^ "_F_" ^ module_base_name ^ ".txt"

  let jacobian_file_name output_prefix input_base_name module_base_name =
    output_prefix ^ input_base_name ^ "_J_" ^ module_base_name ^ ".txt"
  
  let save_time_to_file filepath objective_time jacobian_time =
    let oc = open_out filepath in
    Printf.fprintf oc "%.*g\n%.*g"
      precision objective_time precision jacobian_time;
    close_out oc

  let save_value_to_file filepath value =
    let oc = open_out filepath in
    Printf.fprintf oc "%.*g" precision value;
    close_out oc
  
  let save_vector_to_file filepath vector =
    let oc = open_out filepath in
    assert (Array.length (Owl.Dense.Ndarray.Generic.shape vector) == 1);
    Owl.Dense.Ndarray.Generic.iter
      (Printf.fprintf oc "%.*g\n" precision) vector;
    close_out oc
end

module GMM_IO : DATA_IO
  with type input = Adbench_shared.Shared_gmm_data.gmm_input
  with type output = Adbench_shared.Shared_gmm_data.gmm_output
  with type parameters = Adbench_shared.Shared_gmm_data.gmm_parameters
= struct
  type input = Adbench_shared.Shared_gmm_data.gmm_input
  type output = Adbench_shared.Shared_gmm_data.gmm_output
  type parameters = Adbench_shared.Shared_gmm_data.gmm_parameters

  open Adbench_shared.Shared_gmm_data
  open Adbench_shared.Shared_gmm_loader
  open Output_formatter
  let save_output_to_file
    output
    output_prefix
    input_base_name
    module_base_name
  =
    save_value_to_file
      (objective_file_name output_prefix input_base_name module_base_name)
      output.objective;
    save_vector_to_file
      (jacobian_file_name output_prefix input_base_name module_base_name)
      output.gradient

  let read_input_data filepath parameters =
    load_input filepath parameters.replicate_point
end

module Make
  (T : Adbench_shared.Shared_test_interface.TEST)
  (D : DATA_IO with type input = T.input with type output = T.output)
  : BENCHMARK
  with type input = T.input
  with type output = T.output
  with type parameters = D.parameters
= struct
  type input =  T.input
  type output =  T.output
  type parameters = D.parameters

  type repeats = Repeats of int | MeasurableTimeNotAchieved
  type find_repeat_result = {
    repeats : repeats;
    min_sample : float;
    total_time : float;
  }
  let max_possible_power_of_two = Int.shift_right Int.max_int 1 + 1

  (* All times are in fractional seconds, i.e. seconds as floats. *)
  let find_repeats_for_minimum_measurable_time minimum_measurable_time f =
    let total_time = ref 0.0 in
    let min_sample = ref Float.max_float in

    let repeats = ref 1 in

    (* Exit not thrown by any library function, we use it for a loop break. *)
    let _ =
      try
        while true do
          let t1 = Unix.gettimeofday () in
          f !repeats;
          let t2 = Unix.gettimeofday () in
          (* gettimeoftday is in fractional seconds as needed. *)
          let current_run_time = t2 -. t1 in
          assert (current_run_time >= 0.0);

          if current_run_time > minimum_measurable_time then begin
            let current_sample = current_run_time /. float_of_int !repeats in
            min_sample := Float.min !min_sample current_sample;
            total_time := !total_time +. current_run_time;
            raise Exit
          end;

          repeats := !repeats * 2;
          if !repeats > max_possible_power_of_two then begin
            repeats := -1;
            raise Exit
          end
        done
      with
      | Exit -> ();
    in
    let rep =
      if !repeats <= 0 then MeasurableTimeNotAchieved else Repeats !repeats
    in
    {repeats = rep; min_sample = !min_sample; total_time = !total_time}

  let measure_shortest_time minimum_measurable_time nruns time_limit f =
    let res =
      find_repeats_for_minimum_measurable_time minimum_measurable_time f
    in

    match res.repeats with
    | MeasurableTimeNotAchieved ->
      raise (Failure ( "It was not possible to reach the number of repeats" ^
                       "sufficient to achieve the minimum measurable time."
                     )
            )
    | Repeats reps ->
      let total_time = ref (res.total_time) in
      let min_sample = ref (res.min_sample) in
      let run = ref 1 in

      (* "run" begins from 1 because a first run already done by 
       * "find_repeats_for_minimum_measurable_time" function.
       *)
      while (!run < nruns) && (!total_time < time_limit) do
        run := !run + 1;
        let t1 = Unix.gettimeofday () in
        f reps;
        let t2 = Unix.gettimeofday () in
        (* gettimeoftday is in fractional seconds as needed. *)
        let current_run_time = t2 -. t1 in
        let current_sample = current_run_time /. float_of_int reps in
        min_sample := Float.min !min_sample current_sample;
        total_time := !total_time +. current_run_time;
      done;
      !min_sample;;

  let run sps ps =
    let inputs = D.read_input_data sps.input_file_path ps in
    
    T.prepare inputs;
    
    let objective_time =
      measure_shortest_time
        sps.minimum_measurable_time
        sps.nruns_f
        sps.time_limit
        T.calculate_objective
    in
    let jacobian_time =
      measure_shortest_time
        sps.minimum_measurable_time
        sps.nruns_j
        sps.time_limit
        T.calculate_jacobian
    in
    
    let output = T.output () in
    
    let input_base_name =
      Filename.remove_extension (Filename.basename sps.input_file_path)
    in
    let module_base_name =
      Filename.remove_extension (Filename.basename sps.module_path)
    in
    
    Output_formatter.save_time_to_file
      ( sps.output_prefix ^ input_base_name
                          ^ "_times_"
                          ^ module_base_name
                          ^ ".txt"
      )
      objective_time
      jacobian_time;
    D.save_output_to_file
      output
      sps.output_prefix
      input_base_name
      module_base_name
end