open Adbench_shared
open Owl.Dense.Ndarray.Generic

module GMMTest () : Shared_test_interface.TEST
  with type input = Shared_gmm_data.gmm_input
  with type output = Shared_gmm_data.gmm_output
= struct
  type input = Shared_gmm_data.gmm_input
  type output = Shared_gmm_data.gmm_output

  let input = ref None
  let objective = ref 0.0
  let gradient = ref (zeros Bigarray.Float64 [|0|])

  let prepare input' =
    input := Some input'
  let calculate_objective times =
    match !input with
      | None -> ()
      | Some param ->
        for _ = 1 to times do
          objective := Shared_gmm_objective.gmm_objective param
        done
  let calculate_jacobian times =
  match !input with
    | None -> ()
    | Some param ->
      for _ = 1 to times do
        objective := Shared_gmm_objective.gmm_objective param;
        objective := Shared_gmm_objective.gmm_objective param;
        objective := Shared_gmm_objective.gmm_objective param
      done
  let output _ =
    {
      Shared_gmm_data.objective = !objective;
      Shared_gmm_data.gradient = !gradient
    }
end