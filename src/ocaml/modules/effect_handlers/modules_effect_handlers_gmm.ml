open Adbench_shared
open Modules_effect_handlers_evaluate_tensor_torch
open Modules_effect_handlers_reverse_tensor

module FloatScalar : Shared_gmm_types.GMM_SCALAR
  with type t = float
= struct
  type t = float

  let float x = x
  let log = Stdlib.log
  let ( +. ) = Stdlib.( +. )
  let ( -. ) = Stdlib.( -. )
  let ( *. ) = Stdlib.( *. )
  let ( /. ) = Stdlib.( /. )
end

module EvaluateScalar : Shared_gmm_types.GMM_SCALAR
  with type t = Evaluate.scalar
= struct
  include Evaluate
  type t = Evaluate.scalar

  let float = c
end

module ReverseEvaluate = Reverse (Evaluate)

module ReverseScalar : Shared_gmm_types.GMM_SCALAR
  with type t = ReverseEvaluate.scalar
= struct
  include ReverseEvaluate
  type t = ReverseEvaluate.scalar

  let float = c
end

module EvaluateTensor : Shared_gmm_types.GMM_TENSOR
  with type t = Evaluate.tensor
  with type scalar = Evaluate.scalar
= struct
  include Evaluate
  type t = Evaluate.tensor
  type scalar = Evaluate.scalar

  let tensor x = x
  let add = ( + )
  let sub = ( - )
  let mul = ( * )
end

module ReverseTensor : Shared_gmm_types.GMM_TENSOR
  with type t = ReverseEvaluate.tensor
  with type scalar = ReverseEvaluate.scalar
= struct
  include ReverseEvaluate
  type t = ReverseEvaluate.tensor
  type scalar = ReverseEvaluate.scalar

  let tensor x =
    {
      v = x;
      dv = Evaluate.zeros (Array.of_list (Torch.Tensor.shape x))
    }
  let create ia f = create ia (Evaluate.c f)
  let add = ( + )
  let sub = ( - )
  let mul = ( * )
end

module T = Torch.Tensor

module GMMTest () : Shared_test_interface.TEST
  with type input =
    (float, T.t)
      Shared_gmm_data.gmm_input
  with type output =
   (float, T.t)
     Shared_gmm_data.gmm_output
= struct
  type input =
    (float, T.t)
      Shared_gmm_data.gmm_input
  type output =
    (float, T.t)
      Shared_gmm_data.gmm_output

  let input = ref None
  let objective = ref 0.0
  let gradient = ref (T.zeros [0])
  let _grads = ref (Array.init 3 (fun _ -> T.zeros [0]))

  let prepare input' =
    input := Some input'
  let calculate_objective times =
    let module Objective =
      Shared_gmm_objective.Make (EvaluateScalar) (EvaluateTensor)
    in
    match !input with
      | None -> ()
      | Some param ->
        for _ = 1 to times do
          objective :=
            Effect.Deep.match_with
              Objective.gmm_objective
              param
              Evaluate.evaluate
        done
  let calculate_jacobian times =
    let module Objective =
      Shared_gmm_objective.Make (ReverseScalar) (ReverseTensor)
    in
    match !input with
      | None -> ()
      | Some param ->
        for _ = 1 to times do
          let grads = Effect.Deep.match_with (fun p ->
            ReverseEvaluate.grad (fun ta ->
              let (alphas, means, icfs) = (ta.(0), ta.(1), ta.(2)) in
              Objective.gmm_objective
                { alphas = alphas;
                  means = means;
                  icfs = icfs;
                  x = ReverseTensor.tensor param.x;
                  wishart = {
                    gamma = ReverseScalar.float param.wishart.gamma;
                    m = param.wishart.m
                  }
                }
            ) p
          ) [|param.alphas; param.means; param.icfs|] Evaluate.evaluate in
          _grads := grads
        done
  let output _ =
    let flattened = Array.map T.flatten !_grads in
    gradient := T.concatenate ~dim:0 (Array.to_list flattened);
    {
      Shared_gmm_data.objective = !objective;
      Shared_gmm_data.gradient = !gradient
    }
end