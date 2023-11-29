open Adbench_shared
open Owl.Dense.Ndarray.Generic

module FloatScalar : Adbench_shared.Shared_gmm_types.GMM_SCALAR
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

module OwlFloatTensor : Adbench_shared.Shared_gmm_types.GMM_TENSOR
  with type t = (float, Bigarray.float64_elt) Owl.Dense.Ndarray.Generic.t
  with type scalar = float
= struct
  type t = (float, Bigarray.float64_elt) Owl.Dense.Ndarray.Generic.t
  type scalar = float

  open Owl.Dense.Ndarray.Generic

  let tensor x = x
  let shape = shape
  let zeros = zeros Bigarray.Float64
  let create = create Bigarray.Float64
  let concatenate = concatenate
  let stack = stack
  let squeeze = squeeze
  let get_slice = get_slice
  let slice_left = slice_left
  let get = get
  let mv_inplace a x y =
    Owl_cblas.gemv ~trans:false ~incx:1 ~incy:1 ~alpha:1.0 ~beta:0.0
                   ~a:a ~x:x ~y:y
  let exp = exp
  let add = add
  let sub = sub
  let mul = mul
  let sum_reduce = sum_reduce
  let log_sum_exp = log_sum_exp
  let scalar_mul = scalar_mul
  let pow_scalar = pow_scalar
  let sub_scalar = sub_scalar
end

module GMMTest () : Shared_test_interface.TEST
  with type input =
    (float, (float, Bigarray.float64_elt) t)
      Shared_gmm_data.gmm_input
  with type output =
   (float, (float, Bigarray.float64_elt) t)
     Shared_gmm_data.gmm_output
= struct
  type input =
    (float, (float, Bigarray.float64_elt) t)
      Shared_gmm_data.gmm_input
  type output =
    (float, (float, Bigarray.float64_elt) t)
      Shared_gmm_data.gmm_output

  let input = ref None
  let objective = ref 0.0
  let gradient = ref (zeros Bigarray.Float64 [|0|])

  let prepare input' =
    input := Some input'
  let calculate_objective times =
    let module Objective =
      Shared_gmm_objective.Make (FloatScalar) (OwlFloatTensor)
    in
    match !input with
      | None -> ()
      | Some param ->
        for _ = 1 to times do
          objective := Objective.gmm_objective param
        done
  let calculate_jacobian times =
    let module Objective =
      Shared_gmm_objective.Make (FloatScalar) (OwlFloatTensor)
    in
    match !input with
      | None -> ()
      | Some param ->
        for _ = 1 to times do
          objective := Objective.gmm_objective param;
          objective := Objective.gmm_objective param;
          objective := Objective.gmm_objective param
        done
  let output _ =
    {
      Shared_gmm_data.objective = !objective;
      Shared_gmm_data.gradient = !gradient
    }
end