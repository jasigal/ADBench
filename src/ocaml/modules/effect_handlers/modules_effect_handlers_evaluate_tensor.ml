open Effect.Deep
open Modules_effect_handlers_smooth_tensor

module Evaluate_Non_Diff : SMOOTH_NON_DIFF
  with type scalar = float
  with type tensor = (float, Bigarray.float64_elt) Owl.Dense.Ndarray.Generic.t
= struct
  type scalar = float
  type tensor = (float, Bigarray.float64_elt) Owl.Dense.Ndarray.Generic.t
  
  let shape = Owl.Dense.Ndarray.Generic.shape
end

module T = Owl.Dense.Ndarray.Generic

module Evaluate = struct
  include Smooth (Evaluate_Non_Diff)

  let evaluate = {
    retc = (fun x -> x);
    exnc = raise;
    effc = (fun (type a) (eff : a Effect.t) ->
      match eff with
      | Ap_u_to_s o -> Some (fun (k : (a, _) continuation) ->
          match o with
            | Const f -> continue k f
        )
      | Ap_s_to_s (o, s) -> Some (fun k ->
          match o with
            | Negate -> continue k Stdlib.(-. s)
            | Log -> continue k Stdlib.(log s)
        )
      | Ap_s's_to_s (o, s1, s2) -> Some (fun k ->
          match o with
            | Add -> continue k Stdlib.(s1 +. s2)
            | Subtract -> continue k Stdlib.(s1 -. s2)
            | Multiply -> continue k Stdlib.(s1 *. s2)
            | Divide -> continue k Stdlib.(s1 /. s2)
        )
      | Ap_u_to_t o -> Some (fun k ->
          match o with
            | Zeros ia -> continue k T.(zeros Bigarray.Float64 ia)
            | Create (ia, f) -> continue k T.(create Bigarray.Float64 ia f)
        )
      | Ap_t_to_t (o, t) -> Some (fun k ->
          match o with
            | Squeeze iao -> continue k T.(squeeze ?axis:iao t)
            | Reshape ia -> continue k T.(reshape t ia)
            | GetSlice ill -> continue k T.(get_slice ill t)
            | SliceLeft ia -> continue k T.(slice_left t ia)
            | Transpose iao -> continue k T.(transpose ?axis:iao t)
            | Exp -> continue k T.(exp t)
            | Negate -> continue k T.(neg t)
            | PowerConst f -> continue k T.(pow_scalar t f)
            | SumReduce iao -> continue k T.(sum_reduce ?axis:iao t)
            | LogSumExp (io, bo) ->
              continue k T.(log_sum_exp ?axis:io ?keep_dims:bo t)
        )
      | Ap_t_in_t (o, tout, tin) -> Some (fun k ->
          match o with
            | SetSlice ill -> continue k T.(set_slice ill tout tin)
        )
      | Ap_t't_to_t (o, t1, t2) -> Some (fun k ->
          match o with
            | Add -> continue k T.(t1 + t2)
            | Subtract -> continue k T.(t1 - t2)
            | Multiply -> continue k T.(t1 * t2)
        )
      | Ap_t't_in_t (o, tin1, tin2, tout) -> Some (fun k ->
          match o with
            | MVInplace bo ->
                continue k Owl.Cblas.(
                  gemv ?trans:bo ~incx:1 ~incy:1 ~alpha:1.0 ~beta:0.0 
                       ~a:tin1 ~x:tin2 ~y:tout
                )
        )
      | Ap_t_to_s (o, t) -> Some (fun k ->
          match o with
            | Get ia -> continue k T.(get t ia)
            | Sum -> continue k T.(sum' t)
        )
      | Ap_s't_to_t (o, s, t) -> Some (fun k ->
          match o with
            | ScalarMultiply -> continue k T.(scalar_mul s t)
            | SubtractScalar -> continue k T.(sub_scalar t s)
        )
      | Ap_ta_to_t (o, ta) -> Some (fun k ->
          match o with
            | Concatenate io -> continue k T.(concatenate ?axis:io ta)
            | Stack io -> continue k T.(stack ?axis:io ta)
        )
      | Ap_t_to_ta (o, t) -> Some (fun k ->
          match o with
            | Split (io, ia) -> continue k T.(split ?axis:io ia t)
        )
      | _ -> None
    )
  }
end
