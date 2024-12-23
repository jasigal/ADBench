open Effect.Deep
open Modules_effect_handlers_smooth_tensor

module T = Torch.Tensor
module S = Torch.Scalar

module Evaluate_Non_Diff : SMOOTH_NON_DIFF
  with type scalar = float
  with type tensor = Torch.Tensor.t
= struct
  type scalar = float
  type tensor = Torch.Tensor.t

  let shape t = Array.of_list (Torch.Tensor.size t)
  let add_ x dx = let _ = Torch.Tensor.add_ x dx in ()
end

module Evaluate = struct
  include Smooth (Evaluate_Non_Diff)

  let f64 = Torch_core.Kind.T Torch_core.Kind.f64

  let einsum_ijk_mik_to_mij a x = T.einsum ~equation:"ijk,mik->mij" ~path:None [a; x]

  let einsum_ijk_mij_to_mik a y = T.einsum ~equation:"ijk,mij->mik" ~path:None [a; y]

  let einsum_mij_mik_to_ijk y x = T.einsum ~equation:"mij,mik->ijk" ~path:None [y; x]

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
            | Zeros ia -> continue k (T.zeros (Array.to_list ia))
            | Create (ia, c) -> continue k (T.full ~size:(Array.to_list ia) ~fill_value:(S.f c) ~options:(f64, Torch.Device.Cpu))
        )
      | Ap_t_to_t (o, t) -> Some (fun k ->
          match o with
            | Squeeze None -> continue k (T.squeeze t)
            | Squeeze _ -> raise (Invalid_argument "invalid squeeze use")
            | Reshape ia -> continue k (T.reshape t ~shape:(Array.to_list ia))
            | GetSlice [[s; (-1)]] -> continue k (T.narrow_copy ~dim:0 ~start:s ~length:Stdlib.((List.nth (T.size t) 0) - s)  t)
            | GetSlice [[s; e]] -> continue k (T.narrow_copy ~dim:0 ~start:s ~length:Stdlib.(e - s + 1) t)
            | GetSlice [[]; [s; (-1)]] ->  continue k (T.narrow_copy ~dim:1 ~start:s ~length:Stdlib.((List.nth (T.size t) 1) - s)  t)
            | GetSlice [[]; [s; e]] -> continue k (T.narrow_copy ~dim:1 ~start:s ~length:Stdlib.(e - s + 1) t)
            | GetSlice _ -> raise (Invalid_argument "invalid get_slice use")
            | SliceLeft [|i|] -> continue k (T.narrow ~dim:0 ~start:i ~length:1 t)
            | SliceLeft _ -> raise (Invalid_argument "invalid slice_left use")
            | Transpose (Some [|i; j|]) -> continue k (T.transpose ~dim0:i ~dim1:j t)
            | Transpose _ -> raise (Invalid_argument "invalid transpose use")
            | Exp -> continue k (T.exp t)
            | Negate -> continue k (T.neg t)
            | PowerConst e -> continue k (T.pow_tensor_scalar t ~exponent:(S.f e))
            | SumReduce iao -> continue k (T.sum_dim_intlist t ~dim:(Option.map Array.to_list iao) ~dtype:f64 ~keepdim:true)
            | LogSumExp (io, bo) -> continue k (T.logsumexp ~dim:([Option.value ~default:0 io]) ~keepdim:(Option.value ~default:true bo) t)
            | Softmax (Some i) -> continue k (T.softmax t ~dim:i ~dtype:f64)
            | Softmax None -> continue k (T.softmax t ~dim:0 ~dtype:f64)
        )
      | Ap_t't_to_t (o, t1, t2) -> Some (fun k ->
          match o with
            | Add -> continue k (T.add t1 t2)
            | Subtract -> continue k (T.sub t1 t2)
            | Multiply -> continue k (T.mul t1 t2)
            | Divide -> continue k (T.div t1 t2)
            | Einsum_ijk_mik_to_mij -> continue k (einsum_ijk_mik_to_mij t1 t2)
            | Einsum_ijk_mij_to_mik -> continue k (einsum_ijk_mij_to_mik t1 t2)
            | Einsum_mij_mik_to_ijk -> continue k (einsum_mij_mik_to_ijk t1 t2)
            | SetSlice ill ->
              let tout = T.copy t1 in
              let part = match ill with
                | [[0]] -> tout
                | [[s; (-1)]] -> T.narrow ~dim:0 ~start:s ~length:Stdlib.((List.nth (T.size tout) 0) - s) tout
                | [[s; e]] -> T.narrow ~dim:0 ~start:s ~length:Stdlib.(e - s + 1) tout
                | [[]; [s; (-1)]] -> T.narrow ~dim:1 ~start:s ~length:Stdlib.((List.nth (T.size tout) 1) - s) tout
                | [[]; [s; e]] -> T.narrow ~dim:1 ~start:s ~length:Stdlib.(e - s + 1) tout
                | _ -> raise (Invalid_argument "invalid set_slice use")
              in
              T.copy_ part ~src:t2;
              continue k tout
        )
      | Ap_t_to_s (o, t) -> Some (fun k ->
          match o with
            | Get [|i|] -> continue k (T.get_float1 t i)
            | Get _ -> raise (Invalid_argument "invalid set_slice use")
            | Sum -> continue k (T.get_float1 (T.sum_to_size t ~size:[1]) 0)
        )
      | Ap_s't_to_t (o, s, t) -> Some (fun k ->
          match o with
            | ScalarMultiply -> continue k (T.mul_scalar t (S.f s))
            | SubtractScalar -> continue k (T.sub_scalar t (S.f s))
        )
      | Ap_ta_to_t (o, ta) -> Some (fun k ->
          match o with
            | Concatenate io -> continue k (T.concatenate (Array.to_list ta) ~dim:(Option.value ~default:0 io))
            | Stack io -> continue k (T.stack (Array.to_list ta)  ~dim:(Option.value ~default:0 io))
        )
      | Ap_t_to_ta (o, t) -> Some (fun k ->
          match o with
            | Split (io, ia) -> continue k (Array.of_list (T.split_sizes t ~split_size:(Array.to_list ia) ~dim:(Option.value ~default:0 io)))
        )
      | _ -> None
    )
  }
end
