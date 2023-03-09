open Effect.Deep
open Float
open Shared_smooth

module Evaluate = struct
  include Smooth (struct type t = float end)

  let evaluate = {
    retc = (fun x -> x);
    exnc = raise;
    effc = (fun (type a) (eff : a Effect.t) ->
      match eff with
      | Ap0 n -> Some (fun (k : (a, _) continuation) ->
          match n with
          | Const x -> continue k x
        )
      | Ap1 (u, x) -> Some (fun k ->
          match u with
          | Negate -> continue k (neg x)
          | Sin -> continue k (sin x)
          | Cos -> continue k (cos x)
          | Exp -> continue k (exp x)
        )
      | Ap2 (b, x, y) -> Some (fun k ->
          match b with
          | Plus -> continue k (add x y)
          | Subtract -> continue k (sub x y)
          | Times -> continue k (mul x y)
          | Divide -> continue k (div x y)
        )
      | _ -> None
    )
  }
end
