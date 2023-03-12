open Effect.Deep
open Effect
open Modules_effect_handlers_smooth
open Array

type name = {get_value : int}

module type FRESH = sig
  type _ Effect.t += Fresh : unit -> name Effect.t

  val fresh : unit -> name
end

module Fresh : FRESH = struct
  type _ Effect.t += Fresh : unit -> name Effect.t

  let fresh () = perform (Fresh ())
end

type 't prop = {v : 't; dv : name option}

type 't pointer
  = Single of name * 't
  | Double of name * name * 't * 't

module Reverse_tape (T : SMOOTH) (F : FRESH) = struct
  include Smooth (struct type t = T.t prop end)
  include Fresh

  let increment_name init =
    let i = ref init in {
      retc = (fun x -> (!i, x));
      exnc = raise;
      effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Fresh () -> Some (fun (k : (a, _) continuation) ->
            let t = !i in
            i := !i + 1;
            continue k {get_value = t}
          )
        | _ -> None
      )
  }
  let reverse () = 
    let tape : T.t pointer list ref = ref [] in {
      retc = (fun x -> (!tape, x));
      exnc = raise;
      effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Ap0 n -> Some (fun (k : (a, _) continuation) ->
            let open T in
            continue k {v = op0 n; dv = None}
          )
        | Ap1 (u, x) -> Some (fun k ->
            let open T in
            let open F in
            let res = op1 u x.v in
            match x.dv with
            | None -> continue k {v = res; dv = None}
            | Some nx ->
              tape := Single (nx, der1 u x.v) :: (!tape);
              continue k {v = res; dv = Some (fresh ())}
          )
        | Ap2 (b, x, y) -> Some (fun k ->
            let open T in
            let open F in
            let res = op2 b x.v y.v in
            match (x.dv, y.dv) with
            | (None, None) -> continue k {v = res; dv = None}
            | (Some nx, None) ->
              tape := Single (nx, der2 b L x.v y.v) :: (!tape);
              continue k {v = res; dv = Some (fresh ())}
            | (None, Some ny) ->
              tape := Single (ny, der2 b R x.v y.v) :: (!tape);
              continue k {v = res; dv = Some (fresh ())}
            | (Some nx, Some ny) ->
              tape :=
                Double (nx, ny, der2 b L x.v y.v, der2 b R x.v y.v) :: (!tape);
              continue k {v = res; dv = Some (fresh ())}
          )
        | _ -> None
      )
  }

  let d f x =
    let (m, (tape, _)) =
      match_with (fun () ->
        match_with f {v = x; dv = Some (F.fresh ())} (reverse ())
      ) () (increment_name 0)
    in
    let state = init m (fun _ -> T.c 0.0) in
    state.(m - 1) <- T.c 1.0;
    List.iteri (fun k p ->
      let open T in
      match p with
      | Single (nu, vu) ->
        let dk = state.(m - (k + 1)) in
        let du = state.(nu.get_value) in
        state.(nu.get_value) <- (du +. (vu *. dk))
      | Double (nl, nr, vl, vr) ->
        let dk = state.(m - (k + 1)) in
        let dl = state.(nl.get_value) in
        state.(nl.get_value) <- (dl +. (vl *. dk));
        let dr = state.(nr.get_value) in
        state.(nr.get_value) <- (dr +. (vr *. dk))
    ) tape;
    state.(0)
end
