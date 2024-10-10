open Effect.Deep
open Modules_effect_handlers_smooth_tensor

type 't prop = {v : 't; mutable dv : 't}

module Reverse_Non_Diff (T : SMOOTH_NON_DIFF) : SMOOTH_NON_DIFF
  with type scalar = T.scalar prop
  with type tensor = T.tensor prop
= struct
  type scalar = T.scalar prop
  type tensor = T.tensor prop

  let shape t = T.shape t.v
  let add_ x dx = T.add_ x.v dx.v; T.add_ x.dv dx.dv
  let sprint s =
    print_string "{v = ";
    T.sprint s.v;
    print_string "; dv =";
    T.sprint s.dv;
    print_string "}"
  let tprint t =
    print_string "{v = ";
    T.tprint t.v;
    print_string "; dv =";
    T.tprint t.dv;
    print_string "}"
  let all_zeros t = T.all_zeros t.v && T.all_zeros t.dv
end

module Reverse (T : SMOOTH) = struct
  include Smooth (Reverse_Non_Diff (T : SMOOTH_NON_DIFF))

  let test_zeros t =
    print_string "all_zeros?\n";
    if T.all_zeros t then print_string "YES\n" else print_string "NO\n"

  let reverse = {
    retc = (fun x -> x);
    exnc = raise;
    effc = (fun (type a) (eff : a Effect.t) ->
      match eff with
      | Ap_u_to_s o -> Some (fun (k : (a, _) continuation) -> let open T in
          continue k {v = op_u_to_s o; dv = c 0.0};
          print_u_to_s o; flush_all ();
        )
      | Ap_s_to_s (o, s) -> Some (fun k -> let open T in
          let r = {v = op_s_to_s o s.v; dv = c 0.0} in
          continue k r;
          print_string ">>>\n"; flush_all ();
          print_s_to_s o; flush_all ();
          s.dv <- s.dv +. (der_s_to_s o s.v r.dv);
          sprint s.dv; flush_all ();
          print_string "<<<\n"; flush_all ();
        )
      | Ap_s's_to_s (o, s1, s2) -> Some (fun k -> let open T in
          let r = {v = op_s's_to_s o s1.v s2.v; dv = c 0.0} in
          continue k r;
          print_string ">>>\n"; flush_all ();
          print_s's_to_s o; flush_all ();
          let (dv1, dv2) = der_s's_to_s o s1.v s2.v r.dv in
          s1.dv <- s1.dv +. dv1;
          s2.dv <- s2.dv +. dv2;
          sprint s1.dv; flush_all ();
          sprint s2.dv; flush_all ();
          print_string "<<<\n"; flush_all ();
        )
      | Ap_u_to_t o -> Some (fun k -> let open T in
          let v = op_u_to_t o in
          continue k {v = v; dv = create (shape v) 0.0};
          print_u_to_t o; flush_all ()
        )
      | Ap_t_to_t (o, t) -> Some (fun k -> let open T in
          let v = op_t_to_t o t.v in
          let r = {v = v; dv = create (shape v) 0.0} in
          continue k r;
          print_string ">>>\n"; flush_all ();
          print_t_to_t o; flush_all ();
          (* match o with
            | GetSlice ill ->
              print_string "INDICES\n";
              print_string "[";
              let _ = List.map
                (fun il ->
                  print_string "[";
                  let _ = List.map (Printf.printf "%d; ") il in
                  print_string "]";
                ) ill
              in
              print_string "]\n"; flush_all ();
              print_string "> r.dv\n"; flush_all ();
              tprint r.dv;
              flush_all ();
            | _ -> (); *)
          print_string "> t.v\n"; flush_all ();
          tprint r.dv;
          print_string "> r.dv\n"; flush_all ();
          tprint r.dv;
          let dv = der_t_to_t o t.v r.dv in
          print_string "> dv\n"; flush_all ();
          tprint dv;
          if shape t.dv = shape dv then add_ t.dv dv else t.dv <- t.dv + dv;
          print_string "> t.dv\n"; flush_all ();
          tprint t.dv;
          flush_all ();
          print_string "<<<\n"; flush_all ();
        )
      | Ap_t't_to_t (o, t1, t2) -> Some (fun k -> let open T in
          let v = op_t't_to_t o t1.v t2.v in
          let r = {v = v; dv = create (shape v) 0.0} in
          continue k r;
          print_string ">>>\n"; flush_all ();
          print_t't_to_t o; flush_all ();
          let (dv1, dv2) = der_t't_to_t o t1.v t2.v r.dv in
          if shape t1.dv = shape dv1
            then add_ t1.dv dv1 else t1.dv <- t1.dv + dv1;
          if shape t2.dv = shape dv2
            then add_ t2.dv dv2 else t2.dv <- t2.dv + dv2;
          test_zeros t1.dv; flush_all ();
          test_zeros t2.dv; flush_all ();
          print_string "<<<\n"; flush_all ();
        )
      | Ap_t_to_s (o, t) -> Some (fun k -> let open T in
          let r = {v = op_t_to_s o t.v; dv = c 0.0} in
          continue k r;
          print_string ">>>\n"; flush_all ();
          print_t_to_s o; flush_all ();
          let dv = der_t_to_s o t.v r.dv in
          if shape t.dv = shape dv then add_ t.dv dv else t.dv <- t.dv + dv;
          test_zeros t.dv; flush_all ();
          print_string "<<<\n"; flush_all ();
        )
      | Ap_s't_to_t (o, s, t) -> Some (fun k -> let open T in
          let v = op_s't_to_t o s.v t.v in
          let r = {v = v; dv = create (shape v) 0.0} in
          continue k r;
          (* print_string ">>>\n"; flush_all ();
          print_s't_to_t o; flush_all ();
          print_string "\ns\n"; flush_all ();
          sprint s.v;
          print_string "\nt\n"; flush_all ();
          tprint t.v;
          print_string "\ndr\n"; flush_all ();
          tprint r.dv; *)
          let (ds, dt) = der_s't_to_t o s.v t.v r.dv in
          (* print_string "\ndt\n"; flush_all ();
          tprint dt; *)
          s.dv <- s.dv +. ds;
          if shape t.dv = shape dt then add_ t.dv dt else t.dv <- t.dv + dt;
          (* sprint s.dv; flush_all ();
          test_zeros t.dv; flush_all ();
          print_string "<<<\n"; flush_all (); *)
        )
      | Ap_ta_to_t (o, ta) -> Some (fun k -> let open T in
          let tva = Array.(map (fun t -> t.v) ta) in
          let v = op_ta_to_t o tva in
          let r = {v = v; dv = create (shape v) 0.0} in
          continue k r;
          print_string ">>>\n"; flush_all ();
          print_ta_to_t o; flush_all ();
          let rdva = der_ta_to_t o tva r.dv in
          ignore Array.(map2 (fun t rdv -> (
            if shape t.dv = shape rdv then add_ t.dv rdv else t.dv <- t.dv + rdv;
            test_zeros t.dv; flush_all ()
          )) ta rdva);
          print_string "<<<\n"; flush_all ();
        )
      | Ap_t_to_ta (o, t) -> Some (fun k -> let open T in
          let va = op_t_to_ta o t.v in
          let ra =
            Array.(map (fun v -> {v = v; dv = create (shape v) 0.0}) va)
          in
          continue k ra;
          print_string ">>>\n"; flush_all ();
          print_t_to_ta o; flush_all ();
          let rdva = Array.(map (fun r -> r.dv) ra) in
          let dv = der_t_to_ta o t.v rdva in
          if shape t.dv = shape dv then add_ t.dv dv else t.dv <- t.dv + dv;
          test_zeros t.dv; flush_all ();
          print_string "<<<\n"; flush_all ();
        )
      | _ -> None
    )
  }

  let grad f ta =
    let ra = Array.map (fun t -> {v = t; dv = T.(create (shape t) 0.0)}) ta in
    match_with (fun ta -> (f ta).dv <- T.c 1.0) ra reverse;
    Array.map (fun r -> r.dv) ra
end