open Owl.Dense.Ndarray.Generic
open Shared_gmm_data

let constructl d icfs =
  let lparamidx = ref d in

  let make_l_col i =
    let nelems = Stdlib.(d - i - 1) in
    (* Slicing in Owl requires inculsive indices, so will not create
     * an empty tensor. Thus we have two cases. 
     *)
    let max_lparamidx = (shape icfs).(0) in
    let col =
      if Stdlib.(!lparamidx >= max_lparamidx) then
        zeros Bigarray.Float64 [|Stdlib.(i + 1)|]
      else concatenate ~axis:0 [|
        zeros Bigarray.Float64 [|Stdlib.(i + 1)|];
        get_slice [[!lparamidx; Stdlib.(!lparamidx + nelems - 1)]] icfs;
    |] in
    lparamidx := Stdlib.(!lparamidx + nelems);
    col
  in

  let columns = Array.init d make_l_col in
  stack ~axis:1 columns

let qtimesx qdiag l x =
  let y = zeros Bigarray.Float64 (shape x) in
  for i = 0 to Stdlib.((shape x).(0) - 1) do
    for j = 0 to Stdlib.((shape x).(1) - 1) do
      (* Slice left are views, i.e. memory is shared. *)
      let sl = (squeeze (slice_left l [|j|])) in
      let sx = (squeeze (slice_left x [|i; j|])) in
      let sy = (squeeze (slice_left y [|i; j|])) in
      (* The result is stored into sy, and so y. *)
      Owl_cblas.gemv ~trans:false ~incx:1 ~incy:1 ~alpha:1.0 ~beta:0.0
      ~a:sl ~x:sx ~y:sy
    done;
  done;
  (qdiag * x) + y

let log_sum_exp_vec t =
  let mx = squeeze (max ~axis:1 t) in
  let lset = log_sum_exp ~axis:0 (transpose t - mx) in
  transpose (lset + mx)

let log_gamma_distrib a p =
  let scalar = Stdlib.(0.25 *. float_of_int (p * (p -1)) *. log Float.pi) in
  let summed = Array.fold_left (+.) 0.0
    (Array.init p (fun i ->
      Owl.Maths.loggamma (a +. 0.5 *. float_of_int Stdlib.(1 - (i + 1)))
    ))
  in
  scalar +. summed

let log_wishart_prior p wishart sum_qs qdiags icf =
  let n = float_of_int (Stdlib.(p + wishart.m + 1)) in
  let k = float_of_int ((shape icf).(0)) in
  
  let out = sum_reduce (
    (
      scalar_mul Stdlib.(0.5 *. wishart.gamma *. wishart.gamma)
        (squeeze (
          sum_reduce ~axis:[|1|] (pow_scalar qdiags 2.0) +
          sum_reduce ~axis:[|1|] (pow_scalar (get_slice [[]; [p;-1]] icf) 2.0)
        ))
    )
    - (scalar_mul (float_of_int wishart.m) sum_qs)
  ) in

  let c = n *. (float_of_int p) *. Stdlib.(log (wishart.gamma /. sqrt 2.0)) in
  sub_scalar out (k *. (c -. log_gamma_distrib (0.5 *. n) p))

let gmm_objective param =
  let xshape = shape param.x in
  let n = xshape.(0) in
  let d = xshape.(1) in  

  let qdiags = exp (get_slice [[]; [0; Stdlib.(d - 1)]] param.icfs) in
  let sum_qs = squeeze (sum_reduce ~axis:[|1|] (get_slice [[]; [0; Stdlib.(-) d 1]] param.icfs)) in

  let icf_sz = (shape param.icfs).(0) in
  let ls = stack (Array.init icf_sz (fun i ->
    constructl d (squeeze (get_slice [[i]] param.icfs)))
  ) in

  let xcentered = squeeze (stack (Array.init n (fun i ->
    (get_slice [[i]] param.x) - param.means
  ))) in
  let lxcentered = qtimesx qdiags ls xcentered in
  let sqsum_lxcentered = squeeze (sum_reduce ~axis:[|2|] (pow_scalar lxcentered 2.0)) in
  let inner_term = param.alphas + sum_qs - (scalar_mul 0.5 sqsum_lxcentered) in
  (* Uses the stable version as in the paper, i.e. max-shifted *)
  let lse = squeeze (log_sum_exp_vec inner_term) in
  let slse = sum_reduce lse in

  let const = create Bigarray.Float64 [||] Stdlib.(
    -. (float_of_int n) *. (float_of_int d) *. 0.5 *. log (2.0 *. Float.pi)
  ) in
  
  let wish = log_wishart_prior d param.wishart sum_qs qdiags param.icfs in
  const + slse
        - scalar_mul (float_of_int n) (squeeze (log_sum_exp param.alphas))
        + wish