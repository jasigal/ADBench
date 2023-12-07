open Effect

type u_to_s = Const of float
type s_to_s = Negate | Log
type s's_to_s = Add | Subtract | Multiply | Divide

type u_to_t = Zeros of int array | Create of int array * float
type t_to_t
  = Squeeze of int array option
  | Reshape of int array
  | GetSlice of int list list
  | SliceLeft of int array
  | Transpose of int array option
  | Exp
  | Negate
  | PowerConst of float
  | SumReduce of int array option
  | LogSumExp of int option * bool option
type t't_to_t
  = Add
  | Subtract
  | Multiply
  | Divide
  | MV of bool option
  | SetSlice of int list list

type t_to_s = Get of int array | Sum
type s't_to_t = ScalarMultiply | SubtractScalar
type ta_to_t = Concatenate of int option | Stack of int option
type t_to_ta = Split of int option * int array

type arg = L | R

module type SMOOTH = sig
  type scalar
  type tensor
  type _ Effect.t +=
      Ap_u_to_s : u_to_s -> scalar Effect.t
    | Ap_s_to_s : s_to_s * scalar -> scalar Effect.t
    | Ap_s's_to_s : s's_to_s * scalar * scalar -> scalar Effect.t
    | Ap_u_to_t : u_to_t -> tensor Effect.t
    | Ap_t_to_t : t_to_t * tensor -> tensor Effect.t
    | Ap_t't_to_t : t't_to_t * tensor * tensor -> tensor Effect.t
    | Ap_t_to_s : t_to_s * tensor -> scalar Effect.t
    | Ap_s't_to_t : s't_to_t * scalar * tensor -> tensor Effect.t
    | Ap_ta_to_t : ta_to_t * tensor array -> tensor Effect.t
    | Ap_t_to_ta : t_to_ta * tensor -> tensor array Effect.t

  val c : float -> scalar
  val ( ~. ) : scalar -> scalar
  val log : scalar -> scalar
  val ( +. ) : scalar -> scalar -> scalar
  val ( -. ) : scalar -> scalar -> scalar
  val ( *. ) : scalar -> scalar -> scalar
  val ( /. ) : scalar -> scalar -> scalar

  (* Non-differentiable operations *)
  val shape : tensor -> int array

  (* Creating constant tensors *)
  val zeros : int array -> tensor
  val create : int array -> float -> tensor

  (* Combining tensors *)
  val concatenate : ?axis:int -> tensor array -> tensor
  val stack : ?axis:int -> tensor array -> tensor

  (* Splitting tensors *)
  val split : ?axis:int -> int array -> tensor -> tensor array

  (* Changing tensor shape *)
  val transpose : ?axis:int array -> tensor -> tensor
  val reshape : tensor -> int array -> tensor

  (* Shrinking and slicing tensors *)
  val squeeze : ?axis:int array -> tensor -> tensor
  val get_slice : int list list -> tensor -> tensor
  val slice_left : tensor -> int array -> tensor
  val get : tensor -> int array -> scalar
  val set_slice : int list list -> tensor -> tensor -> tensor

  (* Matrix-vector multiplication *)
  val mv : ?trans:bool -> tensor -> tensor -> tensor

  (* Pointwise tensor operations *)
  val exp : tensor -> tensor
  val pow_const : tensor -> float -> tensor
  val ( ~- ) : tensor -> tensor
  val ( + ) : tensor -> tensor -> tensor
  val ( - ) : tensor -> tensor -> tensor
  val ( * ) : tensor -> tensor -> tensor
  val ( / ) : tensor -> tensor -> tensor
  
  (* Reduction operations *)
  val sum : tensor -> scalar
  val sum_reduce : ?axis:int array -> tensor -> tensor
  val log_sum_exp : ?axis:int -> ?keep_dims:bool -> tensor -> tensor

  (* Scalar-tensor operations *)
  val scalar_mul : scalar -> tensor -> tensor
  val sub_scalar : tensor -> scalar -> tensor

  val op_u_to_s: u_to_s -> scalar
  val op_s_to_s: s_to_s -> scalar -> scalar
  val op_s's_to_s : s's_to_s -> scalar -> scalar -> scalar

  val op_u_to_t : u_to_t -> tensor
  val op_t_to_t : t_to_t -> tensor -> tensor
  val op_t't_to_t : t't_to_t -> tensor -> tensor -> tensor

  val op_t_to_s : t_to_s -> tensor -> scalar
  val op_s't_to_t : s't_to_t -> scalar -> tensor -> tensor
  val op_ta_to_t : ta_to_t -> tensor array -> tensor
  val op_t_to_ta : t_to_ta -> tensor -> tensor array

  val der_s_to_s : s_to_s -> scalar -> (scalar -> scalar)
  val der_s's_to_s : s's_to_s -> scalar -> scalar -> (scalar -> scalar * scalar)

  val der_t_to_t : t_to_t -> tensor -> (tensor -> tensor)
  val der_t't_to_t : t't_to_t -> tensor -> tensor -> (tensor -> tensor * tensor)
  
  val der_t_to_s : t_to_s -> tensor -> (scalar -> tensor)
  val der_s't_to_t : s't_to_t -> scalar -> tensor -> (tensor -> scalar * tensor)
  val der_ta_to_t : ta_to_t -> tensor array -> (tensor -> tensor array)
  val der_t_to_ta : t_to_ta -> tensor -> (tensor array -> tensor)
end

let string_of_t_to_t (o : t_to_t) = match o with
  | Squeeze _iao -> "squeeze"
  | Reshape _d -> "reshape"
  | GetSlice _ill -> "get_slice"
  | SliceLeft _ia -> "slice_left"
  | Transpose _iao -> "transpose"
  | Exp -> "exp"
  | Negate -> "negate"
  | PowerConst _f -> "pow_const"
  | SumReduce _iao -> "sum_reduce"
  | LogSumExp (_io, _bo) -> "log_sum_exp"

let print_ill ill =
  print_string "[";
  List.iter (fun il ->
    print_string "[";
    List.iter (fun i ->
      print_int i;
      print_string ", "
    ) il;
    print_string "], "
  ) ill;
  print_string "]"

module type SMOOTH_NON_DIFF = sig
  type scalar
  type tensor
  
  val shape : tensor -> int array
end

module Smooth (T : SMOOTH_NON_DIFF) : SMOOTH
  with type scalar = T.scalar
  with type tensor = T.tensor
= struct
  include T
  
  type scalar = T.scalar
  type tensor = T.tensor
  type _ Effect.t +=
      Ap_u_to_s : u_to_s -> scalar Effect.t
    | Ap_s_to_s : s_to_s * scalar -> scalar Effect.t
    | Ap_s's_to_s : s's_to_s * scalar * scalar -> scalar Effect.t
    | Ap_u_to_t : u_to_t -> tensor Effect.t
    | Ap_t_to_t : t_to_t * tensor -> tensor Effect.t
    | Ap_t't_to_t : t't_to_t * tensor * tensor -> tensor Effect.t
    | Ap_t_to_s : t_to_s * tensor -> scalar Effect.t
    | Ap_s't_to_t : s't_to_t * scalar * tensor -> tensor Effect.t
    | Ap_ta_to_t : ta_to_t * tensor array -> tensor Effect.t
    | Ap_t_to_ta : t_to_ta * tensor -> tensor array Effect.t

  let c s = perform (Ap_u_to_s (Const s))
  let log s = perform (Ap_s_to_s (Log, s))
  let ( ~. ) s = perform (Ap_s_to_s (Negate, s))
  let ( +. ) s1 s2 = perform (Ap_s's_to_s (Add, s1, s2))
  let ( -. ) s1 s2 = perform (Ap_s's_to_s (Subtract, s1, s2))
  let ( *. ) s1 s2 = perform (Ap_s's_to_s (Multiply, s1, s2))
  let ( /. ) s1 s2 = perform (Ap_s's_to_s (Divide, s1, s2))

  let zeros ia = perform (Ap_u_to_t (Zeros ia))
  let create ia s = perform (Ap_u_to_t (Create (ia, s)))
  let concatenate ?axis ta = perform (Ap_ta_to_t (Concatenate axis, ta))
  let stack ?axis ta = perform (Ap_ta_to_t (Stack axis, ta))
  let split ?axis ia t = perform (Ap_t_to_ta (Split (axis, ia), t))
  let transpose ?axis t = perform (Ap_t_to_t (Transpose axis, t))
  let reshape t d = perform (Ap_t_to_t (Reshape d, t))
  let squeeze ?axis t =  perform (Ap_t_to_t (Squeeze axis, t))
  let get_slice ill t = perform (Ap_t_to_t (GetSlice ill, t))
  let slice_left t ia = perform (Ap_t_to_t (SliceLeft ia, t))
  let get t ia = perform (Ap_t_to_s (Get ia, t))
  let set_slice ill t1 t2 = perform (Ap_t't_to_t (SetSlice ill, t1, t2))
  let mv ?trans a x = perform (Ap_t't_to_t (MV trans, a, x))
  let exp t = perform (Ap_t_to_t (Exp, t))
  let ( ~- ) t = perform (Ap_t_to_t (Negate, t))
  let pow_const t f = perform (Ap_t_to_t (PowerConst f,t))
  let ( + ) t1 t2 = perform (Ap_t't_to_t (Add, t1, t2))
  let ( - ) t1 t2 = perform (Ap_t't_to_t (Subtract, t1, t2))
  let ( * ) t1 t2 = perform (Ap_t't_to_t (Multiply, t1, t2))
  let ( / ) t1 t2 = perform (Ap_t't_to_t (Divide, t1, t2))
  let sum t = perform (Ap_t_to_s (Sum, t))
  let sum_reduce ?axis t = perform (Ap_t_to_t (SumReduce axis, t))
  let log_sum_exp ?axis ?keep_dims t =
    perform (Ap_t_to_t (LogSumExp (axis, keep_dims), t))
  let scalar_mul s t = perform (Ap_s't_to_t (ScalarMultiply, s ,t))
  let sub_scalar t s = perform (Ap_s't_to_t (SubtractScalar, s ,t))

  (* Simple expand operation. ia contains which axes to expand. *)
  let _expand t shp ia =
    let res = ref t in
    for j = 0 to Stdlib.(Array.length ia - 1) do
      res := concatenate ~axis:(ia.(j)) (Array.make shp.(ia.(j)) !res)
    done;
    !res

  (* Inverse of a permutation *)
  let _inv_perm p =
    let l = Array.length p in
    let q = Array.make l 0 in
    for i = 0 to Stdlib.(l - 1) do
      q.(p.(i)) <- i;
    done;
    q

  let op_u_to_s (o : u_to_s) = match o with
    | Const x -> c x
  let op_s_to_s (o : s_to_s) s = match o with
    | Negate -> ~. s
    | Log -> log s
  let op_s's_to_s (o : s's_to_s) s1 s2 = match o with
    | Add -> s1 +. s2
    | Subtract -> s1 -. s2
    | Multiply -> s1 *. s2
    | Divide -> s1 /. s2
  
  let op_u_to_t (o : u_to_t) = match o with
    | Zeros ia -> zeros ia
    | Create (ia, f) -> create ia f
  let op_t_to_t (o : t_to_t) t = match o with
    | Squeeze iao -> squeeze ?axis:iao t
    | Reshape d -> reshape t d
    | GetSlice ill -> get_slice ill t
    | SliceLeft ia -> slice_left t ia
    | Transpose iao -> transpose ?axis:iao t
    | Exp -> exp t
    | Negate -> ~- t
    | PowerConst f -> pow_const t f
    | SumReduce iao -> sum_reduce ?axis:iao t
    | LogSumExp (io, bo) -> log_sum_exp ?axis:io ?keep_dims:bo t
  let op_t't_to_t (o : t't_to_t) t1 t2 = match o with
    | Add -> t1 + t2
    | Subtract -> t1 - t2
    | Multiply -> t1 * t2
    | Divide -> t1 / t2
    | MV bo -> mv ?trans:bo t1 t2
    | SetSlice ill -> set_slice ill t1 t2

  let op_t_to_s (o : t_to_s) t = match o with
    | Get ia -> get t ia
    | Sum -> sum t
  let op_s't_to_t (o : s't_to_t) s t = match o with
    | ScalarMultiply -> scalar_mul s t
    | SubtractScalar -> sub_scalar t s
  let op_ta_to_t (o : ta_to_t) ta = match o with
    | Concatenate io ->  concatenate ?axis:io ta
    | Stack io -> stack ?axis:io ta
  let op_t_to_ta (o : t_to_ta) t = match o with
    | Split (io, ia) -> split ?axis:io ia t

  let der_s_to_s (o : s_to_s) s = match o with
    | Negate -> fun sd -> ~. sd
    | Log -> fun sd -> sd /. s
  let der_s's_to_s (o : s's_to_s) s1 s2 = match o with
    | Add -> fun sd -> (sd, sd)
    | Subtract -> fun sd -> (sd, ~. sd)
    | Multiply -> fun sd -> (s2 *. sd, s1 *. sd)
    | Divide -> fun sd -> (sd /. s2, (sd *. (~. s1)) /. (s2 *. s2))

  let der_t_to_t (o : t_to_t) t = match o with
    | Squeeze _ -> fun td -> reshape td (shape t)
    | Reshape _ -> fun td -> reshape td (shape t)
    | GetSlice ill -> fun td -> set_slice ill (zeros (shape t)) td
    | SliceLeft ia -> fun td ->
      let ill = Array.to_list (Array.map (fun i -> [i]) ia) in
      let shp = Array.(append (make (length ia) 1) (shape td)) in
      let tdr = reshape td shp in
      set_slice ill (zeros (shape t)) tdr
    | Transpose iao ->
      let ia = match iao with
        | None ->
          let d = Array.length (shape t) in
          Array.init d Stdlib.(fun i -> d - i - 1)
        | Some ia -> ia
      in
      fun td -> transpose ~axis:(_inv_perm ia) td
    | Exp -> fun td -> exp t * td
    | Negate -> fun td -> ~- td
    | PowerConst f -> fun td ->
      scalar_mul (c f) (td * pow_const t Stdlib.(f -. 1.0))
    | SumReduce iao ->
      let ia = (match iao with
        | None -> Array.init (Array.length (shape t)) (fun i -> i) 
        | Some ia -> ia
      ) in
      fun td -> _expand td (shape t) ia
    | LogSumExp (io, bo) -> (
      let (i, b) = match (io, bo) with
        | (None, None) -> (0, true)
        | (Some i, None) -> (i, true)
        | (None, Some b) -> (0, b)
        | (Some i, Some b) -> (i, b)
      in
      if b
        then fun td ->
          let et = exp t in
          td * (et / sum_reduce ~axis:[|i|] et)
        else fun td ->
          let shp = shape t in
          shp.(i) <- 1;
          let et = exp t in
          (reshape td shp) * (et / sum_reduce ~axis:[|i|] et)
    )
  let der_t't_to_t (o : t't_to_t) t1 t2 = match o with
    | Add -> fun td -> (td, td)
    | Subtract -> fun td -> (td, ~- td)
    | Multiply -> fun td -> (t2 * td, t1 * td)
    | Divide -> fun td -> (td / t2, (td * (~- t1)) / (t2 * t2))
    | SetSlice ill -> fun td ->
      (set_slice ill td (zeros (shape t2)), get_slice ill td)
    | MV bo ->
      let b = match bo with
        | None -> false
        | Some b -> b
      in
      (* a is a matrx, x is a vector *)
      let (a, x) = (t1, t2) in
      (fun td ->
        let tdm = reshape td [|(shape td).(0); 1|] in
        let xm = reshape x [|1; (shape x).(0)|] in
        (* outer product of td and x^T, stored in ad*)
        let ad = tdm * xm in
        let adt = if b then transpose ~axis:[|1;0|] ad else ad in
        let xd = mv ~trans:(not b) a td in
        (adt, xd)
      )

  let der_t_to_s (o : t_to_s) t = match o with
    | Get ia ->
      let ill = Array.to_list (Array.map (fun i -> [i]) ia) in
      (fun sd ->
        let ones = Array.(make (length (shape t)) 1) in
        set_slice ill (zeros (shape t)) (scalar_mul sd (create ones 1.0))
      )
    | Sum -> fun sd -> scalar_mul sd (create (shape t) 1.0)
  let der_s't_to_t (o : s't_to_t) s t = match o with
    | ScalarMultiply -> fun td -> (sum (t * td), scalar_mul s td)
    | SubtractScalar -> fun td -> (~. (sum td), td)
  let der_ta_to_t (o : ta_to_t) ta = match o with
    | Concatenate io ->
      let i = (match io with
        | None -> 0
        | Some i -> i
      ) in
      fun td -> split ~axis:i (Array.map (fun x -> (shape x).(i)) ta) td
    | Stack io ->
      let i = (match io with
        | None -> 0
        | Some i -> i
      ) in
      (fun td ->
        let shp = shape td in
        let ndim = Array.length shp in
        let axis = Owl_utils.adjust_index i ndim in
        let inp_shp = shape ta.(0) in
        split ~axis:i (Array.make shp.(axis) 1) td
          |> Array.map (fun x -> reshape x inp_shp)
      )
  let der_t_to_ta (o : t_to_ta) _ = match o with
    | Split (io, _) ->
      let i = (match io with
        | None -> 0
        | Some i -> i
      ) in
      fun tda -> concatenate ~axis:i tda
end
