open Effect

type u_to_s = Const of float
type s_to_s = Negate | Log
type s's_to_s = Add | Subtract | Multiply | Divide

type u_to_t = Zeros of int array
type t_to_t
  = Squeeze of int array option
  | GetSlice of int list list
  | SliceLeft of int array
  | Exp
  | SumReduce of int array option
  | LogSumExp of int option * bool option
type t't_to_t = Add | Subtract | Multiply
type t't_in_t = MVInplace

type s_to_t = Create of int array
type t_to_s = Get of int array
type s't_to_t = ScalarMultiply | PowerScalar | SubtractScalar
type ta_to_t = Concatenate of int option | Stack of int option

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
    | Ap_t't_in_t : t't_in_t * tensor * tensor * tensor -> unit Effect.t
    | Ap_s_to_t : s_to_t * scalar -> tensor Effect.t
    | Ap_t_to_s : t_to_s * tensor -> scalar Effect.t
    | Ap_s't_to_t : s't_to_t * scalar * tensor -> tensor Effect.t
    | Ap_ta_to_t : ta_to_t * tensor array -> tensor Effect.t

  val c : float -> scalar
  val ( ~. ) : scalar -> scalar
  val log : scalar -> scalar
  val ( +. ) : scalar -> scalar -> scalar
  val ( -. ) : scalar -> scalar -> scalar
  val ( *. ) : scalar -> scalar -> scalar
  val ( /. ) : scalar -> scalar -> scalar

  (* Creating constant tensors *)
  val zeros : int array -> tensor
  val create : int array -> scalar -> tensor

  (* Combining tensors *)
  val concatenate : ?axis:int -> tensor array -> tensor
  val stack : ?axis:int -> tensor array -> tensor

  (* Shrinking and slicing tensors *)
  val squeeze : ?axis:int array -> tensor -> tensor
  val get_slice : int list list -> tensor -> tensor
  val slice_left : tensor -> int array -> tensor
  val get : tensor -> int array -> scalar

  (* Matrix-vector multiplication *)
  val mv_inplace : tensor -> tensor -> tensor -> unit

  (* Pointwise tensor operations *)
  val exp : tensor -> tensor
  val add : tensor -> tensor -> tensor
  val sub : tensor -> tensor -> tensor
  val mul : tensor -> tensor -> tensor

  (* Reduction operations *)
  val sum_reduce : ?axis:int array -> tensor -> tensor
  val log_sum_exp : ?axis:int -> ?keep_dims:bool -> tensor -> tensor

  (* Scalar-tensor operations *)
  val scalar_mul : scalar -> tensor -> tensor
  val pow_scalar : tensor -> scalar -> tensor
  val sub_scalar : tensor -> scalar -> tensor

  val op_u_to_s: u_to_s -> scalar
  val op_s_to_s: s_to_s -> scalar -> scalar
  val op_s's_to_s : s's_to_s -> scalar -> scalar -> scalar

  val op_u_to_t : u_to_t -> tensor
  val op_t_to_t : t_to_t -> tensor -> tensor
  val op_t't_to_t : t't_to_t -> tensor -> tensor -> tensor
  val op_t't_in_t : t't_in_t -> tensor -> tensor -> tensor -> unit

  val op_s_to_t : s_to_t -> scalar -> tensor
  val op_t_to_s : t_to_s -> tensor -> scalar
  val op_s't_to_t : s't_to_t -> scalar -> tensor -> tensor
  val op_ta_to_t : ta_to_t -> tensor array -> tensor

  val der_s_to_s : s_to_s -> scalar -> scalar
  val der_s's_to_s : s's_to_s -> scalar -> scalar -> scalar * scalar

  (* val der_t_to_t : t_to_t -> tensor -> tensor
  val der_t't_to_t : t't_to_t -> tensor -> tensor -> tensor * tensor
  val der_t't_in_t : t't_in_t -> tensor -> tensor -> tensor * tensor
  
  val der_s_to_t : s_to_t -> scalar -> scalar
  val der_t_to_s : t_to_s -> tensor -> tensor
  val der_s't_to_t : s't_to_t -> scalar -> tensor -> scalar * tensor
  val der_ta_to_t : ta_to_t -> tensor array -> tensor array *)
end

module Smooth (T : sig type scalar type tensor end) : SMOOTH with type scalar = T.scalar with type tensor = T.tensor = struct
  type scalar = T.scalar
  type tensor = T.tensor
  type _ Effect.t +=
      Ap_u_to_s : u_to_s -> scalar Effect.t
    | Ap_s_to_s : s_to_s * scalar -> scalar Effect.t
    | Ap_s's_to_s : s's_to_s * scalar * scalar -> scalar Effect.t
    | Ap_u_to_t : u_to_t -> tensor Effect.t
    | Ap_t_to_t : t_to_t * tensor -> tensor Effect.t
    | Ap_t't_to_t : t't_to_t * tensor * tensor -> tensor Effect.t
    | Ap_t't_in_t : t't_in_t * tensor * tensor * tensor -> unit Effect.t
    | Ap_s_to_t : s_to_t * scalar -> tensor Effect.t
    | Ap_t_to_s : t_to_s * tensor -> scalar Effect.t
    | Ap_s't_to_t : s't_to_t * scalar * tensor -> tensor Effect.t
    | Ap_ta_to_t : ta_to_t * tensor array -> tensor Effect.t

  let c s = perform (Ap_u_to_s (Const s))
  let log s = perform (Ap_s_to_s (Log, s))
  let ( ~. ) s = perform (Ap_s_to_s (Negate, s))
  let ( +. ) s1 s2 = perform (Ap_s's_to_s (Add, s1, s2))
  let ( -. ) s1 s2 = perform (Ap_s's_to_s (Subtract, s1, s2))
  let ( *. ) s1 s2 = perform (Ap_s's_to_s (Multiply, s1, s2))
  let ( /. ) s1 s2 = perform (Ap_s's_to_s (Divide, s1, s2))

  let zeros ia = perform (Ap_u_to_t (Zeros ia))
  let create ia s = perform (Ap_s_to_t (Create ia, s))
  let concatenate ?axis ta = perform (Ap_ta_to_t (Concatenate axis, ta))
  let stack ?axis ta = perform (Ap_ta_to_t (Stack axis, ta))
  let squeeze ?axis t = perform (Ap_t_to_t (Squeeze axis, t))
  let get_slice ill t = perform (Ap_t_to_t (GetSlice ill, t))
  let slice_left t ia = perform (Ap_t_to_t (SliceLeft ia, t))
  let get t ia = perform (Ap_t_to_s (Get ia, t))
  let mv_inplace a x y = perform (Ap_t't_in_t (MVInplace, a, x, y))
  let exp t = perform (Ap_t_to_t (Exp, t))
  let add t1 t2 = perform (Ap_t't_to_t (Add, t1, t2))
  let sub t1 t2 = perform (Ap_t't_to_t (Subtract, t1, t2))
  let mul t1 t2 = perform (Ap_t't_to_t (Multiply, t1, t2))
  let sum_reduce ?axis t = perform (Ap_t_to_t (SumReduce axis, t))
  let log_sum_exp ?axis ?keep_dims t =
    perform (Ap_t_to_t (LogSumExp (axis, keep_dims), t))
  let scalar_mul s t = perform (Ap_s't_to_t (ScalarMultiply, s ,t))
  let pow_scalar t s = perform (Ap_s't_to_t (PowerScalar, s ,t))
  let sub_scalar t s = perform (Ap_s't_to_t (SubtractScalar, s ,t))

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
  let op_t_to_t (o : t_to_t) t = match o with
    | Squeeze iao -> (match iao with
      | None -> squeeze t
      | Some ia -> squeeze ~axis:ia t
      )
    | GetSlice ill -> get_slice ill t
    | SliceLeft ia -> slice_left t ia
    | Exp -> exp t
    | SumReduce iao -> (match iao with
      | None -> sum_reduce t
      | Some ia -> sum_reduce ~axis:ia t
      )
    | LogSumExp (io, bo) -> (match (io, bo) with
      | (None, None) -> log_sum_exp t
      | (Some i, None) -> log_sum_exp ~axis:i t
      | (None, Some b) -> log_sum_exp ~keep_dims:b t
      | (Some i, Some b) -> log_sum_exp ~axis:i ~keep_dims:b t
      )
  let op_t't_to_t (o : t't_to_t) t1 t2 = match o with
    | Add -> add t1 t2
    | Subtract -> sub t1 t2
    | Multiply -> mul t1 t2
  let op_t't_in_t (o : t't_in_t) a x y = match o with
    | MVInplace -> mv_inplace a x y

  let op_s_to_t (o : s_to_t) s = match o with
    | Create ia -> create ia s
  let op_t_to_s (o : t_to_s) t = match o with
    | Get ia -> get t ia
  let op_s't_to_t (o : s't_to_t) s t = match o with
    | ScalarMultiply -> scalar_mul s t
    | PowerScalar -> pow_scalar t s
    | SubtractScalar -> sub_scalar t s
  let op_ta_to_t (o : ta_to_t) ta = match o with
    | Concatenate io -> (match io with
      | None -> concatenate ta
      | Some i -> concatenate ~axis:i ta
      )
    | Stack io -> (match io with
      | None -> stack ta
      | Some i -> stack ~axis:i ta
      )

  let der_s_to_s (o : s_to_s) s = match o with
    | Negate -> ~. (c 1.0)
    | Log -> (c 1.0) /. s
  let der_s's_to_s (o : s's_to_s) s1 s2 = match o with
    | Add -> (c 1.0, c 1.0)
    | Subtract -> (c 1.0, c (-1.0))
    | Multiply -> (s2, s1)
    | Divide -> ((c 1.0) /. s2, (~. s1) /. (s2 *. s2))
end
