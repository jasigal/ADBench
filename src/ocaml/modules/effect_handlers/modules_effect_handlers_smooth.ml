open Effect

type nullary = Const of float
type unary = Negate | Sin | Cos | Exp
type binary = Plus | Subtract | Times | Divide

type arg = L | R

module type SMOOTH = sig
  type t
  type _ Effect.t += Ap0 : nullary -> t Effect.t
                   | Ap1 : unary * t -> t Effect.t
                   | Ap2 : binary * t * t -> t Effect.t

  val c : float -> t
  val ( ~. ) : t -> t
  val sin_ : t -> t
  val cos_ : t -> t
  val exp_ : t -> t
  val ( +. ) : t -> t -> t
  val ( -. ) : t -> t -> t
  val ( *. ) : t -> t -> t
  val ( /. ) : t -> t -> t

  val op0 : nullary -> t
  val op1 : unary -> t -> t
  val op2 : binary -> t -> t -> t

  val der1 : unary -> t -> t
  val dder1 : unary -> t -> t
  val der2 : binary -> arg -> t -> t -> t
  val dder2 : binary -> bool -> arg -> arg -> t -> t -> t
end

module Smooth (T : sig type t end) : SMOOTH with type t = T.t = struct
  type t = T.t
  type _ Effect.t += Ap0 : nullary -> t Effect.t
                   | Ap1 : unary * t -> t Effect.t
                   | Ap2 : binary * t * t -> t Effect.t

  let c x = perform (Ap0 (Const x))
  let ( ~. ) a = perform (Ap1 (Negate, a))
  let sin_ a = perform (Ap1 (Sin, a))
  let cos_ a = perform (Ap1 (Cos, a))
  let exp_ a = perform (Ap1 (Exp, a))
  let ( +. ) a b = perform (Ap2 (Plus, a, b))
  let ( -. ) a b = perform (Ap2 (Subtract, a, b))
  let ( *. ) a b = perform (Ap2 (Times, a, b))
  let ( /. ) a b = perform (Ap2 (Divide, a, b))

  let op0 n = match n with
    | Const x -> c x
  let op1 u x = match u with
    | Negate -> ~. x
    | Sin -> sin_ x
    | Cos -> cos_ x
    | Exp -> exp_ x
  let op2 b x y = match b with
    | Plus -> x +. y
    | Subtract -> x -. y
    | Times -> x *. y
    | Divide -> x /. y

  let der1 u x = match u with
    | Negate -> ~. (c 1.0)
    | Sin -> cos_ x
    | Cos -> ~. (sin_ x)
    | Exp -> exp_ x
  let dder1 u x = match u with
    | Negate -> c 0.0
    | Sin -> ~. (sin_ x)
    | Cos -> ~. (cos_ x)
    | Exp -> exp_ x
  let der2 b a x y = match b with
    | Plus -> (match a with L -> c 1.0 | R -> c 1.0)
    | Subtract -> (match a with L -> c 1.0 | R -> c (-1.0))
    | Times -> (match a with L -> y | R -> x)
    | Divide -> (match a with L -> (c 1.0) /. y | R -> (~. x) /. (y *. y))
  let dder2 b same a1 a2 x y =
    if same then
      match b with
        | Plus -> (match a1 with L -> c 0.0 | R -> c 0.0)
        | Subtract -> (match a1 with L -> c 0.0 | R -> c 0.0)
        | Times -> (match a1 with L -> c 2.0 | R -> c 2.0)
        | Divide -> (match a1 with L -> c 0.0 | R -> c 0.0)
    else
      match b with
        | Plus -> (match a1 with
          | L ->  (match a2 with L -> c 0.0 | R -> c 0.0)
          | R ->  (match a2 with L -> c 0.0 | R -> c 0.0)
          )
        | Subtract -> (match a1 with
          | L ->  (match a2 with L -> c 0.0 | R -> c 0.0)
          | R ->  (match a2 with L -> c 0.0 | R -> c 0.0)
          )
        | Times -> (match a1 with
          | L ->  (match a2 with L -> c 0.0 | R -> c 1.0)
          | R ->  (match a2 with L -> c 1.0 | R -> c 0.0)
          )
        | Divide -> (match a1 with
          | L ->  (match a2 with L -> c 0.0 | R -> (c (-1.0)) /. (y *. y))
          | R ->  (match a2 with
            | L -> (c (-1.0)) /. (y *. y)
            | R -> (c 2.0 *. x) /. (y *. y *. y)
            )
          )
end
