module Binop = struct 
  type t = 
    | Lt | Gt | Ge | Le | Eq | Neq
    | Max | Min 
    | Plus | Minus
    | Times | Div | Mod
    | And | Or
end

module Unop = struct
  type t =
    | Neg | Not
    | Abs
end

module Constant = struct 
  type t = 
    | CInt of int 
    | CTrue 
    | CFalse 

  let of_int i = CInt i 
  let of_bool b = if b then CTrue else CFalse
  let _if c t f =
    match c with CTrue -> t | _ -> f

end