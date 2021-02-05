open Term

let simplify =
  let rrule t =
    match t.tkind with
    | TBin (Binop.Plus, t1, t2) -> (
        match (t1.tkind, t2.tkind) with
        | TConst (Constant.CInt i1), TConst (Constant.CInt i2) -> mk_const (Constant.CInt (i1 + i2))
        | TConst (Constant.CInt 0), _ -> t1
        | _, TConst (Constant.CInt 0) -> t2
        | _, _ -> t )
    | TBin (Binop.Minus, t1, t2) -> (
        match (t1.tkind, t2.tkind) with
        | TConst (Constant.CInt i1), TConst (Constant.CInt i2) -> mk_const (Constant.CInt (i1 - i2))
        | TConst (Constant.CInt 0), _ -> t1
        | _, TConst (Constant.CInt 0) -> mk_un Unop.Neg t2
        | _, _ -> t )
    | TBin (Binop.Times, t1, t2) -> (
        match (t1.tkind, t2.tkind) with
        | TConst (Constant.CInt i1), TConst (Constant.CInt i2) -> mk_const (Constant.CInt (i1 * i2))
        | TConst (Constant.CInt 1), _ -> t1
        | _, TConst (Constant.CInt 1) -> t2
        | _, _ -> t )
    | TBin (Binop.Div, t1, t2) -> (
        match (t1.tkind, t2.tkind) with
        | TConst (Constant.CInt i1), TConst (Constant.CInt i2) -> mk_const (Constant.CInt (i1 / i2))
        | _, TConst (Constant.CInt 1) -> t2
        | _, _ -> t )
    | TBin (Binop.Or, t1, t2) -> (
        match (t1.tkind, t2.tkind) with
        | _, TConst Constant.CTrue -> mk_const Constant.CTrue
        | TConst Constant.CTrue, _ -> mk_const Constant.CTrue
        | _, TConst Constant.CFalse -> t1
        | TConst Constant.CFalse, _ -> t2
        | _, _ -> t )
    | TBin (Binop.And, t1, t2) -> (
        match (t1.tkind, t2.tkind) with
        | _, TConst Constant.CTrue -> t1
        | TConst Constant.CTrue, _ -> t2
        | TConst Constant.CFalse, _ | _, TConst Constant.CFalse -> mk_const Constant.CFalse
        | _, _ -> t )
    | _ -> failwith "not implemented"
  in
  rewrite_with rrule
