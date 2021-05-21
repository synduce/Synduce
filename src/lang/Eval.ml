open Term
open Utils

let simplify t =
  let rrule t =
    match t.tkind with
    | TBin (Binop.Plus, t1, t2) -> (
        match (t1.tkind, t2.tkind) with
        | TConst (Constant.CInt i1), TConst (Constant.CInt i2) -> mk_const (Constant.CInt (i1 + i2))
        | TConst (Constant.CInt 0), _ -> t2
        | _, TConst (Constant.CInt 0) -> t1
        | _, _ -> t)
    | TBin (Binop.Minus, t1, t2) -> (
        match (t1.tkind, t2.tkind) with
        | TConst (Constant.CInt i1), TConst (Constant.CInt i2) -> mk_const (Constant.CInt (i1 - i2))
        | TConst (Constant.CInt 0), _ -> mk_un Unop.Neg t2
        | _, TConst (Constant.CInt 0) -> t1
        | _, _ -> t)
    | TBin (Binop.Times, t1, t2) -> (
        match (t1.tkind, t2.tkind) with
        | TConst (Constant.CInt i1), TConst (Constant.CInt i2) -> mk_const (Constant.CInt (i1 * i2))
        | TConst (Constant.CInt 1), _ -> t2
        | _, TConst (Constant.CInt 1) -> t1
        | _, _ -> t)
    | TBin (Binop.Div, t1, t2) -> (
        match (t1.tkind, t2.tkind) with
        | TConst (Constant.CInt i1), TConst (Constant.CInt i2) -> mk_const (Constant.CInt (i1 / i2))
        | _, TConst (Constant.CInt 1) -> t2
        | _, _ -> t)
    | TBin (Binop.Or, t1, t2) -> (
        match (t1.tkind, t2.tkind) with
        | _, TConst Constant.CTrue -> mk_const Constant.CTrue
        | TConst Constant.CTrue, _ -> mk_const Constant.CTrue
        | _, TConst Constant.CFalse -> t1
        | TConst Constant.CFalse, _ -> t2
        | _, _ -> t)
    | TBin (Binop.And, t1, t2) -> (
        match (t1.tkind, t2.tkind) with
        | _, TConst Constant.CTrue -> t1
        | TConst Constant.CTrue, _ -> t2
        | TConst Constant.CFalse, _ | _, TConst Constant.CFalse -> mk_const Constant.CFalse
        | _, _ -> t)
    | TBin (Binop.Eq, t1, t2) -> (
        match (t1.tkind, t2.tkind) with
        | TConst (Constant.CInt i1), TConst (Constant.CInt i2) ->
            if i1 = i2 then mk_const Constant.CTrue else mk_const Constant.CFalse
        | _, _ -> if Terms.equal t1 t2 then mk_const Constant.CTrue else t)
    | TIte (cond, { tkind = TConst Constant.CTrue; _ }, { tkind = TConst Constant.CFalse; _ }) ->
        cond
    | TIte (cond, tt, tf) -> (
        match cond.tkind with TConst Constant.CTrue -> tt | TConst Constant.CFalse -> tf | _ -> t)
    | _ -> t
  in
  let rec until_stable iters t =
    let t' = rewrite_with rrule t in
    if iters > !Config.reduction_limit && Terms.equal t' t then t' else until_stable (iters + 1) t'
  in
  until_stable 0 t
