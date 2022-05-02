open Base
open Term
open Utils

let simplify t =
  let rrule t =
    match t.tkind with
    | TUn (Unop.Neg, t1) ->
      (match t1.tkind with
      | TConst (Constant.CInt i) -> mk_const (Constant.CInt (-i))
      | _ -> t)
    | TUn (Unop.Not, t1) ->
      (match t1.tkind with
      | TConst Constant.CTrue -> mk_const Constant.CFalse
      | TConst Constant.CFalse -> mk_const Constant.CTrue
      | _ -> t)
    | TBin (Binop.Plus, t1, t2) ->
      (match t1.tkind, t2.tkind with
      | TConst (Constant.CInt i1), TConst (Constant.CInt i2) ->
        mk_const (Constant.CInt (i1 + i2))
      | TConst (Constant.CInt 0), _ -> t2
      | _, TConst (Constant.CInt 0) -> t1
      | _, _ -> t)
    | TBin (Binop.Minus, t1, t2) ->
      (match t1.tkind, t2.tkind with
      | TConst (Constant.CInt i1), TConst (Constant.CInt i2) ->
        mk_const (Constant.CInt (i1 - i2))
      | TConst (Constant.CInt 0), _ -> mk_un Unop.Neg t2
      | _, TConst (Constant.CInt 0) -> t1
      | _, _ -> t)
    | TBin (Binop.Max, t1, t2) ->
      (match t1.tkind, t2.tkind with
      | TConst (Constant.CInt i1), TConst (Constant.CInt i2) ->
        mk_const (Constant.CInt (max i1 i2))
      | _, _ -> t)
    | TBin (Binop.Min, t1, t2) ->
      (match t1.tkind, t2.tkind with
      | TConst (Constant.CInt i1), TConst (Constant.CInt i2) ->
        mk_const (Constant.CInt (max i1 i2))
      | _, _ -> t)
    | TBin (Binop.Gt, t1, t2) ->
      (match t1.tkind, t2.tkind with
      | TConst (Constant.CInt i1), TConst (Constant.CInt i2) ->
        mk_const (if i1 > i2 then CTrue else CFalse)
      | _, _ -> if Terms.equal t1 t2 then mk_const CFalse else t)
    | TBin (Binop.Lt, t1, t2) ->
      (match t1.tkind, t2.tkind with
      | TConst (Constant.CInt i1), TConst (Constant.CInt i2) ->
        mk_const (if i1 < i2 then CTrue else CFalse)
      | _, _ -> if Terms.equal t1 t2 then mk_const CFalse else t)
    | TBin (Binop.Ge, t1, t2) ->
      (match t1.tkind, t2.tkind with
      | TConst (Constant.CInt i1), TConst (Constant.CInt i2) ->
        mk_const (if i1 >= i2 then CTrue else CFalse)
      | _, _ -> t)
    | TBin (Binop.Le, t1, t2) ->
      (match t1.tkind, t2.tkind with
      | TConst (Constant.CInt i1), TConst (Constant.CInt i2) ->
        mk_const (if i1 <= i2 then CTrue else CFalse)
      | _, _ -> t)
    | TBin (Binop.Times, t1, t2) ->
      (match t1.tkind, t2.tkind with
      | TConst (Constant.CInt i1), TConst (Constant.CInt i2) ->
        mk_const (Constant.CInt (i1 * i2))
      | TConst (Constant.CInt 1), _ -> t2
      | _, TConst (Constant.CInt 1) -> t1
      | TConst (Constant.CInt 0), _ -> t1
      | _, TConst (Constant.CInt 0) -> t2
      | _, _ -> t)
    | TBin (Binop.Div, t1, t2) ->
      (match t1.tkind, t2.tkind with
      | TConst (Constant.CInt i1), TConst (Constant.CInt i2) ->
        mk_const (Constant.CInt (i1 / i2))
      | _, TConst (Constant.CInt 1) -> t2
      | TConst (Constant.CInt 0), _ -> t1
      | _, _ -> t)
    | TBin (Binop.Or, t1, t2) ->
      (match t1.tkind, t2.tkind with
      | _, TConst Constant.CTrue -> t2
      | TConst Constant.CTrue, _ -> t1
      | _, TConst Constant.CFalse -> t1
      | TConst Constant.CFalse, _ -> t2
      | _, _ -> t)
    | TBin (Binop.And, t1, t2) ->
      (match t1.tkind, t2.tkind with
      | _, TConst Constant.CTrue -> t1
      | TConst Constant.CTrue, _ -> t2
      | TBin (Binop.Or, { tkind = TUn (Unop.Not, ta); _ }, tb), _
        when Term.term_equal ta t2 -> Terms.(ta && tb)
      | TConst Constant.CFalse, _ | _, TConst Constant.CFalse -> mk_const Constant.CFalse
      | _, _ -> t)
    (* t1 = t2 *)
    | TBin (Binop.Eq, t1, t2) ->
      (match t1.tkind, t2.tkind with
      | TConst (Constant.CInt i1), TConst (Constant.CInt i2) ->
        if i1 = i2 then mk_const Constant.CTrue else mk_const Constant.CFalse
      | tc, TBin (Binop.Max, ta, tb) | TBin (Binop.Max, ta, tb), tc ->
        let tc = { t1 with tkind = tc } in
        if Terms.equal tc ta
        then (* ta = max ta tb -> ta >= tb *)
          mk_bin Binop.Ge ta tb
        else if Terms.equal tc tb
        then (* tb = max ta tb -> tb >= ta *)
          mk_bin Binop.Ge tb ta
        else t
      | tc, TBin (Binop.Min, ta, tb) | TBin (Binop.Min, ta, tb), tc ->
        let tc = { t1 with tkind = tc } in
        if Terms.equal tc ta
        then (* ta = min ta tb -> tb >= ta *)
          mk_bin Binop.Ge tb ta
        else if Terms.equal tc tb
        then (* tb = min ta tb -> ta >= tb *)
          mk_bin Binop.Ge ta tb
        else t
      | _, _ -> if Terms.equal t1 t2 then mk_const Constant.CTrue else t)
    | TIte
        (cond, { tkind = TConst Constant.CTrue; _ }, { tkind = TConst Constant.CFalse; _ })
      -> cond
    | TIte (cond, tt, tf) ->
      (match cond.tkind with
      | TConst Constant.CTrue -> tt
      | TConst Constant.CFalse -> tf
      | _ -> t)
    | _ -> t
  in
  let rec until_stable iters t =
    let t' = rewrite_with rrule t in
    if iters > !Config.reduction_limit && Terms.equal t' t
    then t'
    else until_stable (iters + 1) t'
  in
  until_stable 0 t
;;

let in_model ?(no_simplify = false) ~(ctx : Context.t) (vmap : term VarMap.t) (t : term) =
  let simplify = if no_simplify then identity else simplify in
  let remap _ t =
    match t.tkind with
    | TVar v -> Map.find vmap v
    | _ -> None
  in
  fst (Term.infer_type ctx (simplify (transform ~case:remap t)))
;;
