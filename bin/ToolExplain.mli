val find_matching_unknown
  :  Lang.Variables.VarSet.t
  -> Lang.TermTypes.variable
  -> Lang.TermTypes.variable option

val is_shallow_value : Lang.TermTypes.term -> Lang.TermTypes.term -> bool

val find_missing_argument
  :  ctx:Common.Env.env
  -> Common.ProblemDefs.PsiDef.t
  -> (Lang.TermTypes.variable * (Lang.TermTypes.term * Lang.TermTypes.term)) list
  -> Common.ProblemDefs.witness
  -> unit

val find_missing_delta
  :  ctx:Common.Env.env
  -> Common.ProblemDefs.PsiDef.t
  -> Common.ProblemDefs.unrealizability_witness
  -> unit

val when_unrealizable
  :  ctx:Common.Env.env
  -> Common.ProblemDefs.PsiDef.t
  -> Common.ProblemDefs.unrealizability_witness list
  -> unit
