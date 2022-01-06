open Base
open Lang
open Fmt

type problem_descr =
  { pd_target : Term.function_descr list
  ; pd_reference : Term.function_descr list
  ; pd_repr : Term.function_descr list
  }

let pp_problem_descr (fmt : Formatter.t) (prob : problem_descr) =
  pf fmt "> Target <@.";
  pf fmt "@[<v>%a@]@." (list ~sep:cut (box Term.pp_function_descr)) prob.pd_target;
  pf fmt "@.> Reference <@.";
  pf fmt "@[<v>%a@]@." (list ~sep:cut (box Term.pp_function_descr)) prob.pd_reference;
  pf fmt "@.> Representation <@.";
  pf fmt "@[<v>%a@]@." (list ~sep:cut (box Term.pp_function_descr)) prob.pd_repr
;;

let problem_descr_of_psi_def (problem : Algo.AState.PsiDef.t) =
  { pd_target = PMRS.func_of_pmrs problem.target
  ; pd_reference = PMRS.func_of_pmrs problem.reference
  ; pd_repr = PMRS.func_of_pmrs problem.repr
  }
;;
