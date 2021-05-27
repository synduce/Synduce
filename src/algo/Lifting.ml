open Base
open Lang.Term
open AState
open Syguslib.Sygus
open Utils

let scalar (_p : psi_def) ((_t, _u) : TermSet.t * TermSet.t) _loop_continuation :
    (soln, solver_response) Result.t =
  Log.error_msg "Lifting is only a stub.";
  (*
    This function will perform the scalar lifting and call the loop continuation
    with the lifted problem.
  *)
  Error RFail
