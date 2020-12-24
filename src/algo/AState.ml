open Lang

type psi_def = { target : PMRS.t; orig : PMRS.t; repr : PMRS.t; repr_is_identity : bool }

let _tau = ref RType.TInt

let _theta = ref RType.TInt

let _alpha : (RType.t * (Term.term option)) ref = ref (RType.TInt, None)