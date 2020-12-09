open Lang

type psi_def = { target : PMRS.t; orig : PMRS.t; repr : PMRS.t }

let tau = ref RType.TInt

let theta = ref RType.TInt

let alpha = ref RType.TInt