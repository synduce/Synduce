open Base
open Lang

(**
  PsiDef.t is the definition of the problem: the goal is to synthesize the unknowns in target such
  that target = orig ∘ repr
*)
type t =
  { filename : string (** The original file of the problem. *)
  ; id : int (** An identifier for the problem. *)
  ; target : PMRS.t (**
The target recursion skeleton in the problem.
*)
  ; reference : PMRS.t (**
The reference function in the problem.
*)
  ; repr : PMRS.t (**
The representation function in the problem.
*)
  ; tinv : PMRS.t option (**
The requires predicate of the target recursion skeleton.
*)
  ; repr_is_identity : bool (**
A boolean that is true iff psi_repr is identity.
*)
  ; lifting : RType.t list
        (** The current lifting: the output type of psi_target
should be !_alpha extended with the tuple of types psi_lifting. *)
  }

let logics (p : t) = [ p.repr.plogic; p.reference.plogic; p.target.plogic ]
let compare (p1 : t) (p2 : t) = compare p1.id p2.id
let equal (p1 : t) (p2 : t) = compare p1 p2 = 0
let hash (p : t) = p.id

(* State variables for the algorithm. *)
let psi_id : int ref = ref 0

let new_psi_id () : int =
  let id = !psi_id in
  Int.incr psi_id;
  id
;;
