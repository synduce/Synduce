(** A configuration is a set of argument for unknowns in a recursion skeleton.  *)
open Base

open Common
open Lang
open Term

(**
  A configuration is a map from variables (the unknown functions) to a list of arguments.
*)
type t = term list VarMap.t

(** Create an empty configuration of a set of variables.  *)
let of_varset = VarMap.init_from_varset ~init:(fun _ -> [])

(** Check that the unknowns of a PMRS occur at most once. *)
let check_pmrs (p : PMRS.t) =
  let counts = Hashtbl.create (module Int) in
  Set.iter p.psyntobjs ~f:(fun v -> Hashtbl.set counts ~key:v.vid ~data:0);
  Map.iter ~f:(fun (_, _, _, rhs) -> Analysis.count_occurrences counts rhs) p.prules;
  Hashtbl.for_all ~f:(fun o -> o < 2)
;;

(** Return the configuration of a PMRS, assuming it has been checked. *)
let configuration_of (p : PMRS.t) : t =
  let init = of_varset p.psyntobjs in
  (* TODO: collect configuration in the rules. *)
  init
;;
