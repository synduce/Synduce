(** A configuration is a set of argument for unknowns in a recursion skeleton.  *)
open Base

open Common
open Env
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
  Hashtbl.for_all ~f:(fun o -> o < 2) counts
;;

let base_type_args (ctx : env) (p : PMRS.t) (vs : VarSet.t) =
  let base_type_vars =
    Set.filter vs ~f:(fun v ->
        RType.is_recursive ctx.ctx.types ((ctx @>- Variable.vtype_or_new) v))
  in
  let rec_function_applications =
    List.map (VarSet.elements p.pnon_terminals) ~f:(fun f_var ->
        let argtypes, _ = RType.fun_typ_unpack ((ctx @>- Variable.vtype_or_new) f_var) in
        let argsets =
          Utils.cartesian_nary_product
            (List.map argtypes ~f:(fun t ->
                 Set.elements ((ctx @>- VarSet.filter_by_type) vs t)))
        in
        Utils.cartesian_nary_product argsets)
  in
  let _ = base_type_vars, rec_function_applications in
  ()
;;

(** Build the argument map of the PMRS.
  The argument map is effectively the largest configuration: it is a map from each unknown to
  all the arguments available to the unknown.
*)
let build_argmap (ctx : env) (p : PMRS.t) =
  let empty_conf = of_varset p.psyntobjs in
  let f ~key:_ruleid ~data:(_, lhs_args, lhs_pat, rhs) accum =
    let lhs_argset =
      Set.union
        (VarSet.of_list lhs_args)
        (Option.value_map lhs_pat ~default:VarSet.empty ~f:(fun p ->
             ctx >- Analysis.free_variables (Term.term_of_pattern ctx.ctx p)))
    in
    let _ = rhs, lhs_argset in
    accum
  in
  Map.fold p.prules ~init:empty_conf ~f
;;

(** Return the configuration of a PMRS, assuming it has been checked. *)
let configuration_of (p : PMRS.t) : t =
  let init = of_varset p.psyntobjs in
  (* TODO: collect configuration in the rules. *)
  init
;;
