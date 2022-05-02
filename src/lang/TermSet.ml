open Base
open Term
module S = Set.M (Terms)
include S

let equal (a : t) (b : t) : bool = Set.equal a b
let compare (a : t) (b : t) : int = Set.compare_direct a b
let sexp_of_t (a : t) : Sexp.t = Set.sexp_of_m__t (module Terms) a
let hash : t -> int = Set.hash_m__t (module Terms)
let hash_fold_t : t Hash.folder = Set.hash_fold_m__t (module Terms)
let empty = Set.empty (module Terms)
let map (s : S.t) = Set.map (module Terms) s
let singleton = Set.singleton (module Terms)
let of_list = Set.of_list (module Terms)
let of_varset : VarSet.t -> t = Set.map (module Terms) ~f:mk_var_no_ctx
let union_list = Set.union_list (module Terms)
let filter_by_type t typ = Set.filter ~f:(fun t -> RType.t_equals (type_of t) typ) t

let pp (ctx : Context.t) (f : Formatter.t) (s : t) =
  Fmt.(braces (list ~sep:comma (box (pp_term ctx)))) f (Set.elements s)
;;
