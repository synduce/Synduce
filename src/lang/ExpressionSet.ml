open Base

type elt = Expression.t
type t = (Expression.t, Expression.comparator_witness) Set.t

let empty : t = Set.empty (module Expression)
let singleton : elt -> t = Set.singleton (module Expression)
let of_list : elt list -> t = Set.of_list (module Expression)
let compare : t -> t -> int = Set.compare_m__t (module Expression)
let hash : t -> int = Set.hash_m__t (module Expression)
let hash_fold_t : t Hash.folder = Set.hash_fold_m__t (module Expression)
let sexp_of_t : t -> Sexp.t = Set.sexp_of_m__t (module Expression)
let pp fmt s : unit = Fmt.(braces (list ~sep:comma Expression.pp)) fmt (Set.elements s)
