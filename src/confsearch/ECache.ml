open Base
open Lang
open Term
open EProps
open Common.ProblemDefs

type t =
  { exprs : ExpressionSet.t Hash_set.t
  ; ectx : RContext.t
  }

let is_empty (c : t) = Hash_set.is_empty c.exprs

let dump (fmt : Formatter.t) (c : t) =
  Fmt.(list ExpressionSet.pp) fmt (Hash_set.to_list c.exprs)
;;

let create ~(ctx : Term.Context.t) () : t =
  { exprs = Hash_set.create (module ExpressionSet); ectx = RContext.create ctx }
;;

let add (cache : t) (exprs : ExpressionSet.t) : unit = Hash_set.add cache.exprs exprs

let norm (e : equation) =
  let t = Terms.(e.elhs == e.erhs) in
  Expression.(Option.map ~f:nameless_normal_form (of_term t))
;;

(** `check_subset e c` checks whether some key in the cache `c` is a subset
    of `e`.
   *)
let check_subset (eset : ExpressionSet.t) (c : t) : bool =
  Hash_set.exists c.exprs ~f:(fun eset' -> Set.is_subset eset' ~of_:eset)
;;
