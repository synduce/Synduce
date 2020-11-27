open Base
open Lang.PMRScheme
open Lang.Term
open Lang.Analysis

let most_general_terms (prog : pmrs) =
  let xi = prog.pparams in
  let xi_init =
    let xim =
      match
        Map.of_alist (module Int)
          (List.map ~f:(fun v -> (v.vid, [])) (VarSet.elements xi))
      with
      | `Duplicate_key _ -> failwith "impossible"
      | `Ok xmap -> xmap
    in
    let f ~key:rule_id ~data:(_,_,_,rule_rhs) acc =
      let rule_unknowns =
        Set.inter xi (free_variables rule_rhs)
      in
      List.fold ~init:acc
        ~f:(fun xmap xi -> Map.add_multi xmap ~key:xi.vid ~data:rule_id)
        (Set.elements rule_unknowns)
    in
    Map.fold ~init:xim ~f prog.prules
  in
  xi_init

