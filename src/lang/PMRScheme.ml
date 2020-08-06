open Alpha
open Automata
open Base
open Trees
open Utils

open Result.Let_syntax

(* ============================================================================================= *)
(*                      TYPE DEFINITIONS AND UTILS                                               *)
(* ============================================================================================= *)
type non_terminal = { name : string; id : int; arity : int; }

module NonTerminal =
struct
  type t = non_terminal
  let compare t1 t2 = Base.compare t1.id t2.id
  let equal t1 t2 = Base.equal t1.id t2.id

  let mk ?(id = (-1)) (name : string) (arity : int) : t =
    mk_with_id id name (fun id -> { name; id; arity })
end

type variable = { name : string; id : int; }

module Variable =
struct
  type t = variable
  let compare t1 t2 = Base.compare t1.id t2.id
  let equal t1 t2 = Base.equal t1.id t2.id

  let names = List.map ~f:(fun x -> x.name)

  let mk ?(id = (-1)) (name : string) : t =
    mk_with_id id name (fun id -> { name; id })
end


type terminal = Terminal.t


type term =
  | TApp of term * term list
  | TVar of variable
  | TTerm of terminal
  | TNTerm of non_terminal

module Term =
struct
  type t = term
  let rec equal t1 t2 =
    match t1, t2 with
    | TVar v1, TVar v2 -> Variable.equal v1 v2
    | TTerm tt1, TTerm tt2 -> Terminal.equal tt1 tt2
    | TNTerm tn1, TNTerm tn2 -> NonTerminal.equal tn1 tn2
    | TApp(f1, args1), TApp (f2, args2) ->
      if equal f1 f2 then
        List.equal equal args1 args2 else false
    | _ , _ -> false

end

type pattern = terminal * variable list

type rewrite_rule = non_terminal * variable list * pattern option * term

type pmrs = {
  main_id : int;
  rules : (rewrite_rule list) Map.M(Int).t;
  non_terminals : non_terminal list;
  order : int;
}

(* Type shortcuts *)
type 'a xresult = ('a, (string * Sexp.t) list) Result.t
type 'a sresult = ('a, (string * term) list) Result.t
type variables = variable Map.M(String).t

(* ============================================================================================= *)
(*                                 BASIC PROPETIES                                               *)
(* ============================================================================================= *)
(* Update the order of the pmrs. *)
let update_order (p : pmrs) : pmrs =
  let all_rules = List.concat (List.map ~f:snd (Map.to_alist p.rules)) in
  let order =
    let f m (_, args, p, _) =
      max m (List.length args + if Option.is_some p then 1 else 0)
    in
    List.fold ~f ~init:0 all_rules
  in { p with order = order }


(* ============================================================================================= *)
(*                                 PRETTY PRINTING                                               *)
(* ============================================================================================= *)
let rec pp_term (frmt : Formatter.t) (t : term) =
  match t with
  | TApp (f, args) ->
    Fmt.(pf frmt "(%a %a)" pp_term f (list ~sep:sp pp_term) args)
  | TTerm t -> Fmt.(pf frmt "%s" t.name)
  | TNTerm nt -> Fmt.(pf frmt "%s" nt.name)
  | TVar v -> Fmt.(pf frmt "%s" v.name)

let pp_pattern (frmt : Formatter.t) (t, args : pattern) : unit =
  Fmt.(pf frmt "%s(%a)" t.name (list ~sep:comma string) (Variable.names args))

let pp_rewrite_rule (frmt : Formatter.t) (nt, vargs, pat, t : rewrite_rule) : unit =
  Fmt.(pf frmt "%s %a %a --> %a"
         nt.name
         (list ~sep:comma string) (Variable.names vargs)
         (option pp_pattern) pat
         (box pp_term) t)

let pp_pmrs (frmt : Formatter.t) (pmrs : pmrs) : unit =
  List.iter
    ~f:(fun (_, r) ->
        Fmt.(pf frmt "%a@." (list (box pp_rewrite_rule)) r))
    (Map.to_alist pmrs.rules)

(* ============================================================================================= *)
(*                                 PARSING, READING                                              *)
(* ============================================================================================= *)
let term_of_sexp (nts : non_terminal Map.M(String).t)
    (sigma : terminal Map.M(String).t) (vars : variables)
    (st : Sexp.t) : term xresult =
  let rec f sexp =
    match sexp with
    | Sexp.List (hd::tl) ->
      (f hd >>=!
       (fun g ->
          (Result.map_error ~f:List.concat (Result.combine_errors (List.map ~f tl)))
          >>=! (fun args -> Ok (TApp (g, args)))))

    | Sexp.List [] -> Error ["Empty term", sexp]

    | Sexp.Atom a ->
      (match Map.find nts a with
       | Some ntm -> Ok (TNTerm ntm)
       | None ->
         (match Map.find sigma a with
          | Some tm -> Ok (TTerm tm)
          | None ->
            match Map.find vars a with
            | Some v -> Ok (TVar v)
            | None -> Error ["Variable not found", sexp]))
  in f st

let parse_rules (sigma : terminal Map.M(String).t) (rules : Sexp.t) : pmrs xresult =
  let rules =
    let f s =
      match s with
      | Sexp.List [Sexp.Atom nt;Sexp.List vargs;pat;rt] -> Ok (nt, vargs, Some pat, rt)
      | Sexp.List [Sexp.Atom nt;Sexp.List vargs;rt] -> Ok (nt, vargs, None, rt)
      | _ -> Error ("Not a rule.", s)
    in
    match rules with
    | Sexp.List rules -> Result.combine_errors (List.map ~f rules)
    | _ -> Error ["Rules should be a list of s-expressions.", rules]
  in
  let gather_nonterms rules =
    let f acc (nt_name, args, _pat, _) =
      acc >>=!
      (fun (iid, nts) ->
         if Map.mem nts nt_name then Ok (iid, nts)
         else
           let arity = List.length args + (if Option.is_some _pat then 1 else 0) in
           let new_nt = NonTerminal.mk nt_name arity in
           let iid' = if iid < 0 then new_nt.id else iid in
           Ok (iid', Map.add_exn nts ~key:nt_name ~data:new_nt))
    in
    (List.fold ~init:(Ok (-1, Map.empty (module String))) ~f rules) >>=!
    (fun x -> Ok(x, rules))
  in
  let parse_terms ((iid, nts), rules) : pmrs xresult =
    let f (nt_name, vargs, pat, t) =
      let nt : non_terminal option = Map.find nts nt_name in
      let vars lv =
        let f sv =
          match sv with
          | Sexp.Atom vname ->  Ok(vname, Variable.mk vname)
          | _ -> Error ("Arguments can only be fresh variable names", sv)
        in
        Result.combine_errors (List.map ~f lv) >>=!
        (fun l ->
           (match Map.of_alist (module String) l with
            | `Ok m -> Ok (m, List.map ~f:snd l)
            | `Duplicate_key _ -> Error ["Argument names must be unique", Sexp.List lv]))
      in
      let vargs : (variables * variable list) xresult = vars vargs in
      let pat' : (variables * pattern option) xresult =
        match pat with
        | Some (Sexp.List ((Sexp.Atom constr)::pargs)) ->
          (match Map.find sigma constr with
           | Some cstr_t ->
             let%bind vm, vs = vars pargs in
             Ok (vm, Some (cstr_t, vs))
           | None -> Error ["Could not find constructor", Sexp.Atom constr])

        | Some sp -> Error ["Wrong format for pattern matching", sp]

        | None -> Ok (Map.empty (module String), None)
      in
      match nt, vargs, pat' with
      | Some ntm, Ok (m1, args), Ok(m2, pat)->
        let merge_vars ~key:_ =
          function
          | `Both (v1, _) -> Some v1
          | `Left v -> Some v
          | `Right v -> Some v
        in
        let vars = Map.merge ~f:merge_vars m1 m2 in
        let%bind trm =  term_of_sexp nts sigma vars t in
        Ok (ntm, args, pat,  trm)

      | _ ->
        Error ((match nt with
            | None -> ["Nonterminal name not found", Sexp.Atom nt_name]
            | _ -> []) @
               (match vargs with
                | Error errs -> errs | _ -> []) @
               (match pat' with
                | Error errs -> errs | _ -> []))
    in
    let%bind rules = blast (List.map ~f rules) in
    let rules_mapped =
      let f m (nt, x, y, z : rewrite_rule) =
        Map.update m nt.id
          ~f:(function
              | None -> [(nt, x, y, z)]
              | Some l -> (nt, x, y, z)::l)
      in List.fold ~f ~init:(Map.empty (module Int)) rules
    in
    let nonterminals = List.map ~f:snd (Map.to_alist nts) in
    let rec_scheme = {main_id = iid; rules = rules_mapped; non_terminals = nonterminals; order = -1 } in
    Ok(update_order rec_scheme)
  in rules >>=! gather_nonterms >>=! parse_terms


let term_of_terminal_tree
    (sigma : terminal Map.M(String).t)
    (t : Terminal.t tree) =
  let rec f t =
    match t with
    | Nil -> Error ["Incomplete term", t]
    | Cont -> Error ["Continuation", t]
    | Node(a, args) ->
      let%bind appf =
        match Map.find sigma a.Terminal.name with
        | Some trl -> Ok (TTerm trl)
        | None -> Error ["Unrecognized symbol", t]
      in
      let%bind app_args =
        match args with
        | [Nil] -> Ok []
        | _ ->  blast (List.map ~f args)
      in
      if List.length app_args > 0 then
        Ok (TApp (appf, app_args))
      else Ok appf
  in f t


(* ============================================================================================= *)
(*                                 GRAMMARS                                                      *)
(* ============================================================================================= *)



(* ============================================================================================= *)
(*                                 EVALUATING                                                    *)
(* ============================================================================================= *)
let apply_substs (t : term) (bindings : (int, term) List.Assoc.t) : term =
  let rec replace t =
    match t with
    | TApp(t1, tl) -> TApp(replace t1, List.map ~f:replace tl)
    | TVar v ->
      (match List.Assoc.find bindings ~equal v.id with
       | Some term -> term
       | None -> TVar v )
    | _ -> t
  in replace t

let match_pattern (c : terminal) (vargs : variable list) (t : term) : ((int * term) list) sresult =
  match t with
  | TApp(TTerm c', targs) when c'.id = c.id ->
    (match List.map2 ~f:(fun t v -> (v.id, t)) targs vargs with
     | Ok l -> Ok l
     | Unequal_lengths -> Error ["Pattern does not match (args).", t])

  | TTerm c' when c'.id = c.id -> Ok []

  | _ -> Error ["Pattern does not match.", t]

let rec eval_term_worker (rs : pmrs) (t : term) : term sresult =
  match t with
  | TApp (TNTerm f, args) ->
    let%bind args = blast (List.map ~f:(eval_term_worker rs) args) in
    (match find_and_apply_rule rs f args with
     | Some (Ok t) -> eval_term_worker rs t
     | Some (Error s) -> Error s
     | None -> Error ["Failed reduction", t])

  | TApp(TTerm t0, args) ->
    let%bind args' = blast (List.map ~f:(eval_term_worker rs) args) in
    Ok (TApp(TTerm t0, args'))

  | _ -> Ok t

and find_and_apply_rule (p : pmrs) (f : non_terminal) (args : term list) : (term sresult) option =
  let rules_with_f =
    match Map.find p.rules f.id with
    | Some rules -> rules
    | None -> []
  in
  let rules_applied =
    List.filter ~f:Result.is_ok
      (List.map ~f:(fun rwr -> apply_rule rwr args) rules_with_f)
  in match rules_applied with
  | [] -> None
  | hd :: _ -> Some hd

and apply_rule (_, args, p, t : rewrite_rule) (terms : term list) : term sresult =
  let rec fargs bindings l terms =
    match l, terms with
    | [], _ ->
      (match p, terms with
       | Some (pn, pargs), [tp] ->
         let%bind x = match_pattern pn pargs tp in
         let%bind okb = bindings in
         Ok(okb @ x)
       | None, [] -> bindings
       | _ -> Error ["Missing argument.", t])

    | hd_v :: tl, hd_t :: tl_terms ->
      let%bind okb = bindings in
      fargs (Ok (okb @ [hd_v.id, hd_t])) tl tl_terms

    | _, _ -> Error ["Number of arguments does not match", t]
  in
  fargs (Ok []) args terms >>!| apply_substs t

(* ============================================================================================= *)
(*                                 CONSTRUCTING AN ABSTRACTION                                   *)
(* ============================================================================================= *)

type binding = variable * term
type bindings = (term list) Map.M(Int).t


(* ============================================================================================= *)
(*                                 EVALUATING : MAIN ENTRY POINT                                 *)
(* ============================================================================================= *)
let reduce_term (t : term) ~(with_pmrs : pmrs) : term =
  match
    List.find ~f:(fun nt -> nt.id = with_pmrs.main_id)
      with_pmrs.non_terminals
  with
  | Some maint ->
    (match eval_term_worker with_pmrs (TApp (TNTerm maint, [t])) with
     | Ok t -> t
     | Error errors ->
       List.iter
         ~f:(fun (s, t) -> Fmt.(pf stderr "ERROR %s for %a@." s (box pp_term) t)) errors;
       failwith "Failed to eval term.")

  | None -> failwith "Non Main symbol in PMRS."