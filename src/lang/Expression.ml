(** An expression is a term without let-bindings or function values.  *)

open Base
open EProps
open Term
open Utils
open Option.Let_syntax

let box_id : int ref = ref 0

let new_box_id () =
  let i = !box_id in
  Int.incr box_id;
  i
;;

type boxkind =
  | Indexed of int
  | Typed of RType.t
  | Position of int
[@@deriving sexp, hash]

type t =
  | ETrue
  | EFalse
  | EApp of int * t list
  | EInt of int
  | EChar of char
  | EEmptySet of RType.t
  | EVar of int
  | EBox of boxkind
  | ETup of t list
  | ESel of t * int
  | EIte of t * t * t
  | EData of string * t list
  | EOp of Operator.t * t list
[@@deriving sexp, hash]

let pp_ivar ~(ctx : RContext.t) (f : Formatter.t) (vid : int) : unit =
  Fmt.(
    match RContext.get_var ctx vid with
    | Some v ->
      pf f "%a" (styled (`Fg `Green) (styled `Italic (Variable.pp ctx.parent))) v
    | None -> pf f "?%a" (styled (`Fg `Green) (styled `Italic int)) vid)
;;

let pp_ivarset ~(ctx : RContext.t) (f : Formatter.t) (vis : IS.t) : unit =
  Fmt.(braces (list ~sep:comma (pp_ivar ~ctx)) f (Set.elements vis))
;;

let rec pp ?(ctx = RContext.empty ()) (f : Formatter.t) (expr : t) : unit =
  Fmt.(
    match expr with
    | ETrue -> pf f "#t"
    | EFalse -> pf f "#f"
    | EApp (fid, args) ->
      pf f "%a(%a)" (pp_ivar ~ctx) fid (list ~sep:comma (pp ~ctx)) args
    | EEmptySet _ -> pf f "{}"
    | EInt i -> pf f "%i" i
    | EChar c -> pf f "%c" c
    | EVar i -> pp_ivar ~ctx f i
    | EBox kind ->
      (match kind with
      | Indexed i -> pf f ":%a" (styled `Faint int) i
      | Typed t -> pf f ":%a" (styled `Faint RType.pp) t
      | Position i -> pf f "@%a" (styled `Faint int) i)
    | ETup tl -> pf f "@[(%a)@]" (list ~sep:comma (pp ~ctx)) tl
    | ESel (t, i) -> pf f "@[(%a).%i@]" (pp ~ctx) t i
    | EIte (a, b, c) ->
      pf
        f
        "@[(%a@;%a %a@ %a@ %a %a)@]"
        (styled `Faint (styled `Bold string))
        "if"
        (pp ~ctx)
        a
        (styled `Faint (styled `Bold string))
        "then"
        (pp ~ctx)
        b
        (styled `Faint (styled `Bold string))
        "else"
        (pp ~ctx)
        c
    | EData (c, tl) -> pf f "@[%s(%a)@]" c (list ~sep:comma (pp ~ctx)) tl
    | EOp (op, args) ->
      (match args with
      | [ a ] -> pf f "@[(%a %a)@]" Operator.pp op (pp ~ctx) a
      | [ a; b ] ->
        (match op with
        | Binary Max | Binary Min ->
          pf
            f
            "@[%a(%a, %a)@]"
            (styled (`Fg `Yellow) Operator.pp)
            op
            (pp ~ctx)
            a
            (pp ~ctx)
            b
        | _ ->
          pf
            f
            "@[(%a %a@ %a)@]"
            (pp ~ctx)
            a
            (styled (`Fg `Yellow) Operator.pp)
            op
            (pp ~ctx)
            b)
      | _ ->
        pf
          f
          "@[(%a %a)@]"
          (styled (`Fg `Yellow) Operator.pp)
          op
          (list ~sep:sp (pp ~ctx))
          args))
;;

(* Simple equality *)

let equal : t -> t -> bool = Poly.equal

(* Expression construction. *)
let mk_e_true = ETrue
let mk_e_false = EFalse
let mk_e_int i = EInt i
let mk_e_char i = EChar i
let mk_e_bool b = if b then mk_e_true else mk_e_false
let mk_e_emptyset t = EEmptySet t
let mk_e_var id = EVar id
let mk_e_tup tl = ETup tl
let mk_e_sel t i = ESel (t, i)

let mk_e_assoc op l =
  match l with
  | [] -> failwith "Creation of operation with empty args"
  | [ a ] ->
    (match op with
    | Operator.Unary _ -> EOp (op, l)
    | _ -> a)
  | _ -> EOp (op, l)
;;

let mk_e_ite a b c = EIte (a, b, c)
let mk_e_data c l = EData (c, l)
let mk_e_app i l = EApp (i, l)
let mk_e_bin b e1 e2 = EOp (Binary b, [ e1; e2 ])
let mk_e_un u e = EOp (Unary u, [ e ])

module Op = struct
  let ( + ) = mk_e_bin Binop.Plus
  let ( - ) = mk_e_bin Binop.Minus
  let ( * ) = mk_e_bin Binop.Times
  let ( / ) = mk_e_bin Binop.Div
  let ( && ) = mk_e_bin Binop.And
  let ( || ) = mk_e_bin Binop.Or
  let ( > ) = mk_e_bin Binop.Gt
  let ( < ) = mk_e_bin Binop.Lt
  let ( >= ) = mk_e_bin Binop.Ge
  let ( <= ) = mk_e_bin Binop.Le
  let ( => ) = mk_e_bin Binop.Implies
  let ( <: ) c a = mk_e_data c a
  let ( @: ) a i = mk_e_sel a i
  let max = mk_e_bin Binop.Max
  let min = mk_e_bin Binop.Min
  let not e1 = mk_e_un Unop.Not e1
  let int i = mk_e_int i
  let var i = mk_e_var i
  let ( ~? ) a = mk_e_ite a
end

(* Expression reduction / transformation. *)

let reduce
    ~(case : (t -> 'a) -> t -> 'a option)
    ~(join : 'a -> 'a -> 'a)
    ~(init : 'a)
    (e : t)
  =
  let rec aux e =
    match case aux e with
    | Some c -> c
    | None ->
      (match e with
      | ETrue | EFalse | EChar _ | EInt _ | EEmptySet _ | EVar _ | EBox _ -> init
      | EIte (a, b, c) -> join (aux a) (join (aux b) (aux c))
      | ESel (t, _) -> aux t
      | EApp (_, tl) | EOp (_, tl) | EData (_, tl) | ETup tl ->
        List.fold ~init ~f:(fun a e' -> join (aux e') a) tl)
  in
  aux e
;;

let transform (case : (t -> t) -> t -> t option) (e : t) =
  let rec aux e =
    match case aux e with
    | Some c -> c
    | None ->
      (match e with
      | ETrue | EFalse | EChar _ | EInt _ | EEmptySet _ | EVar _ | EBox _ -> e
      | EIte (a, b, c) -> mk_e_ite (aux a) (aux b) (aux c)
      | EOp (op, tl) -> mk_e_assoc op (List.map ~f:aux tl)
      | ESel (t, i) -> mk_e_sel (aux t) i
      | EApp (i, args) -> mk_e_app i (List.map ~f:aux args)
      | EData (c, tl) -> mk_e_data c (List.map ~f:aux tl)
      | ETup tl -> mk_e_tup (List.map ~f:aux tl))
  in
  aux e
;;

let sub_vars (subs : int IntMap.t) =
  let aux f e =
    match e with
    | EVar i ->
      Some
        (match Map.find subs i with
        | Some j -> EVar j
        | None -> EVar i)
    | EApp (i, l) ->
      Some
        (match Map.find subs i with
        | Some j -> EApp (j, List.map ~f l)
        | None -> EApp (i, List.map ~f l))
    | _ -> None
  in
  transform aux
;;

let rewrite_until_stable (rule : t -> t) (e : t) : t =
  let rec aux i e0 =
    if i > !Config.Optims.rewrite_limit
    then e0
    else (
      let e0' = rule e0 in
      if equal e0' e0 then e0 else aux (i + 1) e0')
  in
  aux 0 e
;;

(*  *)
let size (e : t) : int = reduce ~case:(fun _ _ -> None) ~init:1 ~join:( + ) e
let size_compare (e1 : t) (e2 : t) = compare (size e1) (size e2)

let compare a b =
  let rec aux a b =
    match a, b with
    | EFalse, EFalse | ETrue, ETrue -> 0
    | EFalse, ETrue -> -1
    | ETrue, EFalse -> 1
    | EInt a, EInt b | EVar a, EVar b -> compare a b
    | EBox a, EBox b ->
      (match a, b with
      | Position i, Position j -> compare i j
      | Indexed i, Indexed j -> compare i j
      | Typed t, Typed t' -> if RType.t_equals t t' then 0 else Poly.compare t t'
      | _, _ -> Poly.compare a b)
    | EIte (c, a, b), EIte (c', a', b') -> List.compare aux [ c; a; b ] [ c'; a'; b' ]
    | EData (c, args), EData (c', args') ->
      let z = String.compare c c' in
      if z = 0 then List.compare aux args args' else z
    | EOp (op, args), EOp (op', args') ->
      let z = Operator.compare op op' in
      if z = 0
      then
        if is_commutative op
        then List.compare aux (List.sort ~compare:aux args) (List.sort ~compare:aux args')
        else List.compare aux args args'
      else z
    | ETup tl, ETup tl' -> List.compare aux tl tl'
    | _ ->
      let z = size_compare a b in
      if z = 0 then Poly.compare a b else z
  in
  aux a b
;;

let equal a b = compare a b = 0

include (val Comparator.make ~compare ~sexp_of_t)

let free_variables (e : t) =
  IS.(
    reduce
      ~join:( + )
      ~init:empty
      ~case:(fun _ e ->
        match e with
        | EVar i -> Some ~$i
        | _ -> None)
      e)
;;

(** [alpha_equal e1 e2] returns true if [equal e1 e2] up to variable renaming. *)
let alpha_equal (e1 : t) (e2 : t) : bool =
  let _ = e1, e2 in
  failwith "TODO: Not implemented."
;;

let of_term ?(ctx = RContext.empty ()) t0 : t option =
  let rec f t =
    match t.tkind with
    | TBox t -> f t
    | TConst c ->
      Constant.(
        (match c with
        | CInt i -> mk_e_int i
        | CChar c -> mk_e_char c
        | CTrue -> mk_e_true
        | CFalse -> mk_e_false
        | CEmptySet t -> mk_e_emptyset t))
    | TVar v ->
      RContext.register_var ctx v;
      mk_e_var v.vid
    | TData (c, tl) -> mk_e_data c (List.map ~f tl)
    | TTup tl -> mk_e_tup (List.map ~f tl)
    | TIte (c, tt, tf) -> mk_e_ite (f c) (f tt) (f tf)
    | TBin (Or, { tkind = TUn (Not, a); _ }, b) -> mk_e_bin Implies (f a) (f b)
    | TBin (op, t1, t2) ->
      Binop.(
        (match op with
        | Plus -> mk_e_assoc (Binary Plus) [ f t1; f t2 ]
        | Times -> mk_e_assoc (Binary Times) [ f t1; f t2 ]
        | Div -> mk_e_assoc (Binary Div) [ f t2; f t1 ]
        | And -> mk_e_assoc (Binary And) [ f t1; f t2 ]
        | Or -> mk_e_assoc (Binary Or) [ f t1; f t2 ]
        | Min -> mk_e_assoc (Binary Min) [ f t1; f t2 ]
        | Max -> mk_e_assoc (Binary Max) [ f t1; f t2 ]
        | Gt -> mk_e_bin Gt (f t1) (f t2)
        | Ge -> mk_e_bin Ge (f t1) (f t2)
        | Lt -> mk_e_bin Gt (f t2) (f t1)
        | Le -> mk_e_bin Ge (f t2) (f t1)
        | Eq -> mk_e_bin Eq (f t1) (f t2)
        | Mod -> mk_e_bin Mod (f t1) (f t2)
        | Minus -> mk_e_assoc (Binary Plus) [ f t1; mk_e_un Neg (f t2) ]
        | SetIntersection typ -> mk_e_assoc (Binary (SetIntersection typ)) [ f t1; f t2 ]
        | SetUnion typ -> mk_e_assoc (Binary (SetUnion typ)) [ f t1; f t2 ]
        | SetMinus typ -> mk_e_bin (SetMinus typ) (f t1) (f t2)
        | SetInsert typ -> mk_e_bin (SetInsert typ) (f t1) (f t2)
        | SetMem typ -> mk_e_bin (SetMem typ) (f t1) (f t2)
        | SetSubset typ -> mk_e_bin (SetSubset typ) (f t1) (f t2)
        | Implies -> failwith "=> is only for expressions"))
    | TUn (op, t) ->
      Unop.(
        (match op with
        | Neg -> mk_e_un Neg (f t)
        | Not -> mk_e_un Not (f t)
        | Abs -> mk_e_un Abs (f t)
        | SetCard typ -> mk_e_un (SetCard typ) (f t)
        | SetSingleton typ -> mk_e_un (SetSingleton typ) (f t)
        | SetComplement typ -> mk_e_un (SetComplement typ) (f t)))
    | TSel (t, i) -> mk_e_sel (f t) i
    | TApp (func, args) ->
      (match f func with
      | EVar i -> mk_e_app i (List.map ~f args)
      | _ -> raise_s (Sexp.Atom "Expressions only for fully reduced terms."))
    | TMatch _ | TFun _ -> raise_s (Sexp.Atom "Expressions only for fully reduced terms.")
  in
  try Some (f t0) with
  | _ -> None
;;

let to_term ~(ctx : RContext.t) (e : t) : term option =
  let rec f e =
    match e with
    | ETrue -> Some (Terms.bool true)
    | EFalse -> Some (Terms.bool false)
    | EInt i -> Some (Terms.int i)
    | EChar c -> Some (Terms.char c)
    | EEmptySet t -> Some (Terms.emptyset t)
    | EApp (i, l) ->
      let%bind func = f (EVar i) in
      let%map args = Option.all (List.map ~f l) in
      mk_app func args
    | EVar i ->
      let%map v = RContext.get_var ctx i in
      mk_var ctx.parent v
    | EBox kind ->
      (match kind with
      | Indexed i ->
        let%map v = RContext.get_var ctx i in
        mk_var ctx.parent v
      | _ -> None)
    | ETup tl -> Option.map ~f:(mk_tup ctx.parent) (Option.all (List.map ~f tl))
    | ESel (t, i) -> Option.map ~f:(fun t' -> mk_sel ctx.parent t' i) (f t)
    | EIte (c, tt, tf) ->
      let%map c' = f c
      and tt' = f tt
      and tf' = f tf in
      mk_ite c' tt' tf'
    | EData (c, tl) -> Option.map ~f:(mk_data ctx.parent c) (Option.all (List.map ~f tl))
    | EOp (op, tl) ->
      (match tl, op with
      | [ a ], Unary op ->
        let%map a' = f a in
        mk_un op a'
      | [ a; b ], Binary op ->
        (match op with
        | Implies ->
          let%map a' = f a
          and b' = f b in
          mk_bin Or (mk_un Not a') b'
        | _ ->
          let%map a' = f a
          and b' = f b in
          mk_bin op a' b')
      | _ ->
        Binop.(
          (match op with
          | Binary Plus ->
            Option.bind ~f:(mk_assoc_with_id Binop.Plus) (Option.all (List.map ~f tl))
          | Binary Times ->
            Option.bind ~f:(mk_assoc_with_id Binop.Times) (Option.all (List.map ~f tl))
          | Binary Max ->
            Option.bind ~f:(mk_assoc_with_id Binop.Max) (Option.all (List.map ~f tl))
          | Binary Min ->
            Option.bind ~f:(mk_assoc_with_id Binop.Min) (Option.all (List.map ~f tl))
          | Binary And ->
            Option.bind ~f:(mk_assoc_with_id Binop.And) (Option.all (List.map ~f tl))
          | Binary Or ->
            Option.bind ~f:(mk_assoc_with_id Binop.Or) (Option.all (List.map ~f tl))
          | _ -> None)))
  in
  f e
;;

(**
  `matches x y` checks if `y` matches expression `x`.  If it does, returns `Some s` where
   `s` is a map from `y`'s variables ids to `x`'s. Otherwise, returns `None`.
*)
let matches (e1 : t) (e2 : t) =
  let join l1 l2 =
    match l1, l2 with
    | Some l1, Some l2 ->
      (try
         Some
           (List.fold l2 ~init:l1 ~f:(fun acc (vid, vid') ->
                match List.Assoc.find ~equal:Int.equal acc vid with
                | Some id2' -> if Int.(not (id2' = vid')) then failwith "Dup" else acc
                | None -> (vid, vid') :: acc))
       with
      | _ -> None)
    | _ -> None
  in
  let rec aux e1 e2 =
    match e1, e2 with
    | _ when equal e1 e2 -> Some []
    | EVar i1, EVar i2 -> Some [ i2, i1 ]
    | ETup args1, ETup args2 -> aux_l args1 args2
    | ESel (e1', i1), ESel (e2', i2) -> if Int.equal i1 i2 then aux e1' e2' else None
    | EIte (a1, b1, c1), EIte (a2, b2, c2) ->
      join (join (aux a1 a2) (aux b1 b2)) (aux c1 c2)
    | EData (c1, a1), EData (c2, a2) -> if String.equal c1 c2 then aux_l a1 a2 else None
    | EOp (op1, args1), EOp (op2, args2) ->
      if Operator.equal op1 op2 then aux_l args1 args2 else None
    | _ -> None
  and aux_l l1 l2 =
    match List.zip l1 l2 with
    | Unequal_lengths -> None
    | Ok l ->
      List.fold l ~init:(Some []) ~f:(fun acc (e1', e2') -> join acc (aux e1' e2'))
  in
  aux e1 e2
;;

(* ============================================================================================= *)
(*                           REWRITING AND SIMPLIFICATION                                        *)
(* ============================================================================================= *)

let has_impl a tl =
  List.exists
    ~f:(function
      | EOp (Binary Implies, [ EOp (Unary Not, [ a' ]); _ ]) -> equal a' a
      | EOp (Binary Implies, [ a'; _ ]) -> equal a' a
      | _ -> false)
    tl
;;

(* Replace (a => b) in tl by b given that a is true. *)
let simplify_implies a tl =
  EOp
    ( Binary And
    , a
      :: List.map
           ~f:(function
             | EOp (Binary Implies, [ EOp (Unary Not, [ a' ]); _ ]) when equal a' a ->
               mk_e_true
             | EOp (Binary Implies, [ a'; b ]) when equal a' a -> b
             | e -> e)
           tl )
;;

(** Evaluation *)
let simplify (e : t) : t =
  (* Integer evaluation. *)
  let ieval op f args =
    let symbs, maybe_concrete =
      List.fold args ~init:([], None) ~f:(fun (symbs, concrete) arg ->
          match arg with
          | EInt i ->
            (match concrete with
            | Some i' -> symbs, Some (f i i')
            | None -> symbs, Some i)
          | _ -> symbs @ [ arg ], concrete)
    in
    let args' =
      match maybe_concrete with
      | Some i ->
        if is_null_int_const op i then [ mk_e_int i ] else symbs @ [ mk_e_int i ]
      | None -> symbs
    in
    match
      (* Filter out identity elements. *)
      List.filter
        ~f:(fun x ->
          match x with
          | EInt i -> not (is_id_int_const op i)
          | _ -> true)
        args'
    with
    | [] -> [ List.hd_exn args' ]
    | _ as l -> l
  in
  (* Boolean evaluation. *)
  let beval op f args =
    let args = List.dedup_and_sort ~compare args in
    let symbs, maybe_concrete =
      List.fold args ~init:([], None) ~f:(fun (symbs, concrete) arg ->
          match arg with
          | ETrue ->
            (match concrete with
            | Some i' -> symbs, Some (f true i')
            | None -> symbs, Some true)
          | EFalse ->
            (match concrete with
            | Some i' -> symbs, Some (f false i')
            | None -> symbs, Some false)
          | _ -> symbs @ [ arg ], concrete)
    in
    let args' =
      match maybe_concrete with
      | Some b ->
        if is_null_bool_const op b then [ mk_e_bool b ] else symbs @ [ mk_e_bool b ]
      | None -> symbs
    in
    match
      List.filter
        ~f:(fun x ->
          match x with
          | ETrue -> is_and op
          | EFalse -> is_or op
          | _ -> true)
        args'
    with
    | [] -> [ List.hd_exn args' ]
    | _ as l -> l
  in
  let tr_peval f e =
    match e with
    | EIte (c, a, b) ->
      Some
        (match f c with
        | ETrue -> f a
        | EFalse -> f b
        | c' -> EIte (c', f a, f b))
    | EOp (op, args) ->
      (match List.map ~f args with
      | [ x ] -> Some (mk_e_assoc op [ x ])
      | args ->
        (match op with
        | Binary Plus -> Some (mk_e_assoc op (ieval Binop.Plus ( + ) args))
        | Binary Times -> Some (mk_e_assoc op (ieval Binop.Times ( * ) args))
        | Binary Max -> Some (mk_e_assoc op (ieval Binop.Max max args))
        | Binary Min -> Some (mk_e_assoc op (ieval Binop.Min min args))
        | Binary And -> Some (mk_e_assoc op (beval Binop.And ( && ) args))
        | Binary Or -> Some (mk_e_assoc op (beval Binop.Or ( || ) args))
        | _ -> Some (mk_e_assoc op args)))
    | _ -> None
  in
  let tr_prew _ e =
    match e with
    (* and and or *)
    | EOp (Binary And, ETrue :: rest) ->
      (match rest with
      | [] -> Some ETrue
      | _ -> Some (mk_e_assoc (Binary And) rest))
      (* Simple And rules *)
    | EOp (Binary And, EFalse :: _) -> Some EFalse (* a && a => b -> a && b *)
    | EOp
        ( Binary And
        , EOp (Binary Implies, [ a; b ]) :: EOp (Binary Implies, [ a'; b' ]) :: tl )
      when equal a a' ->
      Some (mk_e_assoc (Binary And) (mk_e_bin Implies a (mk_e_bin And b b') :: tl))
    | EOp (Binary And, a :: EOp (Binary Implies, [ a'; b ]) :: tl) when equal a a' ->
      Some (mk_e_assoc (Binary And) (a :: b :: tl))
    | EOp (Binary And, EOp (Binary Implies, [ a; b ]) :: tl) ->
      if List.mem tl a ~equal then Some (EOp (Binary And, b :: tl)) else None
    | EOp (Binary And, a :: tl) when has_impl a tl -> Some (simplify_implies a tl)
    (* Simple or Rules *)
    | EOp (Binary Or, EFalse :: rest) ->
      (match rest with
      | [] -> Some EFalse
      | _ -> Some (mk_e_assoc (Binary Or) rest))
    | EOp (Binary Or, ETrue :: _) -> Some ETrue
    (* - (i) -> (-i) *)
    | EOp (Unary Neg, [ EInt x ]) -> Some (EInt (-x))
    | EOp (Binary Minus, x :: EInt 0 :: rest) ->
      (match rest with
      | [] -> Some x
      | _ -> Some (EOp (Binary Minus, x :: rest)))
    (* not (not x) -> x *)
    | EOp (Unary Not, [ EOp (Unary Not, [ x ]) ]) -> Some x
    (* max (abs x) 0 -> abs x *)
    | EOp (Binary Max, [ EOp (Unary Abs, [ x ]); EInt 0 ])
    | EOp (Binary Max, [ EInt 0; EOp (Unary Abs, [ x ]) ]) ->
      Some (EOp (Unary Abs, [ x ]))
    (* min (abs x) 0 -> 0 *)
    | EOp (Binary Min, [ EOp (Unary Abs, [ _ ]); EInt 0 ])
    | EOp (Binary Min, [ EInt 0; EOp (Unary Abs, [ _ ]) ]) -> Some (EInt 0)
    (* If-then-elses *)
    | EIte (_, b, c) when equal b c -> Some b
    (* If to min / max *)
    (* a < b ? a : b -> min (a,b) *)
    | EIte (EOp (Binary (Lt | Le), [ a; b ]), a', b') when equal a a' && equal b b' ->
      Some (EOp (Binary Min, [ a; b ]))
    (* a > b ? b : a -> min (a,b) *)
    | EIte (EOp (Binary (Gt | Ge), [ a; b ]), a', b') when equal a b' && equal b a' ->
      Some (EOp (Binary Min, [ a; b ]))
    (* a > b ? a : b -> max (a,b) *)
    | EIte (EOp (Binary (Gt | Ge), [ a; b ]), a', b') when equal a a' && equal b b' ->
      Some (EOp (Binary Max, [ a; b ]))
    (* a < b ? b : a -> max (a,b) *)
    | EIte (EOp (Binary (Lt | Le), [ a; b ]), a', b') when equal a b' && equal b a' ->
      Some (EOp (Binary Max, [ a; b ]))
    (* minmax *)
    | EIte (EOp (Binary (Gt | Ge), [ a; b ]), b', EOp (Binary Max, [ a'; a'' ]))
      when (equal a a' || equal a a'') && equal b b' ->
      Some (mk_e_bin Min b (mk_e_bin Max a a''))
    | EIte (EOp (Binary (Gt | Ge), [ a; b ]), a', EOp (Binary Min, [ b'; b'' ]))
      when (equal b b'' || equal b b') && equal a a' ->
      Some (mk_e_bin Max a (mk_e_bin Min b b''))
    | EIte (EOp (Binary (Lt | Le), [ a; b ]), b', EOp (Binary Min, [ a'; a'' ]))
      when (equal a a' || equal a a'') && equal b b' ->
      Some (mk_e_bin Max b (mk_e_bin Min a a''))
    | EIte (EOp (Binary (Lt | Le), [ a; b ]), a', EOp (Binary Max, [ b'; b'' ]))
      when (equal a a' || equal b b') && equal b b' ->
      Some (mk_e_bin Min a (mk_e_bin Max b b''))
    | EIte
        ( EOp (Binary (Gt | Ge), [ a; b ])
        , EOp (Binary Min, [ a'; EOp (Binary Max, [ b'; b'' ]) ])
        , EOp (Binary Min, [ b_'; EOp (Binary Max, [ a_'; a_'' ]) ]) )
      when equal a a'
           && equal b b_'
           && (equal a a_'' || equal a a_')
           && (equal b b' || equal b b'') ->
      if not (equal b b'')
      then Some (mk_e_bin Min (mk_e_bin Max a b) (mk_e_bin Max a b''))
      else Some (mk_e_bin Min (mk_e_bin Max a b) (mk_e_bin Max a b'))
    | EIte
        ( EOp (Binary (Gt | Ge), [ a; b ])
        , EOp (Binary Max, [ b_'; EOp (Binary Min, [ a_'; a_'' ]) ])
        , EOp (Binary Max, [ a'; EOp (Binary Min, [ b'; b'' ]) ]) )
      when equal a a'
           && equal b b_'
           && (equal a a_'' || equal a a_')
           && (equal b b' || equal b b'') ->
      if not (equal b b'')
      then Some (mk_e_bin Max (mk_e_bin Min a b) (mk_e_bin Min a b''))
      else Some (mk_e_bin Max (mk_e_bin Min a b) (mk_e_bin Min a b'))
    | EIte
        ( EOp (Binary (Lt | Le), [ a; b ])
        , EOp (Binary Max, [ a'; EOp (Binary Min, [ b'; b'' ]) ])
        , EOp (Binary Max, [ b_'; EOp (Binary Min, [ a_'; a_'' ]) ]) )
      when equal a a'
           && equal b b_'
           && (equal a a_'' || equal a a_')
           && (equal b b' || equal b b'') ->
      if not (equal b b'')
      then Some (mk_e_bin Max (mk_e_bin Min a b) (mk_e_bin Min a b''))
      else Some (mk_e_bin Max (mk_e_bin Min a b) (mk_e_bin Min a b'))
    (* Comparisons *)
    | EOp (Binary Lt, [ EInt a; EInt b ]) -> Some (mk_e_bool (a < b))
    | EOp (Binary Gt, [ EInt a; EInt b ]) -> Some (mk_e_bool (a > b))
    | EOp (Binary Le, [ EInt a; EInt b ]) -> Some (mk_e_bool (a <= b))
    | EOp (Binary Ge, [ EInt a; EInt b ]) -> Some (mk_e_bool (a >= b))
    (* (a ? x : y) > y -> a && x > y *)
    | EOp (Binary Gt, [ EIte (a, x, y); y' ]) when equal y y' -> Some Op.(a && x > y)
    (* (a ? x : y) > x -> !a && y > x *)
    | EOp (Binary Gt, [ EIte (a, x, y); x' ]) when equal x x' ->
      Some Op.((not a) && y > x)
    (* (a ? x : y) < y -> a && x < y *)
    | EOp (Binary Lt, [ EIte (a, x, y); y' ]) when equal y y' -> Some Op.(a && y > x)
    (* (a ? x : y) < x -> !a && y > x *)
    | EOp (Binary Lt, [ EIte (a, x, y); x' ]) when equal x x' ->
      Some Op.((not a) && y > x)
    | _ -> None
  in
  rewrite_until_stable (fun x -> x |> transform tr_prew |> transform tr_peval) e
;;

(** Normalizing *)
let normalize (e : t) : t =
  let coll op args =
    mk_e_assoc
      op
      (List.concat_map args ~f:(fun x ->
           match x with
           | EOp (op', args') -> if Operator.(equal op op') then args' else [ x ]
           | _ -> [ x ]))
  in
  let rule e0 =
    let g f e' =
      match e' with
      | EOp (op, args) ->
        Some
          (coll
             op
             ((if is_commutative op then List.sort ~compare else identity)
                (List.map ~f args)))
      | _ -> None
    in
    simplify (transform g e0)
  in
  rewrite_until_stable rule e
;;

(** Using an expression with boxes like a function. *)
let apply (f : t) (args : t list) =
  let transformer _ = function
    | EBox (Position i) -> List.nth args i
    | _ -> None
  in
  normalize (transform transformer f)
;;

(** Get the identity element of a given operator.*)
let get_id_const (op : Operator.t) : t option =
  match op with
  | Binary Plus | Unary Abs | Unary Neg | Binary Minus -> Some (mk_e_int 0)
  | Binary Times -> Some (mk_e_int 1)
  | Binary Min -> Some (mk_e_int Int.max_value)
  | Binary Max -> Some (mk_e_int Int.min_value)
  | Binary And -> Some mk_e_true
  | Binary Or -> Some mk_e_false
  | _ -> None
;;

(** Get a constant of a given type (implemented fo TInt and TBool). *)
let get_ty_const (typ : RType.t) : t =
  RType.(
    match typ with
    | TInt -> mk_e_int 0
    | TBool -> mk_e_true
    | _ -> mk_e_int 0)
;;

let contains_ebox (e : t) : bool =
  let case _ e =
    match e with
    | EBox _ -> Some true
    | _ -> None
  in
  reduce e ~init:false ~join:( || ) ~case
;;

let count_boxkind (b : boxkind) =
  let case _ e =
    match e with
    | EBox b' -> if Poly.equal b b' then Some 1 else None
    | _ -> None
  in
  reduce ~join:( + ) ~case ~init:0
;;

(** Rewrite an expression so that variables are renamed
from 0 to number of free variables, in the order where they appear
in a reduction traversal.
    *)
let nameless_normal_form (e : t) : t =
  let ordered_vs =
    let join l1 l2 =
      List.fold l2 ~init:l1 ~f:(fun l1' a2 ->
          if List.mem ~equal:Int.equal l1' a2 then l1' else l1' @ [ a2 ])
    in
    let case f e =
      match e with
      | EVar i -> Some [ i ]
      | EApp (i, args) -> Some (List.fold ~f:(fun l a -> join l (f a)) ~init:[ i ] args)
      | _ -> None
    in
    reduce ~case ~join ~init:[] e
  in
  let sub =
    match Map.of_alist (module Int) (List.mapi ordered_vs ~f:(fun i v -> v, i)) with
    | `Duplicate_key _ -> failwith "Unexpected."
    | `Ok m -> m
  in
  sub_vars sub e
;;
