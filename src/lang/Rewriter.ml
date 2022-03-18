open Base
open Term
open Option.Let_syntax
open Utils

let get_id_const (op : Binop.t) : term option =
  match op with
  | Plus -> Some (mk_const (Constant.CInt 0))
  | Times -> Some (mk_const (Constant.CInt 1))
  | Min -> Some (mk_const (Constant.CInt Int.max_value))
  | Max -> Some (mk_const (Constant.CInt Int.min_value))
  | And -> Some (mk_const Constant.CTrue)
  | Or -> Some (mk_const Constant.CFalse)
  | _ -> None
;;

let is_id_int_const (op : Binop.t) (i : int) : bool =
  match op, i with
  | Plus, 0 | Div, 1 | Minus, 0 | Times, 1 -> true
  | _ -> false
;;

let is_null_int_const (op : Binop.t) (i : int) : bool =
  match op, i with
  | Times, 0 -> true
  | _ -> false
;;

let is_null_bool_const (op : Binop.t) (b : bool) : bool =
  match op, b with
  | And, false | Or, true -> true
  | _ -> false
;;

let is_and op =
  Binop.(
    match op with
    | And -> true
    | _ -> false)
;;

let is_or op =
  Binop.(
    match op with
    | Or -> true
    | _ -> false)
;;

let get_ty_const (typ : RType.t) : term =
  RType.(
    match typ with
    | TInt -> mk_const (Constant.of_int 0)
    | TBool -> mk_const (Constant.of_bool true)
    | _ -> mk_const (Constant.of_int 0))
;;

(**
  Left distributive operators: for each pair op1, op2 it means that:
  a op1 (b op2 c) = (a op1 b) op2 (a op1 c)
*)
let left_distrib =
  Map.of_alist_multi
    (module Operator)
    Operator.
      [ Binary Plus, Binary Max (* c + max(a,b) -> max(c+a, c+b) *)
      ; Binary Times, Binary Plus (* c * (a + b) -> c * a + c * b *)
      ; Binary Plus, Binary Min (* c + min(a,b) -> min(c + a, c + b) *)
      ; Binary And, Binary Or (* c && (a || b) -> c && a || c && b *)
      ]
;;

(** For example, `is_left_distrib Plus Max` is true. *)
let is_left_distrib op1 op2 =
  match Map.find left_distrib op1 with
  | Some binops -> Binop.(List.mem binops ~equal op2)
  | None -> false
;;

let get_distributing op =
  Operator.(
    match op with
    | Binary Plus -> Some (Binary Times)
    | Binary (Min | Max) -> Some (Binary Plus)
    | Binary Or -> Some (Binary And)
    | _ -> None)
;;

(**
  Right distributive operators: for each pair op1, op2 it means that:
  (b op2 c) op1 a = (b op1 a) op2 (c op1 a)
*)
let right_distrib =
  Map.of_alist_multi
    (module Operator)
    Binop.
      [ Binary Plus, Max
      ; Binary Times, Plus
      ; Binary Div, Plus
      ; Binary Plus, Min
      ; Binary And, Or
      ]
;;

(** For example, `is_right_distrib Plus Max` is true. *)
let is_right_distrib op1 op2 =
  match Map.find right_distrib (Binary op1) with
  | Some binops -> Binop.(List.mem binops ~equal op2)
  | None -> false
;;

(**
  Associative operators.
*)
let _assoc = OpSet.of_list [ Binary Plus; Binary Times; Binary Min; Binary Max ]

(**
  For example, `is_assoc Plus` is true.
*)
let is_assoc op = Set.mem _assoc (Binary op)

let mk_assoc_with_id (op : Binop.t) (tl : term list) : term option =
  let rec aux t rest =
    match rest with
    | hd :: tl -> aux (mk_bin ~pos:hd.tpos ~typ:(Some hd.ttyp) op t hd) tl
    | [] -> t
  in
  match tl with
  | [] -> get_id_const op
  | [ x ] -> Some x
  | hd :: tl -> Some (aux hd tl)
;;

(**
  Commutative operators.
*)
let _commut =
  OpSet.of_list
    [ Binary Plus
    ; Binary Times
    ; Binary Min
    ; Binary Max
    ; Binary Eq
    ; Binary And
    ; Binary Or
    ]
;;

(**
  For example, `is_commutative Plus` is true.
*)
let is_commutative op = Set.mem _commut op

let concrete_int_op (op : Operator.t) =
  Operator.(
    match op with
    | Binary Plus -> Some ( + )
    | Binary Times -> Some ( * )
    | Binary Minus -> Some ( - )
    | Binary Div -> Some ( / )
    | _ -> None)
;;

(** Set of ints = set of variables. Module is only meant to avoid (Set.... (module Int)) everywhere. *)
module IS = struct
  type t = Set.M(Int).t
  type elt = int

  let empty = Set.empty (module Int)
  let singleton i = Set.singleton (module Int) i
  let of_list l = Set.of_list (module Int) l
  let ( + ) = Set.union
  let ( - ) = Set.diff

  (** Set intersection. *)
  let ( ^ ) = Set.inter

  (** Checks is a set is empty.*)
  let ( ?. ) x = Set.is_empty x

  let ( ~$ ) x = singleton x

  let pp (f : Formatter.t) (s : t) : unit =
    Fmt.(braces (list ~sep:comma int)) f (Set.elements s)
  ;;
end

module RContext = struct
  type t = { vars : (int, variable) Hashtbl.t }

  let create () = { vars = Hashtbl.create (module Int) }
  let register_var (ctx : t) (v : variable) = Hashtbl.set ctx.vars ~key:v.vid ~data:v
  let get_var (ctx : t) (id : int) = Hashtbl.find ctx.vars id
end

(** An expression is a term without let-bindings or function values.  *)
module Expression = struct
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
  [@@deriving sexp]

  type t =
    | ETrue
    | EFalse
    | EInt of int
    | EChar of char
    | EVar of int
    | EBox of boxkind
    | ETup of t list
    | ESel of t * int
    | EIte of t * t * t
    | EData of string * t list
    | EOp of Operator.t * t list
  [@@deriving sexp]

  let pp_ivar ~(ctx : Context.t) ~(rctx : RContext.t) (f : Formatter.t) (vid : int) : unit
    =
    Fmt.(
      match RContext.get_var rctx vid with
      | Some v -> pf f "%a" (styled (`Fg `Green) (styled `Italic (Variable.pp ctx))) v
      | None -> pf f "?%a" (styled (`Fg `Green) (styled `Italic int)) vid)
  ;;

  let pp_ivarset
      ?(ctx = Context.create ())
      ~(rctx : RContext.t)
      (f : Formatter.t)
      (vis : IS.t)
      : unit
    =
    Fmt.(braces (list ~sep:comma (pp_ivar ~rctx ~ctx)) f (Set.elements vis))
  ;;

  let rec pp ?(ctx = Context.create ()) ~(rctx : RContext.t) (f : Formatter.t) (expr : t)
      : unit
    =
    Fmt.(
      match expr with
      | ETrue -> pf f "#t"
      | EFalse -> pf f "#f"
      | EInt i -> pf f "%i" i
      | EChar c -> pf f "%c" c
      | EVar i -> pp_ivar ~rctx ~ctx f i
      | EBox kind ->
        (match kind with
        | Indexed i -> pf f ":%a" (styled `Faint int) i
        | Typed t -> pf f ":%a" (styled `Faint RType.pp) t
        | Position i -> pf f "@%a" (styled `Faint int) i)
      | ETup tl -> pf f "@[(%a)@]" (list ~sep:comma (pp ~rctx ~ctx)) tl
      | ESel (t, i) -> pf f "@[(%a).%i@]" (pp ~rctx ~ctx) t i
      | EIte (a, b, c) ->
        pf
          f
          "@[(%a@;%a %a@ %a@ %a %a)@]"
          (styled `Faint (styled `Bold string))
          "if"
          (pp ~rctx ~ctx)
          a
          (styled `Faint (styled `Bold string))
          "then"
          (pp ~rctx ~ctx)
          b
          (styled `Faint (styled `Bold string))
          "else"
          (pp ~rctx ~ctx)
          c
      | EData (c, tl) -> pf f "@[%s(%a)@]" c (list ~sep:comma (pp ~rctx ~ctx)) tl
      | EOp (op, args) ->
        (match args with
        | [ a ] -> pf f "@[(%a %a)@]" Operator.pp op (pp ~rctx ~ctx) a
        | [ a; b ] ->
          (match op with
          | Binary Max | Binary Min ->
            pf
              f
              "@[%a(%a, %a)@]"
              (styled (`Fg `Yellow) Operator.pp)
              op
              (pp ~rctx ~ctx)
              a
              (pp ~rctx ~ctx)
              b
          | _ ->
            pf
              f
              "@[(%a %a@ %a)@]"
              (pp ~rctx ~ctx)
              a
              (styled (`Fg `Yellow) Operator.pp)
              op
              (pp ~rctx ~ctx)
              b)
        | _ ->
          pf
            f
            "@[(%a %a)@]"
            (styled (`Fg `Yellow) Operator.pp)
            op
            (list ~sep:sp (pp ~rctx ~ctx))
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
        | ETrue | EFalse | EChar _ | EInt _ | EVar _ | EBox _ -> init
        | EIte (a, b, c) -> join (aux a) (join (aux b) (aux c))
        | ESel (t, _) -> aux t
        | EOp (_, tl) | EData (_, tl) | ETup tl ->
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
        | ETrue | EFalse | EChar _ | EInt _ | EVar _ | EBox _ -> e
        | EIte (a, b, c) -> mk_e_ite (aux a) (aux b) (aux c)
        | EOp (op, tl) -> mk_e_assoc op (List.map ~f:aux tl)
        | ESel (t, i) -> mk_e_sel (aux t) i
        | EData (c, tl) -> mk_e_data c (List.map ~f:aux tl)
        | ETup tl -> mk_e_tup (List.map ~f:aux tl))
    in
    aux e
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
          then
            List.compare aux (List.sort ~compare:aux args) (List.sort ~compare:aux args')
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

  let of_term ~(rctx : RContext.t) t0 : t option =
    let rec f t =
      match t.tkind with
      | TBox t -> f t
      | TConst c ->
        Constant.(
          (match c with
          | CInt i -> mk_e_int i
          | CChar c -> mk_e_char c
          | CTrue -> mk_e_true
          | CFalse -> mk_e_false))
      | TVar v ->
        RContext.register_var rctx v;
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
          | Implies -> failwith "=> is only for expressions"))
      | TUn (op, t) ->
        Unop.(
          (match op with
          | Neg -> mk_e_un Neg (f t)
          | Not -> mk_e_un Not (f t)
          | Abs -> mk_e_un Abs (f t)))
      | TSel (t, i) -> mk_e_sel (f t) i
      | TMatch _ | TApp _ | TFun _ ->
        raise_s (Sexp.Atom "Expressions only for fully reduced terms.")
    in
    try Some (f t0) with
    | _ -> None
  ;;

  let to_term ~(ctx : Context.t) ~(rctx : RContext.t) e : term option =
    let rec f e =
      match e with
      | ETrue -> Some (Terms.bool true)
      | EFalse -> Some (Terms.bool false)
      | EInt i -> Some (Terms.int i)
      | EChar c -> Some (Terms.char c)
      | EVar i ->
        let%map v = RContext.get_var rctx i in
        mk_var ctx v
      | EBox kind ->
        (match kind with
        | Indexed i ->
          let%map v = RContext.get_var rctx i in
          mk_var ctx v
        | _ -> None)
      | ETup tl -> Option.map ~f:(mk_tup ctx) (Option.all (List.map ~f tl))
      | ESel (t, i) -> Option.map ~f:(fun t' -> mk_sel ctx t' i) (f t)
      | EIte (c, tt, tf) ->
        let%map c' = f c
        and tt' = f tt
        and tf' = f tf in
        mk_ite c' tt' tf'
      | EData (c, tl) -> Option.map ~f:(mk_data ctx c) (Option.all (List.map ~f tl))
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
end

module Skeleton = struct
  open Option.Let_syntax

  (** A type to represent a grammar guess.  *)
  type t =
    | SChoice of t list (** A choice of possible guesses. *)
    | SBin of Binop.t * t * t (** A binary expression.  *)
    | SUn of Unop.t * t (** A unary expression.  *)
    | SIte of t * t * t (** An if-then-else.  *)
    | SType of RType.t
        (** A guess of some type (to be filled with the appropriate non-terminal).  *)
    | STypedWith of RType.t * t list
    | SArg of int (** A direct reference to a function argument.  *)
    | STuple of t list
    | SNonGuessable

  let rec pp (frmt : Formatter.t) (sk : t) : unit =
    match sk with
    | SChoice l -> Fmt.(pf frmt "(%a)" (list ~sep:vbar pp) l)
    | SBin (op, a, b) -> Fmt.(pf frmt "(%a %a %a)" Binop.pp op (box pp) a (box pp) b)
    | SUn (op, a) -> Fmt.(pf frmt "(%a %a)" Unop.pp op (box pp) a)
    | SIte (a, b, c) -> Fmt.(pf frmt "(ite@;%a@;%a@;%a)" (box pp) a (box pp) b (box pp) c)
    | SType t -> Fmt.(pf frmt "<%a?>" RType.pp t)
    | STypedWith (t, choices) ->
      Fmt.(pf frmt "<%a?>(%a)" RType.pp t (box (list ~sep:sp pp)) choices)
    | SArg i -> Fmt.(pf frmt "@%i" i)
    | STuple tl -> Fmt.(pf frmt "(tuple %a)" (box (list ~sep:sp pp)) tl)
    | SNonGuessable -> Fmt.(pf frmt "!?")
  ;;

  let rec of_expression ~(rctx : RContext.t) : Expression.t -> t option = function
    | EInt _ -> Some (SType RType.TInt)
    | EChar _ -> Some (SType RType.TChar)
    | EFalse | ETrue -> Some (SType RType.TBool)
    | EVar v ->
      Option.(RContext.get_var rctx v >>= Variable.vtype >>= fun t -> Some (SType t))
    | EBox boxkind ->
      (match boxkind with
      | Expression.Position i -> Some (SArg i)
      | _ -> None)
    | ETup tl ->
      let%map tl = all_or_none (List.map ~f:(of_expression ~rctx) tl) in
      STuple tl
    | EIte (a, b, c) ->
      let%bind a = of_expression ~rctx a in
      let%bind b = of_expression ~rctx b in
      let%map c = of_expression ~rctx c in
      SIte (a, b, c)
    | ESel _ | EData _ -> None
    | EOp (op, args) ->
      (match op with
      | Unary uop ->
        (match args with
        | [ arg ] ->
          let%map arg = of_expression ~rctx arg in
          SUn (uop, arg)
        | _ -> None)
      | Binary bop ->
        (match args with
        | arg1 :: arg2 :: tl ->
          let%bind arg1 = of_expression ~rctx arg1 in
          let%map arg2 = of_expression ~rctx (EOp (Binary bop, arg2 :: tl)) in
          SBin (bop, arg1, arg2)
        | [ arg ] -> of_expression ~rctx arg
        | [] -> None))
  ;;
end

open Expression

let collect_common_factors (top_op : Operator.t option) (args : t list)
    : (int * Operator.t option * t) list
  =
  let add_fac op_opt factors e =
    let added, factors' =
      (* Find a matching factor - if found, add it to existing factors. *)
      List.fold
        factors
        ~init:(false, [])
        ~f:(fun (added, factors) (fac_count, fac_op_option, fac_expr) ->
          if equal fac_expr e
          then (
            match op_opt, fac_op_option with
            | Some op, Some fo when Operator.equal op fo ->
              true, (fac_count + 1, Some fo, e) :: factors
            | None, Some fo -> added, (fac_count + 1, Some fo, e) :: factors
            | Some op, None -> true, (fac_count + 1, Some op, e) :: factors
            | None, None -> true, (fac_count + 1, None, e) :: factors
            | _ -> added, (fac_count, fac_op_option, fac_expr) :: factors)
          else
            (* List of factors unchanged. *)
            added, (fac_count, fac_op_option, fac_expr) :: factors)
    in
    if added then factors' else (1, op_opt, e) :: factors'
  in
  let f factors arg =
    match top_op with
    | Some top_op ->
      (match arg with
      | EOp (op', args') when is_left_distrib op' top_op ->
        List.fold ~init:factors ~f:(add_fac (Some op')) args'
      | _ -> List.fold ~init:factors ~f:(add_fac None) [ arg ])
    | None ->
      (match arg with
      | EOp (op', args') -> List.fold ~init:factors ~f:(add_fac (Some op')) args'
      | _ -> List.fold ~init:factors ~f:(add_fac None) [ arg ])
  in
  List.fold ~f ~init:[] args
;;

let best_factor (factors : (int * Operator.t option * t) list)
    : (Operator.t option * t) option
  =
  match
    List.max_elt ~compare:(fun (i1, _, _) (i2, _, _) -> Int.compare i1 i2) factors
  with
  | Some (i, op, e) -> if i >= 2 then Some (op, e) else None
  | None -> None
;;

let apply_factor
    (op : Operator.t)
    (args : t list)
    (fac_op : Operator.t option)
    (fac_expr : t)
  =
  let%map fac_op = Option.first_some fac_op (get_distributing op) in
  let args_with_fac, args_no_fac =
    let f arg =
      match arg with
      | EOp (op', args') when Operator.equal op' fac_op ->
        let facs, rest = List.partition_tf ~f:(fun e -> equal e fac_expr) args' in
        (match facs with
        | [] -> Either.Second arg
        | _ :: tl -> Either.First (mk_e_assoc op' (tl @ rest)))
      | _ ->
        if equal arg fac_expr
        then (
          match get_id_const fac_op with
          | Some e -> Either.First e
          | _ -> Either.Second arg)
        else Either.Second arg
    in
    List.partition_map ~f args
  in
  match args_with_fac with
  | [] -> normalize (mk_e_assoc op args_no_fac)
  | _ ->
    let factorized = mk_e_assoc fac_op [ fac_expr; mk_e_assoc op args_with_fac ] in
    normalize (mk_e_assoc op (factorized :: args_no_fac))
;;

let apply_ite_factor
    (cond : t)
    (true_br : t)
    (false_br : t)
    (fac_op : Operator.t)
    (fac_expr : t)
  =
  let f arg =
    match arg with
    | EOp (op', args') when Operator.equal op' fac_op ->
      let facs, rest = List.partition_tf ~f:(fun e -> equal e fac_expr) args' in
      (match facs with
      | [] -> None
      | _ :: tl ->
        (match tl @ rest with
        | [] -> None
        | _ as args -> Some (mk_e_assoc op' args)))
    | _ ->
      if equal arg fac_expr
      then (
        match get_id_const fac_op with
        | Some e -> Some e
        | _ -> None)
      else None
  in
  let true_br_defac = f true_br
  and false_br_defac = f false_br in
  match true_br_defac, false_br_defac with
  | Some tbr, Some fbr ->
    Some (normalize (mk_e_assoc fac_op [ fac_expr; mk_e_ite cond tbr fbr ]))
  | _ -> None
;;

(** Factorize a term using distributivity rules. *)
let factorize (e : t) : t =
  let fac_rule e =
    let case f e =
      match e with
      | EOp (op, args) ->
        let args' = List.map ~f args in
        let factors = collect_common_factors (Some op) args' in
        Option.bind (best_factor factors) ~f:(fun (fac_op_opt, fac_expr) ->
            apply_factor op args' fac_op_opt fac_expr)
      | EIte (cond, br_true, br_false) ->
        let cond = f cond
        and br_true = f br_true
        and br_false = f br_false in
        Option.(
          bind
            (best_factor (collect_common_factors None [ br_true; br_false ]))
            ~f:(fun (fac_op_opt, fac_expr) ->
              bind fac_op_opt ~f:(fun fac_op ->
                  apply_ite_factor cond br_true br_false fac_op fac_expr)))
      | _ -> None
    in
    normalize (transform case e)
  in
  rewrite_until_stable fac_rule (normalize e)
;;

let distrib (op1 : Operator.t) (args : t list) : t =
  let f hd tl =
    mk_e_assoc
      op1
      (List.fold ~init:[ hd ] tl ~f:(fun acc e' ->
           match e' with
           | EOp (op2, args) ->
             if is_left_distrib op1 op2
             then
               [ mk_e_assoc
                   op2
                   (List.map args ~f:(fun arg -> mk_e_assoc op1 (acc @ [ arg ])))
               ]
             else acc @ [ e' ]
           | EIte (cond, true_br, false_br) ->
             [ mk_e_ite
                 cond
                 (mk_e_assoc op1 (acc @ [ true_br ]))
                 (mk_e_assoc op1 (acc @ [ false_br ]))
             ]
           | _ -> acc @ [ e' ]))
  in
  let args = if is_commutative op1 then List.sort ~compare args else args in
  match args with
  | [ a ] -> mk_e_assoc op1 [ a ]
  | hd :: tl -> f hd tl
  | _ -> failwith "Empty application."
;;

(** Expand a term by applying distributivity rules.*)
let expand (e : t) : t =
  let expand_rule e =
    let case f e =
      match e with
      | EOp (op1, args) -> Some (distrib op1 (List.map ~f args))
      | _ -> None
    in
    normalize (transform case e)
  in
  rewrite_until_stable expand_rule e
;;

let eequals a b =
  equal (expand (factorize (normalize a))) (expand (factorize (normalize b)))
;;

(** Rewrite rule out of a lemma. *)
let rewrite_with_lemma (lemma : t) : t -> t list =
  let conjs =
    match expand lemma with
    | EOp (Binary And, args) -> args
    | lemma' -> [ lemma' ]
  in
  let dyn_rules =
    let r e =
      let open Operator in
      match e with
      (* a >= x : a -> max a x *)
      | EOp (Binary Ge, [ a; EInt x ]) | EOp (Binary Le, [ EInt x; a ]) ->
        [ a, mk_e_assoc (Binary Max) [ a; EInt x ] ]
      | EOp (Binary Gt, [ a; EInt x ]) | EOp (Binary Lt, [ EInt x; a ]) ->
        [ a, mk_e_assoc (Binary Max) [ a; EInt (x + 1) ] ]
      (* a <= x : a -> min a x *)
      | EOp (Binary Le, [ a; EInt x ]) | EOp (Binary Ge, [ EInt x; a ]) ->
        [ a, mk_e_assoc (Binary Min) [ a; EInt x ] ]
      | EOp (Binary Lt, [ a; EInt x ]) | EOp (Binary Gt, [ EInt x; a ]) ->
        [ a, mk_e_assoc (Binary Min) [ a; EInt (x - 1) ] ]
      | _ -> []
    in
    List.concat_map ~f:r conjs
  in
  let apply_dyn_rule e =
    List.map dyn_rules ~f:(fun (lhs, rhs) ->
        let e' = transform (fun _ e -> if eequals lhs e then Some rhs else None) e in
        e')
  in
  apply_dyn_rule
;;

let match_core (bid : boxkind) (sube : t) : t -> t =
  let transformer _ e0 =
    if eequals sube e0
    then Some (EBox bid)
    else (
      match e0, sube with
      | EOp (op, args), EOp (subop, subargs) when Operator.equal op subop ->
        let subargs', rest =
          List.partition_tf ~f:(List.mem ~equal:eequals subargs) args
        in
        if List.length subargs' = List.length subargs
        then Some (mk_e_assoc op (EBox bid :: rest))
        else None
      (* | EInt i1, EInt i2 ->
        Some (mk_e_assoc (Binary Binop.Plus) [ mk_e_int (i2 - i1); EBox bid ]) *)
      | _ -> None)
  in
  transform transformer
;;

let match_as_is (bid : boxkind) (sube : t) ~(of_ : t) : t option =
  let res = match_core bid sube of_ in
  if count_boxkind bid res > 0 then Some res else None
;;

(** Matching subexpressions (up to rewriting) *)
let match_after_expand ?(lemma = None) (bid : boxkind) (sube : t) ~(of_ : t) : t option =
  (* Expand expressions. *)
  let of_ = expand of_
  and sube = expand sube in
  (* Use lemmas to rewrite expressions.  *)
  let of_choices =
    let choices_from_lemma_r =
      Option.value
        (Option.map
           ~f:(fun l -> of_ :: List.map ~f:expand (rewrite_with_lemma l of_))
           lemma)
        ~default:[ of_ ]
    in
    List.remove_consecutive_duplicates ~equal:eequals choices_from_lemma_r
  in
  match List.filter_map ~f:(fun of_ -> match_as_is bid sube ~of_) of_choices with
  | hd :: _ -> Some hd
  | _ -> None
;;

let match_as_subexpr ?(lemma = None) (bid : boxkind) (sube : t) ~(of_ : t) : t option =
  match match_after_expand ~lemma bid sube ~of_ with
  | Some hd -> Some hd
  | None -> match_as_is bid (factorize sube) ~of_:(factorize of_)
;;

let simplify_term ~(ctx : Context.t) (t : term) : term =
  let simpl =
    let rctx = RContext.create () in
    let%bind expr = of_term ~rctx t in
    to_term ~ctx ~rctx (normalize expr)
  in
  match simpl with
  | Some s -> s
  | None -> t
;;
