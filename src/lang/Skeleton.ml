open Base
open EProps
open Term
open Option.Let_syntax
open Utils

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

let rec of_expression ~(ctx : RContext.t) : Expression.t -> t option = function
  | EInt _ -> Some (SType RType.TInt)
  | EChar _ -> Some (SType RType.TChar)
  | EFalse | ETrue -> Some (SType RType.TBool)
  | EEmptySet t -> Some (SType (RType.TSet t))
  | EApp (_, _) -> None
  | EVar v ->
    Option.(
      RContext.get_var ctx v >>= Variable.vtype ctx.parent >>= fun t -> Some (SType t))
  | EBox boxkind ->
    (match boxkind with
    | Expression.Position i -> Some (SArg i)
    | _ -> None)
  | ETup tl ->
    let%map tl = all_or_none (List.map ~f:(of_expression ~ctx) tl) in
    STuple tl
  | EIte (a, b, c) ->
    let%bind a = of_expression ~ctx a in
    let%bind b = of_expression ~ctx b in
    let%map c = of_expression ~ctx c in
    SIte (a, b, c)
  | ESel _ | EData _ -> None
  | EOp (op, args) ->
    (match op with
    | Unary uop ->
      (match args with
      | [ arg ] ->
        let%map arg = of_expression ~ctx arg in
        SUn (uop, arg)
      | _ -> None)
    | Binary bop ->
      (match args with
      | arg1 :: arg2 :: tl ->
        let%bind arg1 = of_expression ~ctx arg1 in
        let%map arg2 = of_expression ~ctx (EOp (Binary bop, arg2 :: tl)) in
        SBin (bop, arg1, arg2)
      | [ arg ] -> of_expression ~ctx arg
      | [] -> None))
;;
