open Base
open Term
open TermTypes

let empty_spec = { ensures = None; requires = None }
let reinit ~(ctx : Context.t) () = Hashtbl.clear ctx.specs

let get_spec ~(ctx : Context.t) (v : variable) : spec option =
  Hashtbl.find ctx.specs v.vid
;;

let get_ensures ~(ctx : Context.t) (v : variable) : term option =
  match Hashtbl.find ctx.specs v.vid with
  | Some spec -> spec.ensures
  | None -> None
;;

let get_requires ~(ctx : Context.t) (v : variable) : term option =
  match Hashtbl.find ctx.specs v.vid with
  | Some spec -> spec.requires
  | None -> None
;;

let set_spec ~(ctx : Context.t) (v : variable) (spec : spec) : unit =
  Hashtbl.set ctx.specs ~key:v.vid ~data:spec
;;

let set_ensures ~(ctx : Context.t) (v : variable) (ens_t : term) =
  match get_spec ~ctx v with
  | Some s -> set_spec ~ctx v { s with ensures = Some ens_t }
  | None -> set_spec ~ctx v { ensures = Some ens_t; requires = None }
;;

let set_requires ~(ctx : Context.t) (v : variable) (req_t : term) =
  match get_spec ~ctx v with
  | Some s -> set_spec ~ctx v { s with requires = Some req_t }
  | None -> set_spec ~ctx v { ensures = None; requires = Some req_t }
;;

let is_not_empty s = Option.is_some s.ensures || Option.is_some s.requires

let pp_spec ~(ctx : Context.t) (frmt : Formatter.t) (s : spec) =
  let spec_color = `Hi `Magenta in
  (if is_not_empty s then Fmt.(pf frmt "%a" (styled (`Fg spec_color) string) "«"));
  (match s.requires with
  | Some req ->
    Fmt.(
      pf frmt "@[%a %a@]" (styled (`Fg spec_color) string) "requires" (pp_term ctx) req)
  | None -> ());
  (match s.requires, s.ensures with
  | Some _, Some _ -> Fmt.pf frmt "@;,@;"
  | _ -> ());
  (match s.ensures with
  | Some ens ->
    Fmt.(pf frmt "@[%a %a@]" (styled (`Fg spec_color) string) "ensures" (pp_term ctx) ens)
  | None -> ());
  if is_not_empty s then Fmt.(pf frmt "%a" (styled (`Fg spec_color) string) "»")
;;

let dump_all ~(ctx : Context.t) (frmt : Formatter.t) () : unit =
  Hashtbl.iteri ctx.specs ~f:(fun ~key ~data ->
      Fmt.(
        pf
          frmt
          "@[- Spec %a:%i :@;%a@]@;<100 0>"
          (option string)
          (Variable.get_name ctx key)
          key
          (pp_spec ~ctx)
          data))
;;
