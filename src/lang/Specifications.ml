open Base
open Term

type spec = { ensures : term option; requires : term option }

let empty_spec = { ensures = None; requires = None }

let specs : (int, spec) Hashtbl.t = Hashtbl.create (module Int)

let reinit () = Hashtbl.clear specs

let get_spec (v : variable) : spec option = Hashtbl.find specs v.vid

let get_ensures (v : variable) : term option =
  match Hashtbl.find specs v.vid with Some spec -> spec.ensures | None -> None

let get_requires (v : variable) : term option =
  match Hashtbl.find specs v.vid with Some spec -> spec.requires | None -> None

let set_spec (v : variable) (spec : spec) : unit = Hashtbl.set specs ~key:v.vid ~data:spec

let set_ensures (v : variable) (ens_t : term) =
  match get_spec v with
  | Some s -> set_spec v { s with ensures = Some ens_t }
  | None -> set_spec v { ensures = Some ens_t; requires = None }

let set_requires (v : variable) (req_t : term) =
  match get_spec v with
  | Some s -> set_spec v { s with requires = Some req_t }
  | None -> set_spec v { ensures = None; requires = Some req_t }

let is_not_empty s = Option.is_some s.ensures || Option.is_some s.requires

let pp_spec (frmt : Formatter.t) (s : spec) =
  let spec_color = `Hi `Magenta in
  (if is_not_empty s then Fmt.(pf frmt "%a" (styled (`Fg spec_color) string) "«"));
  (match s.requires with
  | Some req -> Fmt.(pf frmt "@[%a %a@]" (styled (`Fg spec_color) string) "requires" pp_term req)
  | None -> ());
  (match (s.requires, s.ensures) with Some _, Some _ -> Fmt.pf frmt "@;,@;" | _ -> ());
  (match s.ensures with
  | Some ens -> Fmt.(pf frmt "@[%a %a@]" (styled (`Fg spec_color) string) "ensures" pp_term ens)
  | None -> ());
  if is_not_empty s then Fmt.(pf frmt "%a" (styled (`Fg spec_color) string) "»")

let dump_all (frmt : Formatter.t) () : unit =
  Hashtbl.iteri specs ~f:(fun ~key ~data ->
      Fmt.(
        pf frmt "@[- Spec %a:%i :@;%a@]@;<100 0>" (option string) (Variable.get_name key) key
          pp_spec data))
