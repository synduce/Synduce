open Base
open Term

type spec = { ensures : term option; requires : term option }

let empty_spec = { ensures = None; requires = None }

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
