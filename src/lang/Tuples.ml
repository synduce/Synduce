open Base
open RType

(* ============================================================================================= *)
(*                        TUPLE TYPES                                                            *)
(* ============================================================================================= *)

(* Tuple types need to be managed for interfacing with SMT solvers. The following functions provide
   tools to add datatype declarations to SMTLIB files, parse reponses, and in general manage the tuple
   types that can be encountered in the program.
*)

let tuple_map : (t list * string) list ref = ref []
let projection_prefix = "proj_"
let constructor_prefix = "mk_"
let tuple_type_name_init = "synd_tup"

let make_tuple_name (tl : t list) : string =
  let names =
    List.map
      ~f:(fun t ->
        match RType.smt_name t with
        | Some n -> String.substr_replace_all ("_" ^ n) ~pattern:" " ~with_:"_"
        | None -> String.substr_replace_all "_unknown" ~pattern:" " ~with_:"_")
      tl
  in
  List.fold_left ~f:(fun s v -> s ^ v) ~init:tuple_type_name_init names
;;

let type_name_of_types (tl : t list) : string =
  let f (tl', x) = if List.equal subtype_of tl tl' then Some x else None in
  match List.find_map ~f !tuple_map with
  | Some x -> x
  | None ->
    let x = make_tuple_name tl in
    tuple_map := (tl, x) :: !tuple_map;
    x
;;

let types_of_tuple_name (name : string) =
  let f (tl, x) = if String.equal x name then Some tl else None in
  List.find_map ~f !tuple_map
;;

let constr_name_of_types (tl : t list) : string =
  constructor_prefix ^ type_name_of_types tl
;;

let is_type_name (name : string) =
  List.exists ~f:(fun (_, x) -> String.equal name x) !tuple_map
;;

let is_constr_name (name : string) =
  let f (_, x) = String.is_suffix ~suffix:x name in
  List.exists ~f !tuple_map && String.is_prefix ~prefix:constructor_prefix name
;;

let type_name_of_constr (name : string) =
  let f (_, x) = String.is_suffix ~suffix:x name in
  Option.map ~f:snd (List.find ~f !tuple_map)
;;

let type_name_of_proj_name (s : string) : string option =
  match String.chop_prefix s ~prefix:projection_prefix with
  | Some rest ->
    let parts = String.split rest ~on:'_' in
    (match List.drop_last parts with
    | Some tuple_type_name ->
      let s = String.concat ~sep:"_" tuple_type_name in
      if is_type_name s then Some s else None
    | _ -> None)
  | None -> None
;;

let proj_of_proj_name (s : string) : (t list * int) option =
  match String.chop_prefix s ~prefix:projection_prefix with
  | Some rest ->
    let parts = String.split rest ~on:'_' in
    (match List.drop_last parts, List.last parts with
    | Some tuple_type_name, Some i_str ->
      (try
         Option.map
           (types_of_tuple_name (String.concat ~sep:"_" tuple_type_name))
           ~f:(fun tl -> tl, Int.of_string i_str)
       with
      | _ -> None)
    | _ -> None)
  | None -> None
;;

let proj_name_of_types (tl : t list) (i : int) : string =
  projection_prefix ^ type_name_of_types tl ^ Fmt.str "_%i" i
;;
