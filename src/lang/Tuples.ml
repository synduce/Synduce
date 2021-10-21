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
let projection_prefix = "synduce_tuple_proj_"
let constructor_prefix = "mk"

let make_tuple_name (tl : t list) : string =
  let names =
    List.map
      ~f:(fun t -> String.substr_replace_all Fmt.(str "_%a" pp t) ~pattern:" " ~with_:"_")
      tl
  in
  List.fold_left ~f:(fun s v -> s ^ v) ~init:"Tuple" names
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

let is_constr_name (name : string) =
  let f (_, x) = String.is_suffix ~suffix:x name in
  List.exists ~f !tuple_map && String.is_prefix ~prefix:constructor_prefix name
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
