open Base

type t =
  { ids : (int, string) Hashtbl.t
  ; names : (string, int list) Hashtbl.t
  ; max_id : int ref
  }

let create () =
  { ids = Hashtbl.create (module Int) ~size:20
  ; names = Hashtbl.create (module String) ~size:1000
  ; max_id = ref 0
  }
;;

let copy (env : t) =
  { ids = Hashtbl.copy env.ids
  ; names = Hashtbl.copy env.names
  ; max_id = ref !(env.max_id)
  }
;;

let reinit t =
  t.max_id := 0;
  Hashtbl.clear t.ids;
  Hashtbl.clear t.names
;;

let new_id (env : t) =
  let i = !(env.max_id) in
  Int.incr env.max_id;
  i
;;

let forget (env : t) (i : int) (s : string) =
  Hashtbl.remove env.ids i;
  match Hashtbl.find env.names s with
  | Some id_list ->
    (match List.filter ~f:(fun i' -> not (i' = i)) id_list with
    | hd :: tl -> Hashtbl.set env.names ~key:s ~data:(hd :: tl)
    | _ -> Hashtbl.remove env.names s)
  | None -> ()
;;

let fresh ?(s = "x") (env : t) : string =
  match Hashtbl.find env.names s with
  | Some (id0 :: ids) ->
    let new_id = id0 + 1 in
    let fresh_name = s ^ Int.to_string new_id in
    Hashtbl.set env.names ~key:s ~data:(new_id :: id0 :: ids);
    fresh_name
  | Some [] ->
    let fresh_name = s ^ "0" in
    Hashtbl.set env.names ~key:s ~data:[ 0 ];
    fresh_name
  | _ ->
    Hashtbl.set env.names ~key:s ~data:[];
    s
;;

let mk_with_id (env : t) (i : int) (s : string) (f : int -> 'a) : 'a =
  if i > 0
  then (
    Hashtbl.add_multi env.names ~key:s ~data:i;
    f i)
  else (
    let id = new_id env in
    match Hashtbl.add env.ids ~key:id ~data:s with
    | _ ->
      Hashtbl.add_multi env.names ~key:s ~data:i;
      f id)
;;

let get_exn (env : t) (id : int) = Hashtbl.find_exn env.ids id
let bad_prefixes = [ "synduce"; "tuple"; "function"; "mkTup" ]

(* Check that a variable has an acceptable name. *)
let check_source_variable_name
    ((loc1, _) : Lexing.position * Lexing.position)
    (v : string)
  =
  (match List.find ~f:(fun prefix -> String.is_prefix ~prefix v) bad_prefixes with
  | Some bad_prefix ->
    Utils.Log.error_msg
      Fmt.(
        str
          "Variable name %s (in %s at %i:%i) cannot start with ``%s''"
          v
          loc1.pos_fname
          loc1.pos_lnum
          loc1.pos_cnum
          bad_prefix);
    failwith "Bad variable name."
  | None -> ());
  if String.contains v '\''
  then (
    Utils.Log.error_msg
      Fmt.(
        str
          "Variable name %s (in %s at %i:%i) should not contain \' "
          v
          loc1.pos_fname
          loc1.pos_lnum
          loc1.pos_cnum);
    failwith "Bad variable name.")
;;

module Nice = struct
  let nicenames = [ "a"; "b"; "c"; "x"; "y"; "z"; "w"; "u"; "v" ]
  let index = ref 0
  let nice_queue = Queue.of_list nicenames

  let reset () =
    Int.incr index;
    Queue.clear nice_queue;
    Queue.enqueue_all nice_queue nicenames
  ;;

  let next () =
    match Queue.dequeue nice_queue with
    | Some x -> if !index > 0 then x ^ Int.to_string !index else x
    | None ->
      reset ();
      Queue.dequeue_exn nice_queue
  ;;
end
