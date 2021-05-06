open Base

type typ = string * int

let _IDS = Hashtbl.create (module Int) ~size:20

let get_ids () = _IDS

let _NAMES = Hashtbl.create (module String) ~size:100

let _MAX_ID = ref 0

let new_id () =
  let i = !_MAX_ID in
  _MAX_ID := !_MAX_ID + 1;
  i

let fresh ?(s = "x") () : string =
  match Hashtbl.find _NAMES s with
  | Some (id0 :: ids) ->
      let new_id = id0 + 1 in
      let fresh_name = s ^ Int.to_string new_id in
      Hashtbl.set _NAMES ~key:s ~data:(new_id :: id0 :: ids);
      fresh_name
  | Some [] ->
      let fresh_name = s ^ "0" in
      Hashtbl.set _NAMES ~key:s ~data:[ 0 ];
      fresh_name
  | _ ->
      Hashtbl.set _NAMES ~key:s ~data:[];
      s

let mk_with_id (i : int) (s : string) (f : int -> 'a) : 'a =
  if i > 0 then f i
  else
    let id = new_id () in
    match Hashtbl.add _IDS ~key:id ~data:s with
    | _ ->
        ();
        f id
