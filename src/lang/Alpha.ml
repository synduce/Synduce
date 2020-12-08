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

let rec fresh (s : string) : string =
  if Hashtbl.mem _NAMES s then
    fresh (s^(Int.to_string (new_id ())))
  else
    (Hashtbl.add_multi _NAMES ~key:s ~data:!_MAX_ID; s)

let mk_with_id (i : int) (s : string) (f : int -> 'a) : 'a =
  if i > 0 then f i
  else
    let id = new_id () in
    match Hashtbl.add _IDS ~key:id ~data:s with
    | _ -> ();
      f id
