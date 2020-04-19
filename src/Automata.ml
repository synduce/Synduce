open Base
open Utils
open Trees

module Tree = Trees
module IM = Map.M(Int)

type value =
  | VInt of int
  | VBool of bool
  | VString of string

type generator = unit -> value

let empty_gen _ = VString "{}"

let gens (k : int) =
  match k with
  | -1 -> (fun () -> VInt (Random.int 20))
  | -2 -> (fun () -> VBool (Random.int 20 > 10))
  | _ -> empty_gen

module Symbol =
struct
  type t = { name : string; arity : int }
  let equal s1 s2 = String.equal s1.name s2.name && s1.arity = s2.arity
  let mk name arity : t = { name; arity }
  let of_gen (g : generator) : t =
    match g () with
    | VInt i ->  mk (Int.to_string i) (-1)
    | VBool b -> mk (Bool.to_string b) (-2)
    | VString k -> mk k (-3)
end

type tdfta = {
  states : int list;
  initial : int;
  final : (int * int) list;
  sigma : (int, Symbol.t) List.Assoc.t;
  transitions : ((int * int list) list) IM.t ;
}

let is_final (a : tdfta) s =  List.Assoc.mem ~equal a.final s

let final_state (a : tdfta) s =
  List.Assoc.find a.final ~equal s


let itree_to_stree (sigma : (int, Symbol.t) List.Assoc.t) (it : int tree) : Symbol.t tree =
  let to_symbol i =
    List.Assoc.find ~equal sigma i
  in
  let rec f it =
    match it with
    | Nil -> Nil
    | Cont -> Cont
    | Node(x, l) ->
      let l' = List.map ~f l in
      match to_symbol x with
      | Some symb -> Node(symb, l')
      | None -> Node(Symbol.of_gen (gens x), l')
  in f it

let generate ~(depth : int) (a : tdfta) : (Symbol.t tree) list =
  let rec gen k s_in =
    match final_state a s_in with
    | Some i -> if i < 0 then [Node(i, [])] else [Nil]
    | None ->
      (match Map.find a.transitions s_in with
       | Some transitions -> List.concat (List.map ~f:(take_t k) transitions)
       | None -> [])
  and take_t k (x, s_l) : int tree list =
    if k > 0 then
      let chs = product (List.map ~f:(gen (k - 1)) s_l) in
      List.map ~f:(fun ch -> Node(x, ch)) chs
    else
      let f _ = Cont in
      [Node(x, (List.init (List.length s_l) ~f))]
  in
  List.map ~f:(itree_to_stree a.sigma) (gen depth a.initial)

let recognize ?(cont_ok = true) (autom : tdfta) (symb_tree : Symbol.t tree) : bool =
  let symbol_to_id = List.Assoc.inverse autom.sigma in
  let rec take_t s x l =
    if is_final autom s then true else
      begin
        match Map.find autom.transitions s with
        | Some ts ->
          (match List.Assoc.find ~equal:Symbol.equal symbol_to_id x with
           | Some xid ->
             let with_x = List.filter ~f:(fun (x', _) -> xid = x') ts in
             List.fold_left
               ~init:true
               ~f:(fun acc (_, sl) -> acc && (take sl l))
               with_x
           | None -> false)
        | None -> false
      end
  and take sl l =
    let f state tc =
      match tc with
      | Nil -> is_final autom state
      | Cont -> cont_ok
      | Node(x, l') -> take_t state x l'
    in
    match List.map2 ~f sl l with
    | Ok bl -> List.fold ~f:(&&) ~init:true bl
    | Unequal_lengths -> false
  in
  match symb_tree with
  | Nil -> true
  | Cont -> true
  | Node(x, l) -> take_t autom.initial x l