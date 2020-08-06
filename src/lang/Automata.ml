open Alpha
open Base
open Utils
open Trees

module Tree = Trees
module IM = Map.M(Int)


module Terminal =
struct
  type t = { name : string; id : int; arity : int; otype : typ list }
  let compare t1 t2 = Base.compare t1.id t2.id
  let equal t1 t2 = Base.equal t1.id t2.id

  let mk ?(id = (-1)) (name : string) (arity : int) (otype : typ list) : t =
    mk_with_id id name (fun id -> { name; id; arity; otype })

  let mk_many (info : (string * int * typ list) list) : t list =
    let f acc (s, i, t) =
      (mk s i t) :: acc
    in List.fold ~f ~init:[] info
end

type tdfta = {
  states : int list;
  initial : int;
  final : int list;
  sigma : (int, Terminal.t) List.Assoc.t;
  transitions : ((int * int list) list) IM.t ;
}

let is_final (a : tdfta) s =  List.mem ~equal a.final s


let itree_to_stree (sigma : (int, Terminal.t) List.Assoc.t) (it : int tree) : Terminal.t tree =
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
      | None -> Nil
  in f it

let generate ~(depth : int) (a : tdfta) : (Terminal.t tree) list =
  let rec gen k s_in =
    match is_final a s_in with
    | true -> [Nil]
    | false ->
      (match Map.find a.transitions s_in with
       | Some transitions -> List.concat (List.map ~f:(take_t k) transitions)
       | None -> [])
  and take_t k (alpha, states) : int tree list =
    if k > 0 then
      let chs = product (List.map ~f:(gen (k - 1)) states) in
      List.map ~f:(fun ch -> Node(alpha, ch)) chs
    else
      let f _ = Cont in
      [Node(alpha, (List.init (List.length states) ~f))]
  in
  List.map ~f:(itree_to_stree a.sigma) (gen depth a.initial)

let recognize ?(cont_ok = true) (autom : tdfta) (symb_tree : Terminal.t tree) : bool =
  let symbol_to_id = List.Assoc.inverse autom.sigma in
  let rec take_t s x l =
    if is_final autom s then true else
      begin
        match Map.find autom.transitions s with
        | Some ts ->
          (match List.Assoc.find ~equal:Terminal.equal symbol_to_id x with
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