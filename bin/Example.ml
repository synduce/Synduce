open Base
open Lib.Automata
open Lib.PMRS
open Lib.Utils
open Sexplib

let test_depth = 3

(* Declare symbols. *)

let nil = Terminal.mk "Nil" 0 []

let cons = Terminal.mk "Cons" 2 []

let concat = Terminal.mk "Concat" 2 []

let single = Terminal.mk "Single" 1 []

let node = Terminal.mk "Node" 3 []
let zero = Terminal.mk "0" 0 []
let one = Terminal.mk "1" 0 []
let plus = Terminal.mk "+" 3 []
let times = Terminal.mk "*" 3 []
let tint = Terminal.mk "Int" 1 []

let odot = Terminal.mk "odot" 2 []
let x_0 = Terminal.mk "x_0" 0 []

let f_0 = Terminal.mk "f_0" 1 []

(* Automaton defining a tree grammar *)
let a1 =
  {
    states = [0; 1; 2; 3; 4; 5; 6];
    initial = 0;
    final = [2;3;6];
    sigma = [(10, node);(11,nil);(12,zero);(13,one)];
    transitions = Map.of_alist_exn (module Int)
        [
          0,[10,[1;4;0]; 11,[2]];
          1,[12,[3]];
          4,[10,[5;0;4]; 11,[2]];
          5,[13,[6]];
        ];
  }

let a2 =
  {
    states = [0; 1; 2; 3];
    initial = 0;
    final = [2;3];
    sigma = [(10, concat); (11, single); (12, nil); (13, zero)];
    transitions = Map.of_alist_exn (module Int)
        [
          0,[10,[0;0];11,[1];12,[2]];
          1,[13,[3]]
        ];
  }

let sigma =
  Map.of_alist_exn (module String)
    [
      ("Node", node);
      ("Nil", nil);
      ("Concat", concat);
      ("Single", single);
      ("0", zero);
      ("1", one);
      ("+", plus);
      ("*", plus);
      ("x_0", x_0);
      ("f_0", f_0);
      ("odot", odot)
    ]

let r1 = Sexp.load_sexp "inputs/simple.pmrs"
let r2 = Sexp.load_sexp "inputs/listhom.pmrs"
let pmrs1 = parse_rules sigma r1
let pmrs2 = parse_rules sigma r2

let t1 = time (fun () -> generate ~depth:test_depth a1)
let t2 = time (fun () -> generate ~depth:test_depth a2)
let t1_complete = List.filter ~f:(fun t -> recognize ~cont_ok:false a1 t) t1
let t2_complete = List.filter ~f:(fun t -> recognize ~cont_ok:false a2 t) t2

let test_automaton autom gens () =
  Fmt.(pf stdout "Generated %i terms.@." (List.length gens));
  List.iter ~f:(fun x ->
      if recognize autom x then ()
      else Fmt.(pf stdout "ERROR")) gens;
  List.iter ~f:(fun x ->
      if recognize ~cont_ok:false autom x then
        Fmt.(pf stdout "TERM: %a@."
               (Tree.print_tree (fun fmt s -> string fmt s.Terminal.name)) x)
      else ()
    ) gens

let test_pmrs pmrs tcomp =
  let terms = List.map ~f:(term_of_terminal_tree sigma) tcomp in
  begin
    match pmrs with
    | Ok rules ->
      begin
        Fmt.(pf stdout "RULES:@.%a@." pp_pmrs rules);
        let f t =
          match t with
          | Ok t ->
            let res = reduce_term ~with_pmrs:rules t in
            Fmt.(pf stdout "Reduce %a:@.\t -> %a@." (box pp_term) t (box pp_term) res)
          | Error _ -> ()
        in
        List.iter ~f terms
      end
    | Error sl ->
      Fmt.(pf stdout "%a@." (list (pair ~sep:comma  string Sexp.pp_hum)) sl)
  end;

;;
test_automaton a1 t1 ();;
test_automaton a2 t2 ();;
test_pmrs pmrs1 t1_complete;;
test_pmrs pmrs2 t2_complete;;