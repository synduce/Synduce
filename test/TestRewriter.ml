open Base
open Lang
open Rewriter
open Expression
open Fmt

let e_to_expand =
  let open Op in
  [
    var 1 * (var 2 + var 3);
    var 1 * ((var 2 * int 5) + var 3);
    max (max (max (var 22) (int 0) + var 5) (int 0) + var 3) (int 0);
    max (max (var 5) (int 0) + var 3) (int 0);
    max (max (max (var 1 + var 2) (int 0) + var 3) (int 0) + var 4) (int 0);
    var 1 * (var 2 + var 3) * int 2;
    ~?(var 4) (var 1 * (var 2 + var 3)) (var 2);
    var 1 && (var 1 || var 3);
  ]

let e_to_factorize =
  let open Op in
  [
    (var 1 * var 2) + (var 1 * var 3);
    (var 1 * var 2) + (var 1 * var 3) + (var 1 * var 3 * var 4);
    max (max (var 1 + var 2) (var 1 + var 3)) (var 1 + var 3 + var 4);
    max (var 1 + var 1 + (var 1 * var 2)) ((var 1 * int 2) + (var 1 * var 2));
    var 1 + var 1 + var 1;
  ]

let e_pairs_to_elim =
  let open Op in
  [
    ( max (max (max (var 22) (int 0) + var 5) (int 0) + var 3) (int 0),
      max (max (var 5) (int 0) + var 3) (int 0) );
    ( var 5 * max (max (max (var 22) (int 0) + var 5) (int 0) + var 3) (int 0),
      max (max (var 5) (int 0) + (int 0 + var 3)) (int 0) );
  ]

;;
set_style_renderer stdout `Ansi_tty;
Caml.Format.set_margin 100;
pf stdout "Expand@.";
List.iteri e_to_expand ~f:(fun i e ->
    pf stdout "@[%a: %a --->@;<1 4>%a@]@." (styled (`Bg `Magenta) int) i pp e pp (expand e));
pf stdout "Factorize@.";
List.iteri e_to_factorize ~f:(fun i e ->
    pf stdout "@[%a: %a --->@;<1 4>%a@]@." (styled (`Bg `Magenta) int) i pp e pp (factorize e));
pf stdout "Match subexpression@.";
List.iteri e_pairs_to_elim ~f:(fun i (e, esub) ->
    pf stdout "@[%a: %a --->@;<1 4>%a@]@." (styled (`Bg `Magenta) int) i pp e pp esub;
    match match_as_subexpr esub ~of_:e with
    | Some (i, t) -> pf stdout "%i -> %a@." i pp t
    | None -> pf stdout "No.@.")
