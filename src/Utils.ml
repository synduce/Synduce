open Base

module IntMap = Map.M(Int)
module StringMap = Map.M(Int)

type z = Z : z

type 'n s = S : 'n -> 'n s

let rec product (l : 'a list list) : 'a list list  =
  let rec aux ~acc l1 l2 = match l1, l2 with
    | [], _ | _, [] -> acc
    | h1::t1, h2::t2 ->
      let acc = (h1::h2)::acc in
      let acc = (aux ~acc t1 l2) in
      aux ~acc [h1] t2
  in match l with
  | [] -> []
  | [l1] -> List.map ~f:(fun x -> [x]) l1
  | l1::tl ->
    let tail_product = product tl in
    aux ~acc:[] l1 tail_product

let time f =
  let t = Unix.gettimeofday () in
  let res = f () in
  Fmt.(pf stdout "ELAPSED: %f s@." (Unix.gettimeofday () -. t));
  res


let (>>?|) = Option.(>>|)
let (>>!|) = Result.(>>|)
let (>>=?) = Option.(>>=)
let (>>=!) = Result.(>>=)

let blast = fun x -> Result.map_error ~f:List.concat (Result.combine_errors x)