type 'a list = Nil | Cons of 'a * 'a list

let rec spec = function
  | Nil -> (0, 0)
  | Cons (hd, tl) ->
      let sum, mts = spec tl in
      (sum + hd, max mts (sum + hd))

let rec target l = f [%synt s0] l

and f s = function Nil -> s | Cons (hd, tl) -> f ([%synt oplus] s hd) tl
