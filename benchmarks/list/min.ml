type 'a clist =  Single of 'a | Concat of 'a clist * 'a clist

type 'a list = Elt of 'a | Cons of 'a * 'a list

let rec amin =
    function
    | Elt(a) -> a
    | Cons(hd, tl) -> min hd (amin tl)
    [@inv (fun x -> true)]

let rec list_of_clist  =
    function
    | Single(a) -> Elt(a)
    | Concat(x, y) -> dec y x

and dec l1 =
    function
    | Single(a) -> Cons(a, list_of_clist l1)
    | Concat(x, y) -> dec (Concat(l1, y)) x

let rec hom =
    function
    | Single(a)    -> f0 a
    | Concat(x, y) -> odot (hom x) (hom y)
    [@defining f0 odot]
    [@equiv amin list_of_clist]


