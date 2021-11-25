type 'a conslist =
  | Nil
  | Cons of 'a * 'a conslist

type 'a eltlist =
  | Elt of 'a
  | ECons of 'a * 'a eltlist
