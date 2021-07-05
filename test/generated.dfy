

datatype tree<a1> =  Node(a1, tree<a1>, tree<a1>) |  Nil
function repr(xre : tree<int>) : tree<int>  {xre}
function spec(xsp : tree<int>) : int  {f(0, xsp)}
function f(x5 : int, x6 : tree<int>) : int 
  {match x6
case Nil =>  x5
case Node(a, l, r) =>  ((sum) => f(sum + a, r))(
                                                  f(x5, l))}
function target(xtarg : tree<int>) : int 
  {match xtarg
case Nil =>  s0
case Node(a, l, r) =>  join(a, target(l),
                                                     target(r))}
function s0() : int  {0}
function join(x2 : int, x3 : int, x4 : int) : int  {x3 + x2 + x4}
lemma correctness_lemma(x : tree)  ensures (target(x) == spec(repr(x))) {}
