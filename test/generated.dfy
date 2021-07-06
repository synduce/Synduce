datatype tree<a1> =  Node(a1, tree<a1>, tree<a1>) |  Nil

function repr(xre : tree<int>) : tree<int>  
{
  xre
}
function spec(xsp : tree<int>) : int  
{
  f(0, xsp)
}
function f(x5 : int, x6 : tree<int>) : int 
// decreases x6
{
  match x6
  case Nil =>  x5
  case Node(a, l, r) => 
    ((sum) => f(sum + a, r))(f(x5, l))
  
}
function target(xtarg : tree<int>) : int 
// decreases xtarg
{
  match xtarg
  case Nil => 0
  case Node(a, l, r) =>  join(a, target(l), target(r))
}
function join(x2 : int, x3 : int, x4 : int) : int
{
  x3 + x2 + x4
}


lemma correctness_lemma(x : tree<int>)
ensures target(x) == spec(repr(x))
{
  match x
  case Nil => {}
  case Node(a, l, r) => {}
}
