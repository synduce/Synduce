datatype list =  Cons(int, list<int>) |  Elt(int)

datatype clist<a1> =  Concat(clist<a1>, clist<a1>) |  Single(a1)

function repr(xre : clist<int>) : list<int>
decreases xre
{
  match xre
  case Single(a) => Elt(a)
  case Concat(x, y) => dec(y, x)
}

function dec(xd : clist<int>, xd0 : clist<int>) : list<int>
decreases xd0
{
  match xd0
  case Single(a) => Cons(a, repr(xd))
  case Concat(x, y) => dec(Concat(y, xd), x)
}

function spec(xsp : list<int>) : int
decreases xsp
{
  match xsp
  case Elt(a) => a
  case Cons(hd, tl) => min (hd, spec(tl))
}

function target(xtarg : clist<int>) : int
decreases xtarg
{
  match xtarg
  case Single(a) => a
  case Concat(x, y) => odot(target(x), target(y))
}

function f0(x : int) : int
decreases x
{
  x
}

function odot(x0 : int, x1 : int) : int
decreases x1
{
  min (x0, x1)
}

lemma correctness_lemma(x : clist) 
ensures (target(x) == spec(repr(x)))
{
  
}
