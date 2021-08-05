datatype list<t> =  Cons(int, list<t>) |  Elt(int)

datatype clist<a1> =  Concat(clist<a1>, clist<a1>) |  Single(a1)

function min(a:int, b:int): (x: int)
    ensures a == x || x == b
    ensures x <= a && x <= b
{
    if a >= b then b else a
}

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

lemma lemma0(p20 : clist<int>, p21 : clist<int>) 
ensures (spec(repr(Concat(p20, p21))) == target(Concat(p20, p21)))
{
  match p20
    case Single(p25) =>
     {
       match p21
         case Single(p28) => {
                               calc == {
                                 
                                 } }
           case Concat(p26, p27) => {
                                      calc == {
                                        
                                        } }
        }
          case Concat(p23, p24) =>
           {
             match p21
               case Single(p31) =>
                {
                  calc == {
                    
                    } }
                 case Concat(p29, p30) => {
                                            calc == {
                                              
                                              } }
              } }

lemma correctness_lemma(x : clist<int>) 
ensures (target(x) == spec(repr(x)))
{
  match x
  case Single(p22) => assert(true);
  case Concat(p20, p21) => lemma0(p20, p21);
}
