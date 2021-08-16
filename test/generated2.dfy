datatype list<b0> =  Cons(int, list<b0>) |  Elt(int)

datatype clist<a1> =  Concat(clist<a1>, clist<a1>) |  Single(a1)

function repr(xre : clist<int>) : (list<int>)
decreases xre
{
  match xre
  case Single(a) => Elt(a)
  case Concat(x, y) => dec(y, x)
}

function dec(xd : clist<int>, xd0 : clist<int>) : (list<int>)
decreases xd0
{
  match xd0
  case Single(a) => Cons(a, repr(xd))
  case Concat(x, y) => dec(Concat(y, xd), x)
}

function spec(xsp : list<int>) : (int)
decreases xsp
{
  match xsp
  case Elt(a) => a
  case Cons(hd, tl) => min (hd, spec(tl))
}

function target(xtarg : clist<int>) : (int)
decreases xtarg
{
  match xtarg
  case Single(a) => a
  case Concat(x, y) => odot(target(x), target(y))
}

function f0(x : int) : (int)
decreases x
{
  x
}

function odot(x0 : int, x1 : int) : (int)
decreases x1
{
  min (x0, x1)
}

function max(a : int, b : int) : (x: (int))
ensures (a == x) || (b == x)
ensures (x >= a) && (x >= b)
{
  if a >= b then a else b
}

function min(a : int, b : int) : (x: (int))
ensures (a == x) || (b == x)
ensures (x <= a) && (x <= b)
{
  if a >= b then b else a
}

lemma lemma0(p20 : clist<int>, p21 : clist<int>) 
ensures (spec(repr(Concat(p20, p21))) == target(Concat(p20, p21)))
{
  match p20
    case Single(i14) =>
     {
       match p21
         case Single(i15) =>
          {
            calc == {
              spec(repr(Concat(Single(i14), Single(i15))));
              spec(dec(Single(i15), Single(i14)));
              min (i14, spec(repr(Single(i15))));
              min (i14, i15);
              target(Concat(Single(i14), Single(i15)));
              } }
           case Concat(p24, p25) =>
            {
              lemma0(p25, p24);
                lemma0(p24, p25);
                calc == {
                  spec(repr(Concat(Single(i14), Concat(p24, p25))));
                  spec(dec(Concat(p24, p25), Single(i14)));
                  min (i14, spec(repr(Concat(p24, p25))));
                  min (i14, spec(dec(p25, p24)));
                  target(Concat(Single(i14), Concat(p24, p25)));
                  } }
        }
          case Concat(p22, p23) =>
           {
             match p21
               case Single(i16) =>
                {
                  lemma0(p23, Concat(p23, Single(i16)));
                    lemma0(p23, p22);
                    lemma0(Concat(p23, Single(i16)), p23);
                    lemma0(Concat(p23, Single(i16)), p22);
                    lemma0(p22, p23);
                    lemma0(p22, Concat(p23, Single(i16)));
                    calc == {
                      spec(repr(Concat(Concat(p22, p23), Single(i16))));
                      spec(dec(Single(i16), Concat(p22, p23)));
                      spec(dec(Concat(p23, Single(i16)), p22));
                      target(Concat(Concat(p22, p23), Single(i16)));
                      } }
                 case Concat(p26, p27) =>
                  {
                    lemma0(p23, p26);
                      lemma0(p23, p27);
                      lemma0(p23, Concat(p23, Concat(p26, p27)));
                      lemma0(p23, p22);
                      lemma0(p26, p23);
                      lemma0(p26, p27);
                      lemma0(p26, Concat(p23, Concat(p26, p27)));
                      lemma0(p26, p22);
                      lemma0(p27, p23);
                      lemma0(p27, p26);
                      lemma0(p27, Concat(p23, Concat(p26, p27)));
                      lemma0(p27, p22);
                      lemma0(Concat(p23, Concat(p26, p27)), p23);
                      lemma0(Concat(p23, Concat(p26, p27)), p26);
                      lemma0(Concat(p23, Concat(p26, p27)), p27);
                      lemma0(Concat(p23, Concat(p26, p27)), p22);
                      lemma0(p22, p23);
                      lemma0(p22, p26);
                      lemma0(p22, p27);
                      lemma0(p22, Concat(p23, Concat(p26, p27)));
                      calc == {
                        spec(repr(Concat(Concat(p22, p23), Concat(p26, p27))));
                        spec(dec(Concat(p26, p27), Concat(p22, p23)));
                        spec(dec(Concat(p23, Concat(p26, p27)), p22));
                        target(Concat(Concat(p22, p23), Concat(p26, p27)));
                        } }
              } }

lemma correctness_lemma(x : clist<int>) 
ensures (target(x) == spec(repr(x)))
{
  match x
    case Single(i13) => assert(true);
    case Concat(p20, p21) => lemma0(p20, p21); }
