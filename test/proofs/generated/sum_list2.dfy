datatype list<t> =  Cons(int, list<t>) |  Nil

datatype clist<a1> =  Concat(clist<a1>, clist<a1>) |  Single(a1) |  CNil

function rep(xclist_to_li : clist<int>) : list<int>
decreases clen(xclist_to_li), xclist_to_li
{
  match xclist_to_li
  case CNil => Nil
  case Single(a) => Cons(a, Nil)
  case Concat(x, y) => dec(y, x)
}

function dec(xd : clist<int>, xd0 : clist<int>) : list<int>
decreases clen(xd) + clen(xd0), xd0
{
  match xd0
  case CNil => rep(xd)
  case Single(a) => Cons(a, rep(xd))
  case Concat(x, y) => dec(Concat(y, xd), x)
}

function spec(xs : list<int>) : int
decreases xs
{
  match xs
  case Nil => 0
  case Cons(hd, tl) => hd + spec(tl)
}

function target(xhs : clist<int>) : int
decreases xhs
{
  match xhs
  case CNil => 0
  case Single(a) => a
  case Concat(x, y) => join(target(x), target(y))
}

function s0() : int

{
  0
}

function f0(x : int) : int
decreases x
{
  x
}

function clen(cl: clist): int
    ensures clen(cl) >= 0
{
    match cl
    case CNil => 1
    case Single(a) => 1
    case Concat(x, y) => clen(x) + clen(y)
}

function join(x0 : int, x1 : int) : int
decreases x1
{
  x0 + x1
}
lemma l1(x: clist<int>, y: clist<int>)
ensures spec(rep(Concat(x, y))) == join(target(x), target(y))
decreases clen(x)+clen(y), x, y // manually added
{
  match x
  case CNil => {
    match y
    case CNil => {}
    case Single(b)=>{
      calc == {
        spec(rep(Concat(x, y))); 
        spec(dec(Single(b), CNil)); // expansion of rep + x + y
        spec(rep(Single(b))); // expansion of dec
        // spec(Cons(b, Nil)); // expansion of rep
        b; // expansion of spec (1 step skipped)
        join(target(x), target(y));
      }
    }
    case Concat(y1, y2) => {}
  }
  case Single(a) => {
    match y
    case CNil => {
      calc == {
        spec(rep(Concat(x, y)));
        spec(dec(CNil, Single(a))); // expansion of rep + x + y
        spec(Cons(a, rep(CNil))); // expansion of dec
        // spec(Cons(a, Nil)); // expansion of rep
        a; // expansion of spec (1 step skipped)
        join(target(x), target(y));
      }
    }
    case Single(b)=>{
      calc == {
        spec(rep(Concat(x, y))); 
        spec(dec(Single(b), Single(a))); // expansion of rep + x + y
        spec(Cons(a, rep(Single(b)))); // expansion of dec
        // spec(Cons(a, Cons(b, Nil))); // another expansion of rep
        a + spec(Cons(b, Nil)); // expansion of spec
        a + b; // another expansion of spec
        join(target(x), target(y));
      }
    }
    case Concat(y1, y2) => {
      l1(y1, y2);
      calc == {
        spec(rep(Concat(x, y))); // start
        spec(dec(Concat(y1, y2), Single(a))); // 1 expansion of rep function + expansion of x, y
        spec(Cons(a, rep(Concat(y1, y2)))); // expansion of dec sub-function
        a + spec(rep(Concat(y1, y2))); // expansion of spec
        a + target(y1) + target(y2); // use of l1
        join(target(x), target(y)); // end goal
      }
    }
  }
  case Concat(x1, x2) => {
    l1(x1, x2);
    match y
    case CNil => {
      l1(x1, Concat(x2, CNil));
      calc == {
        spec(rep(Concat(x, y)));  // start
        spec(dec(CNil, Concat(x1, x2))); // expansion of rep function + expansion of x, y
        spec(dec(Concat(x2, CNil), x1)); // expansion of dec sub-function
        // spec(rep(Concat(x1, Concat(x2, CNil)))); // expansion of dec sub-function
        target(x1) + target(Concat(x2, CNil)); // use of l1
        join(target(x), target(y)); // end goal
      }
    }
    case Single(b) => {
      l1(x1, Concat(x2, Single(b)));
      calc == {
        spec(rep(Concat(x, y))); // start
        spec(dec(Single(b), Concat(x1, x2))); // expansion of rep + x + y
        spec(dec(Concat(x2, Single(b)), x1)); // expansion of dec
        // spec(rep(Concat(x1, Concat(x2, Single(b))))); // unexpansion of dec?
        target(x1) + target(Concat(x2, Single(b))); // use of l1
        join(target(x), target(y)); 
      }
    }
    case Concat(y1, y2) => {
      l1(x1, Concat(x2, Concat(y1, y2)));
      calc == {
        spec(rep(Concat(x, y)));
        spec(dec(Concat(y1, y2), Concat(x1, x2))); // expansion of rep + x + y
        spec(dec(Concat(x2, Concat(y1, y2)), x1)); // expansion of dec
        // spec(rep(Concat(x1, Concat(x2, Concat(y1, y2))))); // unexpansion of dec?
        target(x1) + target(Concat(x2, Concat(y1, y2))); // use of l1
        join(target(x), target(y));
      }
    } 
  }
}

lemma correctness_lemma(x : clist<int>) 
ensures (target(x) == spec(rep(x)))
{
  match x
  case CNil => {}
  case Single(a) => {}
  case Concat(x, y) => {
    l1(x, y);
  }
}
