datatype clist = Single(int) | Concat(clist, clist)
datatype list = Elt(int) | Cons(int, list)

function min(a:int, b:int): (x: int)
    ensures a == x || x == b
    ensures x <= a && x <= b
{
    if a >= b then b else a
}

function spec(l: list): int
{
    match l
    case Elt(a) => a
    case Cons(hd, tl) => min(hd, spec(tl))
}

function rep(cl: clist): list
    decreases clen(cl), cl
{
    match cl
    case Single(a) => Elt(a)
    case Concat(x, y) => dec(y, x)
}
function dec(l1: clist, l2: clist): list
    decreases clen(l1) + clen(l2), l2
{
    match l2
    case Single(a) => Cons(a, rep(l1))
    case Concat(x, y) => dec(Concat(y, l1), x)
}
function clen(cl: clist): int
    ensures clen(cl) >= 0
{
    match cl
    case Single(a) => 1
    case Concat(x, y) => clen(x) + clen(y)
}

function f0(x: int): int
{
    x
}
function odot(x0:int, x1: int): int
{
    min(x0, x1)
}

function target(cl: clist): int
{
    match cl
    case Single(a) => f0(a)
    case Concat(x, y) => odot(target(x), target(y))
}
lemma minLemma(x: clist, y: clist)
ensures spec(dec(y, x)) == min(spec(rep(x)), spec(rep(y)))
decreases clen(x) + clen(y), x,y
{
    match x
    case Single(a) => {}
    case Concat(x1, x2) => {
        minLemma(x1, Concat(x2, y));
    }
}

lemma correctness(cl: clist)
ensures target(cl) == spec(rep(cl))
{
    match cl
    case Single(a) => {}
    case Concat(x, y)=>
    {
        minLemma(x, y);
    }
}