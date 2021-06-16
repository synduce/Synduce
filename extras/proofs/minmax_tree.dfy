datatype tree = Node(int, tree, tree) | Leaf(int)

function rep(t: tree):tree
{
    t
}


function f(t: tree): (int, int)
{
    match t
    case Leaf(x) => (x, x)
    case Node(a, l, r) => 
        var (amin, amax) := g((a, a), l);
        g((amin, amax), r)

}
function min(a:int, b:int): (x: int)
    ensures a == x || x == b
    ensures x <= a && x <= b
{
    if a >= b then b else a
}
function max(a:int, b:int): (x: int)
    ensures a == x || x == b
    ensures x >= a && x >= b
{
    if a >= b then a else b
}
function g(s: (int, int), t: tree): (out: (int, int))
    decreases t
    ensures out.0 == min(s.0, h(t).0)
    ensures out.1 == max(s.1, h(t).1)
{
    match t
    case Leaf(x) =>
        var (amin, amax) := s;
        (min(amin, x), max(amax, x))
    case Node(a, l, r) =>
        var (amin, amax) := s;
        g(g((min(amin, a), max(amax, a)), l), r)
}
function spec(t: tree): (int, int)
{
    f(t)
}

function target(t1: tree): (int, int)
{
    h(t1)
}

// generated from Synduce
function f0(x15: int): (int, int)
{
    (x15, x15)
}

function join (x17: int, t1: (int, int), t2: (int, int)): (int, int)
    ensures forall x:int :: join(x17, t1, t2).0 == x ==> x <= t1.0 && x <= t2.0 && x <= x17
{
    (min(x17, min(t1.0, t2.0)), max(x17, (max(t1.1, t2.1))))
}
function h(t: tree): (int, int)
{
    match t
    case Leaf(x) => 
        (x, x)
    case Node (a, l, r) =>
        join(a, h(l), h(r))
}

lemma minLemma(t: tree)
ensures target(t).0 == spec(rep(t)).0
{
    match t
    case Leaf(x) =>
    {}
    case Node(a, l, r)=>
    {
        minLemma(l);
        minLemma(r);

    }
}
lemma maxLemma(t: tree)
ensures target(t).1 == spec(rep(t)).1
{
    match t
    case Leaf(x) =>
    {}
    case Node(a, l, r)=>
    {
        maxLemma(l);
        maxLemma(r);
    }
}



lemma correctness(t: tree)
ensures target(t) == spec(rep(t))
{
    match t
    case Leaf(x) =>
    {}
    case Node(a, l, r)=>
    {
        minLemma(t);
        maxLemma(t);
    }
}