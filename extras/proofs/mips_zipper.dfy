datatype btree = Empty | Node(int, btree, btree)
datatype sel = Left | Right
datatype zipper = Top | Zip(sel, int, btree, zipper)


function spec(t: btree): (int, int)
{
    mips((0, 0), t)
}
function max(a:int, b:int): (x: int)
    ensures a == x || x == b
    ensures x >= a && x >= b
{
    if a >= b then a else b
}
function mips(s:(int, int), t: btree): (int, int)
    decreases t
{
    match t
    case Empty => s
    case Node(a, l, r) =>
        var (sum1, m1) := mips(s, l);
        mips((sum1+a, max(sum1+a, m1)), r)
}

function depth(zip: zipper): int
{
    match zip
    case Top => 0
    case Zip(w, lbl, child, z) => 0
}

function repr(zip:zipper): btree
    decreases zip
{
    match zip
    case Top => Empty
    case Zip(w, lbl, child, z) => h(lbl, child, z, w)
}


function h(lbl: int, child: btree, z: zipper, c: sel): btree
decreases z
{
    match c
    case Left => Node(lbl, child, repr(z))
    case Right => Node(lbl, repr(z), child)
}
function s0(): (int, int)
{
    (0, 0)
}

function joinl(x23: int, j: (int, int), j1: (int, int)): (int, int)
{
    (x23 + (j.0 + j1.0), max(j.1, (x23 + (j.0 + j1.1))))
}
function joinr(x23: int, j: (int, int), j1: (int, int)): (int, int)
{
    (x23 + (j.0 + j1.0), max(j1.1, (x23 + (j.1 + j1.0))))
}

function aux(a: int, child: btree, z: zipper, c: sel): (int, int)
    decreases z
{
    match c
    case Left => joinl(a, spec(child), target(z))
    case Right => joinr(a, spec(child), target(z))
}

function target(zip: zipper): (int, int)
decreases zip
{
    match zip
    case Top => s0()
    case Zip(c, a, child, z) => aux(a, child, z, c)
}

lemma correctness(zip:zipper)
ensures target(zip) == spec(repr(zip))
{
    match zip
    case Top => {}
    case Zip(c, a, child, z)=>
    {
        
    }
}