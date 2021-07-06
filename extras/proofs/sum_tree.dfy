datatype tree = nil | node(int, tree, tree)

function rep(t: tree):tree
{
    t
}

function f(s: int, t: tree): int
    decreases t
{
    match t
    case nil => s
    case node(a, l, r) =>
        var sum := f(s, l);
        f(sum+a, r)

}
function spec(t: tree):int
{
    f(0, t)
}

function target(t: tree): int
    decreases t
{
    match t
    case nil => 0
    case node(a, l, r)=> a + target(l) + target(r)
}
lemma sumLemma(s: int, t: tree)
    ensures f(s, t) == s + target(t)
    decreases t 
{
    match t
    case nil => {}
    case node(a, l, r) =>
    {
        sumLemma(s, l);
        sumLemma(s + target(l) + a, r);
        calc == {
            f(s, t);
            f(f(s, l) + a, r);
            f(s + target(l) + a, r);
            s + target(l) + a + target(r);
            s + target(t);
        }
    }
}
lemma correctness(t: tree)
ensures target(t) == spec(rep(t))
{
    match t
    case nil => {}
    case node(a, l, r) =>
    {
        sumLemma(0, t);
    }
}