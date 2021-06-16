datatype clist = cnil | single(int) | concat(clist, clist)
datatype list = nil | cons(int, list)

function rep(cl: clist): list
    decreases clen(cl), cl
    // ensures len(rep(cl)) == clen(cl)
{
    match cl
    case cnil => nil
    case single(a) => cons(a, nil)
    case concat(x, y) => dec(y, x)
}
function dec(l1: clist, l2: clist): list
    decreases clen(l1) + clen(l2), l2
    // ensures len(dec(l1, l2)) == clen(l1) + clen(l2)
{
    match l2
    case cnil => rep(l1)
    case single(a) => cons(a, rep(l1))
    case concat(x, y) => dec(concat(y, l1), x)
}



function spec(l: list):int
{
    match l
    case nil => 0
    case cons(hd, tl) =>
        hd + spec(tl)
}

function clen(cl: clist): int
    ensures clen(cl) >= 0
{
    match cl
    case cnil => 1
    case single(a) => 1
    case concat(x, y) => clen(x) + clen(y)
}
function len(l: list):int
    ensures len(l) >=0
{
    match l
    case nil => 0
    case cons(head, tail)=> 1+ len(tail)
}

function target(cl : clist): int
{
    match cl
    case cnil => 0
    case single(a) => a
    case concat(x, y) => target(x) + target(y)
}

lemma sumLemma(x:clist, y:clist)
    // ensures target(concat(x, y)) == spec(rep(concat(x, y)))
    ensures spec(dec(y, x)) == target(y) + target(x)
    decreases clen(x)+clen(y), x, y
{
    match x
    case cnil => {
        match y
        case cnil => {}
        case single(b) => {}
        case concat(y1, y2) => {
            sumLemma(y1, y2);
        }
    }
    case single(a) => {
        match y
        case cnil => {}
        case single(b) => {
            calc == {
                spec(dec(y, x));
                spec(cons(b, cons(a, nil)));
                b + spec(cons(a, nil));
                b + a;
                target(y) + target(x);
            }
        }
        case concat(y1, y2) => {
            sumLemma(y1, y2);
        }
    }
    case concat(x1, x2) => {
        match y
        case cnil => {
            assert target(concat(x2, y)) == target(x2);
            sumLemma(x1, concat(x2, y));
            calc == {
                spec(dec(y, x));
                spec(dec(concat(x2, y), x1));
                target(concat(x2, y)) + target(x1);
                target(x2) + target(x1);
            }
        }
        case single(b) => {
            assert target(concat(x2, y)) == b + target(x2);
            sumLemma(x1, concat(x2, y));
            calc == {
                spec(dec(y, x));
                spec(dec(concat(x2, y), x1));
                target(concat(x2, y)) + target(x1);
                b + target(x2) + target(x1);
            }
        }
        case concat(y1, y2) => {
            assert target(concat(x2, y)) == target(y) + target(x2);
            sumLemma(x1, concat(x2, y));
            calc == {
                spec(dec(y, x));
                spec(dec(concat(x2, y), x1));
                target(concat(x2, y)) + target(x1);
                target(y) + target(x2) + target(x1);
            }
        }
    }
}


lemma correctness(cl: clist)
ensures target(cl) == spec(rep(cl))
{
    match cl
    case cnil => {}
    case single(a) => {}
    case concat(x, y)=>
    {
        sumLemma(x, y);
    }
}