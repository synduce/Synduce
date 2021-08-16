datatype list<b0> =  Cons(int, list<b0>) |  Nil

datatype clist<a1> =  Concat(clist<a1>, clist<a1>) |  Single(a1) |  CNil

function clist_to_list(xclist_to_li : clist<int>) : (list<int>)
decreases xclist_to_li
{
  match xclist_to_li
  case CNil => Nil
  case Single(a) => Cons(a, Nil)
  case Concat(x, y) => dec(y, x)
}

function dec(xd : clist<int>, xd0 : clist<int>) : (list<int>)
decreases xd0
{
  match xd0
  case CNil => clist_to_list(xd)
  case Single(a) => Cons(a, clist_to_list(xd))
  case Concat(x, y) => dec(Concat(y, xd), x)
}

function sum(xs : list<int>) : (int)
decreases xs
{
  match xs
  case Nil => 0
  case Cons(hd, tl) => hd + sum(tl)
}

function hsum(xhs : clist<int>) : (int)
decreases xhs
{
  match xhs
  case CNil => 0
  case Single(a) => a
  case Concat(x, y) => join(hsum(x), hsum(y))
}

function s0() : (int)

{
  0
}

function f0(x : int) : (int)
decreases x
{
  x
}

function join(x0 : int, x1 : int) : (int)
decreases x1
{
  x0 + x1
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

lemma lemma0(p74 : clist<int>, p75 : clist<int>) 
ensures (sum(clist_to_list(Concat(p74, p75))) == hsum(Concat(p74, p75)))
{
  match p74
    case CNil =>
     {
       match p75
         case CNil =>
          {
            calc == {
              sum(clist_to_list(Concat(CNil, CNil)));
              sum(dec(CNil, CNil));
              sum(clist_to_list(CNil));
              0;
              hsum(Concat(CNil, CNil));
              } }
           case Single(i60) =>
            {
              calc == {
                sum(clist_to_list(Concat(CNil, Single(i60))));
                sum(dec(Single(i60), CNil));
                sum(clist_to_list(Single(i60)));
                i60 + sum(Nil);
                i60 + 0;
                hsum(Concat(CNil, Single(i60)));
                } }
             case Concat(p78, p79) =>
              {
                lemma0(p79, p78);
                  lemma0(p78, p79);
                  calc == {
                    sum(clist_to_list(Concat(CNil, Concat(p78, p79))));
                    sum(dec(Concat(p78, p79), CNil));
                    sum(clist_to_list(Concat(p78, p79)));
                    sum(dec(p79, p78));
                    hsum(Concat(CNil, Concat(p78, p79)));
                    } } }
        case Single(i59) =>
         {
           match p75
             case CNil =>
              {
                calc == {
                  sum(clist_to_list(Concat(Single(i59), CNil)));
                  sum(dec(CNil, Single(i59)));
                  i59 + sum(clist_to_list(CNil));
                  i59 + 0;
                  hsum(Concat(Single(i59), CNil));
                  } }
               case Single(i61) =>
                {
                  calc == {
                    sum(clist_to_list(Concat(Single(i59), Single(i61))));
                    sum(dec(Single(i61), Single(i59)));
                    i59 + sum(clist_to_list(Single(i61)));
                    i59 + i61 + sum(Nil);
                    i59 + i61 + 0;
                    hsum(Concat(Single(i59), Single(i61)));
                    } }
                 case Concat(p80, p81) =>
                  {
                    lemma0(p81, p80);
                      lemma0(p80, p81);
                      calc == {
                        sum(clist_to_list(Concat(Single(i59), Concat(p80,
                              p81))));
                        sum(dec(Concat(p80, p81), Single(i59)));
                        i59 + sum(clist_to_list(Concat(p80, p81)));
                        i59 + sum(dec(p81, p80));
                        hsum(Concat(Single(i59), Concat(p80, p81)));
                        } } }
            case Concat(p76, p77) =>
             {
               match p75
                 case CNil =>
                  {
                    lemma0(p77, Concat(p77, CNil));
                      lemma0(p77, p76);
                      lemma0(Concat(p77, CNil), p77);
                      lemma0(Concat(p77, CNil), p76);
                      lemma0(p76, p77);
                      lemma0(p76, Concat(p77, CNil));
                      calc == {
                        sum(clist_to_list(Concat(Concat(p76, p77), CNil)));
                        sum(dec(CNil, Concat(p76, p77)));
                        sum(dec(Concat(p77, CNil), p76));
                        hsum(Concat(Concat(p76, p77), CNil));
                        } }
                   case Single(i62) =>
                    {
                      lemma0(p77, Concat(p77, Single(i62)));
                        lemma0(p77, p76);
                        lemma0(Concat(p77, Single(i62)), p77);
                        lemma0(Concat(p77, Single(i62)), p76);
                        lemma0(p76, p77);
                        lemma0(p76, Concat(p77, Single(i62)));
                        calc == {
                          sum(clist_to_list(Concat(Concat(p76, p77),
                                Single(i62))));
                          sum(dec(Single(i62), Concat(p76, p77)));
                          sum(dec(Concat(p77, Single(i62)), p76));
                          hsum(Concat(Concat(p76, p77), Single(i62)));
                          } }
                     case Concat(p82, p83) =>
                      {
                        lemma0(p77, p82);
                          lemma0(p77, p83);
                          lemma0(p77, Concat(p77, Concat(p82, p83)));
                          lemma0(p77, p76);
                          lemma0(p82, p77);
                          lemma0(p82, p83);
                          lemma0(p82, Concat(p77, Concat(p82, p83)));
                          lemma0(p82, p76);
                          lemma0(p83, p77);
                          lemma0(p83, p82);
                          lemma0(p83, Concat(p77, Concat(p82, p83)));
                          lemma0(p83, p76);
                          lemma0(Concat(p77, Concat(p82, p83)), p77);
                          lemma0(Concat(p77, Concat(p82, p83)), p82);
                          lemma0(Concat(p77, Concat(p82, p83)), p83);
                          lemma0(Concat(p77, Concat(p82, p83)), p76);
                          lemma0(p76, p77);
                          lemma0(p76, p82);
                          lemma0(p76, p83);
                          lemma0(p76, Concat(p77, Concat(p82, p83)));
                          calc == {
                            sum(clist_to_list(Concat(Concat(p76, p77),
                                  Concat(p82, p83))));
                            sum(dec(Concat(p82, p83), Concat(p76, p77)));
                            sum(dec(Concat(p77, Concat(p82, p83)), p76));
                            hsum(Concat(Concat(p76, p77), Concat(p82, p83)));
                            } } }
             }

lemma correctness_lemma(x : clist<int>) 
ensures (hsum(x) == sum(clist_to_list(x)))
{
  match x
    case CNil => assert(true);
    case Single(i58) => assert(true);
    case Concat(p74, p75) => lemma0(p74, p75); }
