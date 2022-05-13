open Base
open Lang
open Rewriter
open Expression

let assert_fac_expand e1 e2 =
  let fe2 = factorize e2 in
  let ee1 = expand e1 in
  let b1 = equal e1 fe2 in
  let b2 = equal ee1 e2 in
  Alcotest.(check bool) "fac_expand" (b1 && b2) true
;;

let assert_eequals e1 e2 = Alcotest.(check bool) "eequals" (eequals e1 e2) true

let assert_is_subexpr ?(lemma = None) e1 e2 =
  let b = Option.is_some (match_as_subexpr ~lemma (Position 0) e1 ~of_:e2) in
  Alcotest.(check bool) "subexpr" b true
;;

let test_matches ?(b = true) s e1 e2 =
  let assert_matches e1 e2 =
    let b' =
      match matches e1 e2 with
      | Some _ -> true
      | None -> false
    in
    Alcotest.(check bool) "matches" b b'
  in
  Alcotest.(test_case s `Quick (fun () -> assert_matches e1 e2))
;;

let () =
  let a, b, c, u, v, w, x, y, z =
    let open Op in
    var 1, var 2, var 3, var 4, var 5, var 6, var 7, var 8, var 9
  in
  let open Alcotest in
  run
    "Rewriting"
    [ (* Addition, substraction *)
      ( "add-sub"
      , [ test_case "a+0 = a" `Quick (fun () -> assert_eequals Op.(a + int 0) a)
        ; test_case "a+b+0=a+b" `Quick (fun () ->
              assert_eequals Op.(a + (b + int 0)) Op.(a + b))
        ; test_case "x+y=y+x" `Quick (fun () -> assert_eequals Op.(x + y) Op.(y + x))
        ; test_case "u-0=u" `Quick (fun () -> assert_eequals Op.(u - int 0) u)
        ; test_case "a+b+c=c+b+a" `Quick (fun () ->
              assert_eequals Op.(a + b + c) Op.(c + b + a))
        ] )
    ; (* Multiplication *)
      ( "mult"
      , [ test_case "x*y*z=z*y*x" `Quick (fun () ->
              assert_eequals Op.(x * y * z) Op.(z * y * x))
        ; test_case "a*(b+c)=a*b+a*c" `Quick (fun () ->
              assert_eequals Op.(a * (b + c)) Op.((a * b) + (a * c)))
        ; test_case "(x+y)*(b+c)=x*b+y*b+x*c+y*c" `Quick (fun () ->
              assert_eequals
                Op.((x + y) * (b + c))
                Op.((x * b) + (y * b) + (x * c) + (y * c)))
        ] )
    ; (* Min and Max *)
      ( "min-max"
      , [ test_case "max(a,b)=max(b,a)" `Quick (fun () ->
              assert_eequals Op.(max a b) Op.(max b a))
        ; test_case "min(x,y)=min(y,x)" `Quick (fun () ->
              assert_eequals Op.(min x y) Op.(min y x))
        ; test_case "max(v,max(w,z))=max(max(v,z),w)" `Quick (fun () ->
              assert_eequals Op.(max v (max w z)) Op.(max (max v z) w))
        ; test_case "max(max(x,0)+y,0)=max(max(x+y,y),0)" `Quick (fun () ->
              assert_eequals
                Op.(max (max x (int 0) + y) (int 0))
                Op.(max (max (x + y) y) (int 0)))
        ; test_case "a>b?a:b=max(a,b)" `Quick (fun () ->
              assert_eequals Op.(~?(a > b) a b) Op.(max a b))
        ; test_case "a<b?a:b=min(a,b)" `Quick (fun () ->
              assert_eequals Op.(~?(a < b) a b) Op.(min a b))
        ; test_case "a<=b?a:b=min(a,b)" `Quick (fun () ->
              assert_eequals Op.(~?(a <= b) a b) Op.(min a b))
        ; test_case "a>=b?a:b=max(a,b)" `Quick (fun () ->
              assert_eequals Op.(~?(a >= b) a b) Op.(max a b))
        ; test_case "b<a?a:b=max(a,b)" `Quick (fun () ->
              assert_eequals Op.(~?(b < a) a b) Op.(max a b))
        ; test_case "b>a?a:b=min(a,b)" `Quick (fun () ->
              assert_eequals Op.(~?(b > a) a b) Op.(min a b))
        ] )
    ; (* If-then-else *)
      ( "if-then-else"
      , [ test_case "complex-if" `Quick (fun () ->
              assert_eequals
                Op.(~?(x > int 1) (x + ~?(y > int 0) y (int 0)) (~?(y > int 0) y (int 0)))
                Op.(~?(x > int 1) x (int 0) + ~?(y > int 0) y (int 0)))
        ; test_case "(a?x:y)>x=!a&&y>x" `Quick (fun () ->
              assert_eequals Op.(~?a x y > x) Op.((not a) && y > x))
        ; test_case "a?x:y>y=a&&x>y" `Quick (fun () ->
              assert_eequals Op.(~?a x y > y) Op.(a && x > y))
        ] )
    ; (* And, or *)
      ( "and-or"
      , [ test_case "x&&true=x" `Quick (fun () -> assert_eequals Op.(x && ETrue) x)
        ; test_case "x&&y&&false=false" `Quick (fun () ->
              assert_eequals Op.(x && y && EFalse) EFalse)
        ; test_case "x||true||y=true" `Quick (fun () ->
              assert_eequals Op.(x || ETrue || y) ETrue)
        ; test_case "x||false=x" `Quick (fun () -> assert_eequals Op.(x || EFalse) x)
        ; test_case "x||false||y=x||y" `Quick (fun () ->
              assert_eequals Op.(x || EFalse || y) Op.(x || y))
        ] )
    ; (* Implies *)
      ( "implies"
      , [ test_case "x&&x=>y=x&&y" `Quick (fun () ->
              assert_eequals Op.(x && x => y) Op.(x && y))
        ; test_case "x&&y&&x=>y=x&&y" `Quick (fun () ->
              assert_eequals Op.((x && y) && x => y) Op.(x && y))
        ; test_case "x&&!x=>y=x" `Quick (fun () ->
              assert_eequals Op.(x && (not x) => y) x)
        ; test_case "x&&z&&!x=>y=x&&z" `Quick (fun () ->
              assert_eequals Op.(x && z && (not x) => y) Op.(x && z))
        ] )
    ; ( "subexpression"
      , [ test_case "a < a+ b" `Quick (fun () -> assert_is_subexpr a Op.(a + b))
        ; test_case "a < a + b + c" `Quick (fun () -> assert_is_subexpr a Op.(a + b + c))
        ; test_case "a + 0 < a + b" `Quick (fun () ->
              assert_is_subexpr Op.(a + int 0) Op.(a + b))
        ; test_case "a + b + 0 < a + b + c" `Quick (fun () ->
              assert_is_subexpr Op.(a + b + int 0) Op.(a + b + c))
        ; test_case "a + 1 < a + 1 + 0 + b + v" `Quick (fun () ->
              assert_is_subexpr Op.(a + int 1) Op.(a + int 1 + int 0 + b + v))
        ; test_case "u * (v + 1) < (u * v) + u + (u*v*w)" `Quick (fun () ->
              assert_is_subexpr Op.(u * (v + int 1)) Op.((u * v) + u + (u * v * w)))
        ; test_case
            "max(max(x + 0,0)+y),0) < max(x+y+z,max(x+y, max(y,0)))"
            `Quick
            (fun () ->
              assert_is_subexpr
                Op.(max (max (x + int 0) (int 0) + y) (int 0))
                Op.(max (x + y + z) (max (x + y) (max y (int 0)))))
        ] )
    ; ( "matches"
      , [ test_matches "(a + b) matches (x + y)" Op.(a + b) Op.(x + y)
        ; test_matches
            "Cons(a,b) matches Cons(x,y)"
            Op.("Cons" <: [ a; b ])
            Op.("Cons" <: [ x; y ])
        ; test_matches "a[0] matches x[0]" Op.(a @: 0) Op.(x @: 0)
        ; test_matches
            "Cons(a[0],b) matches Cons(x[0],y)"
            Op.("Cons" <: [ a @: 0; b ])
            Op.("Cons" <: [ x @: 0; y ])
        ; test_matches
            ~b:false
            "Cons(a,b) not matches Cons(x,x)"
            Op.("Cons" <: [ a; b ])
            Op.("Cons" <: [ x; x ])
        ] )
    ; ( "fac-expand"
      , [ test_case "u*(v+w) -> u*v + u*w" `Quick (fun () ->
              assert_fac_expand Op.(u * (v + w)) Op.((u * v) + (u * w)))
        ; test_case "x + max(y,z) -> max(x+y,x+z)" `Quick (fun () ->
              assert_fac_expand Op.(x + max y z) Op.(max (x + y) (x + z)))
        ; test_case "x+ a?b:c ->a?x+b:x+c" `Quick (fun () ->
              assert_fac_expand Op.(x + ~?a b c) Op.(~?a (x + b) (x + c)))
        ; test_case "x+max(y+max(z,0),0) -> max(x,max(x+y, x+y+z))" `Quick (fun () ->
              assert_fac_expand
                Op.(normalize (x + max (y + max z (int 0)) (int 0)))
                Op.(normalize (max x (max (x + y) (x + y + z)))))
        ] )
    ]
;;
