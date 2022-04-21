open Alcotest
open Base
open Utils

module TestableLL = struct
  type t = int list list

  let equal : t -> t -> bool = List.equal (List.equal Int.equal)
  let pp : t Fmt.t = Fmt.(brackets (list ~sep:semi (brackets (list ~sep:semi int))))
end

let ilistlist = TestableLL.(testable pp equal)
let ilist = testable Fmt.(brackets (list ~sep:comma int)) (List.equal Int.equal)

let test_cartesian_nary_product =
  let empty_list_test () =
    (check ilistlist) "with_empty_list" (cartesian_nary_product [ []; [ 0; 1 ]; [] ]) []
  in
  let empty_list_test_2 () =
    (check ilistlist)
      "with_empty_list_2"
      (cartesian_nary_product [ [ 1 ]; [ 0; 1 ]; [] ])
      []
  in
  let empty_list_test_3 () =
    (check ilistlist)
      "with_empty_list_3"
      (cartesian_nary_product [ []; [ 0; 1 ]; [ 1 ] ])
      []
  in
  let non_empty_list_test_1 () =
    (check ilistlist)
      "non_empty_list_1"
      (cartesian_nary_product [ [ 1 ]; [ 2; 3 ]; [ 4 ] ])
      [ [ 1; 2; 4 ]; [ 1; 3; 4 ] ]
  in
  let non_empty_list_test_2 () =
    (check ilistlist)
      "non_empty_list_2"
      (cartesian_nary_product [ [ 1; 2 ]; [ 2; 3 ]; [ 4 ] ])
      [ [ 1; 2; 4 ]; [ 2; 2; 4 ]; [ 1; 3; 4 ]; [ 2; 3; 4 ] ]
  in
  let non_empty_list_test_3 () =
    (check ilistlist)
      "non_empty_list_3"
      (cartesian_nary_product [ [ 1; 2 ]; [ 2; 3 ]; [ 4; 5 ] ])
      [ [ 1; 2; 4 ]
      ; [ 2; 2; 4 ]
      ; [ 1; 3; 4 ]
      ; [ 2; 3; 4 ]
      ; [ 1; 2; 5 ]
      ; [ 2; 2; 5 ]
      ; [ 1; 3; 5 ]
      ; [ 2; 3; 5 ]
      ]
  in
  [ test_case "empty_list" `Quick empty_list_test
  ; test_case "empty_list" `Quick empty_list_test_2
  ; test_case "empty_list" `Quick empty_list_test_3
  ; test_case "non_empty_list" `Quick non_empty_list_test_1
  ; test_case "non_empty_list" `Quick non_empty_list_test_2
  ; test_case "non_empty_list" `Quick non_empty_list_test_3
  ]
;;

let test_sub_list =
  let t1 a b c () = (check ilist) "sub_indexing" (sub_list a b) c in
  [ test_case "sub_list_empty" `Quick (t1 [] [] [])
  ; test_case "sub_list_neg_empty" `Quick (t1 [] [ -1 ] [])
  ; test_case "sub_list_pos_empty" `Quick (t1 [] [ 1 ] [])
  ; test_case "sub_list_zero" `Quick (t1 [ 1 ] [ 0 ] [ 1 ])
  ; test_case "sub_list_simple" `Quick (t1 [ 1; 2; 3 ] [ 0; 1 ] [ 1; 2 ])
  ; test_case "sub_list_last_2" `Quick (t1 [ 11; 12; 13 ] [ 1; 2 ] [ 12; 13 ])
  ; test_case "sub_list_out_of_bounds" `Quick (t1 [ 11; 12; 13 ] [ 2; 3 ] [ 13 ])
  ; test_case "sub_list_last_2_neg" `Quick (t1 [ 11; 12; 13; 14 ] [ 2; 3; -1 ] [ 13; 14 ])
  ; test_case "sub_list_neg_first" `Quick (t1 [ 11; 12; 13; 14 ] [ -2; 3 ] [ 14 ])
  ; test_case "sub_list_neg_last" `Quick (t1 [ 11; 12; 13; 14 ] [ 2; -1 ] [ 13 ])
  ; test_case "sub_list_neg_last_oob" `Quick (t1 [ 11; 12; 13; 14 ] [ 6; -1 ] [])
  ]
;;

let () =
  run
    "Utils"
    [ "cartesian_nary_product", test_cartesian_nary_product; "sub_list", test_sub_list ]
;;
