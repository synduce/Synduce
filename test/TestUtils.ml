open Alcotest
open Base
open Utils

module TestableLL = struct
  type t = int list list

  let equal : t -> t -> bool = List.equal (List.equal Int.equal)
  let pp : t Fmt.t = Fmt.(brackets (list ~sep:semi (brackets (list ~sep:semi int))))
end

let ilistlist = TestableLL.(testable pp equal)

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

let () = run "Utils" [ "cartesian_nary_product", test_cartesian_nary_product ]
