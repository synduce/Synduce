
open Base

let test_success ?(name = "test") () = Fmt.(pf stdout "%s success@." name)
let test_fail ?(name = "test") () = Fmt.(pf stdout "%s fail@." name)

let test_result (s : string) (r : (bool, 'a) Result.t) : unit = 
  match r with 
  | Ok true -> test_success ~name:s ()
  | _ -> test_fail ~name:s ()
