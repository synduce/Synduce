open Base

let read_sig filename =
    Location.input_name := filename ;
    let handle =
      try Stdio.In_channel.create filename
      with Sys_error msg -> Stdio.prerr_endline msg; Caml.exit 1
    in
    let buf = Lexing.from_channel handle in
    Location.init buf filename ;
    let ast = Parse.implementation buf in
    Stdio.In_channel.close  handle ;
    ast
  ;;

ignore (read_sig "../../../tests/inputs/example.ml")