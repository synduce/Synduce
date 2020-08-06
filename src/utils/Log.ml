open Fmt


let wrap (s : string) =
  (fun fmt () -> string fmt s)

let wrap1 f s t = fun fmt () -> pf fmt f s t

let error (msg : Format.formatter -> unit -> unit) : unit =
  pf stdout "%a@;%a@." (styled (`Bg `Red) string) "[ERROR]" msg ()