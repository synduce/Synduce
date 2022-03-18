type t =
  { names : Alpha.t
  ; types : RType.env
  }

let create () = { names = Alpha.create (); types = RType.create () }
