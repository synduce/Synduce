open Base
open Lang

type t = TermSet.t Hash_set.t

let create () = Hash_set.create (module TermSet)
