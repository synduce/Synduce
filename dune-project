(lang dune 3.10)

(name Synduce)

(version 0.2)

(license MIT)

(authors "Victor Nicolet" "Danya Lette")

(maintainers "victorn@cs.toronto.edu")

(homepage "https://github.com/victornicolet/Synduce")

(bug_reports "https://github.com/victornicolet/Synduce/issues")

(generate_opam_files true)

(using menhir 2.0)

(package
 (name Synduce)
 (synopsis "Recursive Function Synthesizer")
 (description
  "Executable and libraries for automatic synthesis of recursive functions.")
 (depends
  (alcotest
   (>= 1.5))
  (base
   (>= v0.16.0))
  (core
   (>= v0.16.0))
  (core_unix
   (>= v0.16.0))
  (extlib
   (>= 1.7.9))
  (fileutils
   (>= 0.6.4))
  (fmt
   (>= 0.9.0))
  (getopt
   (>= 20230213))
  (lwt
   (>= 5.6.0))
  (lwt_ppx
   (>= 2.1.0))
  (menhir
   (>= 20230608))
  (odoc
   (>= 2.2.0))
  (ocamlgraph
   (>= 2.0.0))
  (parsexp
   (>= v0.16.0))
  (parsexp_io
   (>= v0.16.0))
  (ppx_hash
   (>= v0.16.0))
  (ppx_let
   (>= v0.16.0))
  (stdio
   (>= v0.16.0))
  (ppx_deriving
   (>= 5.2))
  (ppx_sexp_conv
   (>= v0.16.0))
  (ppx_optcomp
   (>= v0.16.0))
  (sexplib
   (>= v0.16.0))
  (yojson
   (>= 2.1.0))))
