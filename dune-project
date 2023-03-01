(lang dune 2.9)

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
   (>= v0.14.0))
  (core
   (>= v0.14.0))
  (core_unix
   (and
    (>= v0.14.0)))
  (extlib
   (>= 1.7.7))
  (fileutils
   (>= 0.6.0))
  (fmt
   (>= 0.8.8))
  (getopt
   (>= 20120615))
  (lwt
   (>= 5.4.0))
  (lwt_ppx
   (>= 2.0.0))
  (menhir
   (>= 20220210))
  (odoc
   (>= 1.5.0))
  (ocamlgraph
   (>= 2.0.0))
  (parsexp
   (>= v0.14.0))
  (parsexp_io
   (>= v0.14.0))
  (ppx_hash
   (>= v0.14.0))
  (ppx_let
   (>= v0.14.0))
  (stdio
   (>= v0.14.0))
  (ppx_deriving
   (>= 5.2))
  (ppx_sexp_conv
   (>= v0.14.0))
  (ppx_optcomp
   (>= v0.14.0))
  (sexplib
   (>= v0.14.0))
  (yojson
   (>= 1.6.0))))
