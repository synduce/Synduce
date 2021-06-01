(lang dune 2.6)

(name Synduce)

(version 0.1)

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
  (base
   (>= v0.14.0))
  (fmt
   (>= 0.8.8))
  (getopt
   (>= 20120615))
  (menhir
   (>= 20200624))
  (parsexp
   (>= v0.14.0))
  (ppx_let
   (>= v0.14.0))
  (stdio
   (>= v0.14.0))
  (ppx_sexp_conv
   (>= v0.14.0))
  (fileutils
   (>= 0.6.3))
  (extlib
   (>= 1.7.7))
  (lwt
   (>= 5.4.0))
  (sexplib
   (>= v0.14.0))))
