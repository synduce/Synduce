(lang dune 2.6)
(name AutoRec)
(version 0.1)
(license MIT)
(authors "Victor Nicolet")
(maintainers "victorn@cs.toronto.edu")

(generate_opam_files true)
(using menhir 2.0)


(package
    (name AutoRec)
    (synopsis "Automatic Synthesis of Recursive Functions")
    (description "Executable and libraries for automatic synthesis of recursive functions.")
    (depends
        (base (>= v0.14.0))
        (fmt (>= 0.8.8))
        (getopt (>= 20120615))
        (menhir (>= 20200624))
        (parsexp (>= v0.14.0))
        (ppx_let (>= v0.14.0))
        (sexplib (>= v0.14.0))))
