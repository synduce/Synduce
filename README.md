# AutoRec

Automatic recursive function synthesis.

# Requirements
You will need [OCaml 4.10.0](https://ocaml.org/releases/4.10.0.html) and the [OCaml Package Manager (opam)](https://opam.ocaml.org) to get started.

The dependencies of this project can be installed via opam (```opam install . --deps-only```).
Once all the dependencies are installed, call ```dune build``` from the root of the project.


## Installation script on Ubuntu:
```
sudo apt install opam
opam init
eval $(opam env)
opam switch create 4.10.0
eval $(opam env)
opam install . --deps-only
make
```