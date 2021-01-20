# ReFunS

Automatic recursive function synthesis.

# Requirements
You will need [OCaml 4.11.1](https://ocaml.org/releases/4.11.1.html) and the [OCaml Package Manager (opam)](https://opam.ocaml.org) to get started.

The dependencies of this project can be installed via opam (```opam install . --deps-only```).
Once all the dependencies are installed, call ```make``` from the root of the project. The Makefile simply calls dune build and creates a shortcut to the binary executable.


## Installation script on Ubuntu:
This small script should work for an installation from scratch on Ubuntu, or any system with the apt package manager.
```
sudo apt install opam
opam init
eval $(opam env)
opam switch create 4.11.1
eval $(opam env)
opam install . --deps-only
make
```
The above should work for OSX with `brew` instead of `(sudo) apt`, but this has not been tested.

# Folder structure

- `./bin/` contains all the sources for the executable,
- `./src/` contains all the sources for the libraries. The `lang` folder is where you will find most of the language definitions.
- `./benchmarks/` contains benchmarks and sample inputs. `parse_examples/parsing.pmrs` is an example of the input syntax for the PMRS with recursive type definitions. The syntax is similar ta Caml language, the specificity is the pmrs declaration.