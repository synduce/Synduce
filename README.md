# Atropos

Automatic recursive function synthesis.

# Examples
The `benchmarks` folder contains input examples. An input problem is defined by three components: a reference function `spec`, a representation function `repr` and a recursion skeleton `target`.
The datatypes on which each of these components operate have to be defined first.

For example, in the file `benchmarks/list/sumhom.pmrs` we have the following type definitions:
```
type 'a clist = CNil | Single of 'a | Concat of 'a clist * 'a clist

type 'a list = Nil | Cons of 'a * 'a list
```
The first type is the type of lists built with the concatenation operator from empty lists or singletons. The second type is more natural: it is the type of cons-lists. With these two types we can specify problems that consists in parallelizing functions on lists. The reference function can be defined on cons-lists, for example the function that adds all the elements of the list:
```
pmrs spec =
    main l -> f l
    | f Nil -> 0
    | f Cons(hd,tl) -> hd + (f tl)
```
The recursion scheme `spec` recursively computes the sum of the elements. If the input is `Nil`, the first rule is used and the result is `0`. If the input is a `Cons`, the head `hd` is added to the result of the recursive call `(f tl)`.
We want to synthesize a list homomorphism, that is, a function on concatenation lists. The following recursion scheme models a list homomorphism:
```
pmrs (odot,f_0,s_0) target =
    target t           -> h t
    | h CNil          -> s_0
    | h Single(a)    -> f_0 a
    | h Concat(x, y) -> odot (h x) (h y)
```
The function `odot`, `f_0` and the constant value `s_0` have to be synthesized.
However, *Atropos* has no knowledge of how to translate the inputs of `target` to inputs of `spec`. Here, lists are merely recursive types. The user has to provide the representation function, modeled by a recursion scheme. `repr : 'a clsit -> 'a list` is defined as follows:
```
pmrs repr =
    repr l -> c l
    | c CNil -> Nil
    | c Single(a) -> Cons(a, Nil)
    | c Concat(x, y) -> dec y x
    | dec l CNil -> repr l
    | dec l Single(a) -> Cons(a, repr l)
    | dec l Concat(x, y) -> dec (Concat(l, y)) x
```
Note that the representation function and the target recursion skeleton can be reused across a large set of examples. All the benchmarks in`benchmarks/list` use the same `repr` and `target`, modulo some changes in the base case when lists have a minimum size.
Running `./atropos benchmarks/list/sumhom.pmrs` returns the solution in less than a second:
```
pmrs target =
      target t   -> h t
      | h  CNil  -> 0
      | h  Single(a)  -> a + 0
      | h  Concat(x, y)  -> (h x) + (h y)

```


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
- `./benchmarks/` contains benchmarks and sample inputs. `parse_examples/parsing.pmrs` is an example of the input syntax for the PMRS with recursive type definitions. The syntax is similar to Caml, except for the recursion scheme declarations.

