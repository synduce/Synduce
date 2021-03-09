# Atropos

Automatic recursive function synthesis.
This is very much work in progress, but is reasonably functional.
CAV21 branch.
# Requirements
You will need a recent [OCaml](https://ocaml.org/releases/4.11.1.html) installation and the [OCaml Package Manager (opam)](https://opam.ocaml.org) to get started.

The Ocaml dependencies of this project can be installed via opam (```opam install . --deps-only```).
Once all the dependencies are installed, call ```make``` from the root of the project. The Makefile simply calls dune build and creates a shortcut to the binary executable.

You will need [**Z3**](https://github.com/Z3Prover/z3) and [**CVC4**](https://cvc4.github.io) installed on your system. *Atropos* detects where your binaries are using `which z3` and `which cvc4`.

## Installation script on Ubuntu:
This small script should work for an installation from scratch on Ubuntu, or any system with the apt package manager.
```
sudo apt install opam
opam init
eval $(opam env)
opam switch install ocaml-base-compiler
eval $(opam env)
opam install . --deps-only
make
```
The installation of the dependencies sometimes fails. If it does, try installing `core` on its own, and then try again.

## Basic Usage
`./atropos -h` should get you started.

# Examples
The `benchmarks` folder contains input examples. An input problem is defined by three components: a reference function `spec`, a representation function `repr` and a recursion skeleton `target`.
The datatypes on which each of these components operate have to be defined first.

For example, in the file `benchmarks/list/sum.ml` we have the following type definitions:
```ocaml
type 'a clist = CNil | Single of 'a | Concat of 'a clist * 'a clist

type 'a list = Nil | Cons of 'a * 'a list
```
The first type is the type of lists built with the concatenation operator from empty lists or singletons. The second type is more natural: it is the type of cons-lists. With these two types we can specify problems that consists in parallelizing functions on lists. The reference function can be defined on cons-lists, for example the function that adds all the elements of the list:
```ocaml
let rec sum =
    function
    | Nil -> 0
    | Cons(hd,tl) -> hd + (sum tl)
```
Our goal is to synthesize a function of type `int clist -> int`, but we have initially a function `int list -> int`. Let us write a function `clist_to_list : 'a clist -> 'a list`:
```ocaml
let rec clist_to_list  =
    function
    | CNil -> Nil
    | Single(a) -> Cons(a, Nil)
    | Concat(x, y) -> dec y x
and dec l1 =
    function
    | CNil -> clist_to_list l1
    | Single(a) -> Cons(a, clist_to_list l1)
    | Concat(x, y) -> dec (Concat(l1, y)) x
```
Two of the three components of the synthesis problem are defined. Now, let us write the recursion skeleton that needs to be synthesized. The function `hsum: int clist -> int` with unknowns `s0`, `f0` and `odot` defines a list homomorphism:
```ocaml
let rec hsum =
    function
    | CNil          -> s0
    | Single(a)    -> f0 a
    | Concat(x, y) -> odot (hsum x) (hsum y)
    [@defining s0 f0 odot]
    [@equiv sum clist_to_list ]
```
The extension `[@defining s0 f0 odot]` indicate that the functions `s0`, `f0` and `odot` need to be synthesized. The extension `[@equiv sum clist_to_list@]` indicates that they should be synthesized such that the resulting implementation of `hsum` is equivalent to `sum` composed with `clist_to_list`.

Note that the representation function and the target recursion skeleton can be reused across a large set of examples. All the benchmarks in `benchmarks/list` use the same `repr` and `target`, modulo some changes in the base case when lists have a minimum size.
Running `./atropos benchmarks/list/sum.ml` returns the solution in less than a second:
```ocaml
let s0 = 0
let f0 a = a + 0
let odot s1 s2 = s1 + s2
let rec hsum =
    function
    | CNil          -> s0
    | Single(a)    -> f0 a
    | Concat(x, y) -> odot (hsum x) (hsum y)
```

## Caml Syntax

The interface using the Caml syntax is still in development. See the `.ml` files in the benchmarks folder.
The PMRS syntax supports more input benchmarks.

## PMRS Syntax
`atropos` also uses a special syntax to specify recursion schemes (Pattern Matching Recursion Schemes). The files ending in `.pmrs` are examples of this syntax. In a `.pmrs` file, three recursion schemes need to be given: `spec` is the reference function, `repr` is the representation function and `target` is the recursion skeleton.
A recursion scheme (pmrs) follows the syntax
```
pmrs (unknowns*) pmrs_name pmrs_args {invariant} =
    | f x_1 ... x_n -> t
    | g y_1 ... y_m p -> t
    ...
```
- `(unknowns*)` is an optional list of string identifying the unknowns of the pmrs.
- `pmrs_name` is the name of the pmrs.
- `pmrs_args` are the non-recursible arguments of the pmrs. They cannot be matched. See for example the 'benchmarks/list/search.pmrs' for an example, where the argument is the integer that is searched for.
- `{invariant}` is an optional invariant of the function (for the reference function). `invariant` is a function of the form `fun (x1, x2, ...) -> expr(x1, x2, ...)` where `x1, x2, ..` is the output of the function, and `expr(x1, x2, ..)` a boolean expression. `invariant` indicates that `invariant(pmrs_name(x))` is true for any input `x` of the pmrs.
- `f x_1 ... x_n -> t` is a non-pattern matching rule with arguments `x_1 ... x_n` and contractum `t`.
- `g x_1 ... x_n p -> t` is a pattern matching rule with arguments `x_1 ... x_n` and pattern `p` and contractum `t`.



# Folder structure

- `./bin/` contains all the sources for the executable,
- `./src/` contains all the sources for the libraries. The `lang` folder is where you will find most of the language definitions.
- `./benchmarks/` contains benchmarks and sample inputs. `parse_examples/parsing.pmrs` is an example of the input syntax for the PMRS with recursive type definitions. The syntax is similar to Caml, except for the recursion scheme declarations.

