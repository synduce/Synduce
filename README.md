# Atropos

## Requirements and building
The script `setup.sh` installs all the dependencies and packages from a fresh Ubuntu installation.

You will need [**Z3**](https://github.com/Z3Prover/z3) and [**CVC4**](https://cvc4.github.io) installed on your system. *Atropos* detects where your binaries are using `which z3` and `which cvc4`. Atropos needs a CVC4 version >= 1.8.


You will need a recent [OCaml](https://ocaml.org/releases/4.11.1.html) installation and the [OCaml Package Manager (opam)](https://opam.ocaml.org) to get started.

The Ocaml dependencies of this project can be installed via opam (```opam install . --deps-only```).
Once all the dependencies are installed, call ```make``` from the root of the project. The Makefile simply calls dune build and creates a shortcut to the binary executable.



# Folder structure

- `./bin/` contains all the sources for the executable,
- `./src/` contains all the sources for the libraries. The `lang` folder is where you will find most of the language definitions and `algo` the algorithmic components.
- `./benchmarks/` contains benchmarks and sample inputs. `parse_examples/parsing.pmrs` is an example of the input syntax for the PMRS with recursive type definitions. The syntax is similar to Caml, except for the recursion scheme declarations. Call `./benchmarks/report.py` to see a list of benchmarks. The `./benchmarks/unrealizable` folder contains examples on which the tool either fails or runs forever because no solution exists.

### Basic Usage
`./atropos -h` should get you started.

The subfolders of the `benchmarks` folder contains input examples. An input problem is defined by three components: a reference function, a representation function and a recursion skeleton. By default, the tool looks for a reference function called `spec`, a representation function `repr` and a recursion skeleton `target`.

The file should start by defining the input types (which must be recursive types) of the reference
and the recursion skeleton. If the input type of the reference function is `tau` and the input type
of the recursion skeleton is `theta`, the representation function must be a function from `theta` to
`tau`.

The tool accepts inputs defined in two different formats: using a syntax specific for pattern-matching recursion scheme (PRMS) (files with `.pmrs` extension) or an OCaml file (`.ml` extension). The PMRS syntax is more feature complete, but we are actively working on the ML syntax.

## PMRS syntax

`atropos` uses a special syntax to specify recursion schemes (Pattern Matching Recursion Schemes).
The files ending in `.pmrs` are examples of this syntax. The benchmark `benchmarks/list/mpshom.pmrs` includes detailed comments on the input format.

The general syntax of a pattern matching recursion scheme is as follows:
```
pmrs (unknowns*) pmrs_name pmrs_args {invariant} =
    | f x_1 ... x_n -> t
    | g y_1 ... y_m p -> t
    ...
```
- `(unknowns*)` is an optional list of string identifying the unknowns of the pmrs.
- `pmrs_name` is the name of the pmrs.
- `pmrs_args` are the non-recursible arguments of the pmrs. They cannot be matched. See for example the 'benchmarks/list/search.pmrs' for an example. This is useful when the function needs more than one argument.
- `{invariant}` is an optional invariant of the function (for the reference function). `invariant` is a function of the form `fun (x1, x2, ...) -> expr(x1, x2, ...)` where `x1, x2, ..` is the output of the function, and `expr(x1, x2, ..)` a boolean expression. `invariant` indicates that `invariant(pmrs_name(x))` is true for any input `x` of the pmrs.
- `f x_1 ... x_n -> t` is a non-pattern matching rule with arguments `x_1 ... x_n` and contractum `t`.
- `g x_1 ... x_n p -> t` is a pattern matching rule with arguments `x_1 ... x_n` and pattern `p` and contractum `t`.
- The first rule defines the main function of the PMRS. The type of the non-terminal symbol of
the first rule should be the type of a function from input type of the PMRS to output type.

## ML syntax
The interface using the Caml syntax is still in development. See the `.ml` files in the benchmarks folder. Specifications are supported through syntax extensions: optional objects such as invariants are specified through attributes, and mandatory components (functions to be synthesized) are written using `[%synt name-of-the-function]`. The Caml syntax is limited to benchmarks where the main recursive function has only one input argument for now.

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
let rec hsum = function
  | CNil -> [%synt s0]
  | Single a -> [%synt f0] a
  | Concat (x, y) -> [%synt join] (hsum x) (hsum y)
;;
assert (hsum = clist_to_list @@ sum)
```
The functions to be synthesized are `[%synt s0]`, `[%synt f0]` and `[%synt join]`. The syntax extensions can be replaced by either simple functions or constants.
The final assertions indicates that the tool should synthesize the expressions for `s0`, `f0` and `join` such that the function `hsum` is functionally equivalent to using `clist_to_list` and then `sum` (i.e. for any input `x` we have `hsum x = sum (clist_to_list x)`).

Note that the representation function and the target recursion skeleton can be reused across a large set of examples. All the benchmarks in `benchmarks/list` use the same `repr` and `target`, modulo some changes in the base case when lists have a minimum size.
Running `./atropos benchmarks/list/sum.ml` returns the solution in less than a second:
```ocaml
let rec hsum = function
         CNil -> 0
        | Single(a) -> a
        | Concat(x, y) -> (hsum x) + (hsum y)
```
NOTE: The notation `s.0` to access the first element of the tuple `s` in the *output* format.


# Evaluation
The folder `extras/cav21` contains scripts and a readme to reproduce the results presented in the CAV21 paper.
Please move the scripts to the root folder before executing them.