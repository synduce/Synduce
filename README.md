# Synduce
[![CI](https://github.com/synduce/Synduce/actions/workflows/build.yml/badge.svg?branch=dev)](https://github.com/synduce/Synduce/actions/workflows/build.yml)

*Synduce* is a program synthesizer: you specify a reference function, and a target recursion skeleton, and *Synduce* completes it for you.

![Synduce output](https://github.com/victornicolet/Synduce/blob/dev/extras/output.png?raw=true)

## Building the tool

Synduce is mainly written in OCaml using the `dune` build system and `opam` to install package dependencies.
You will need a recent [OCaml](https://ocaml.org/releases/4.11.1.html) (>= 4.08.0) installation and the [OCaml Package Manager (opam)](https://opam.ocaml.org) to get started.
The Ocaml dependencies of this project can be installed via opam. The `syguslib-utils` library must be installed from the submodule. To install the dependencies, run:
```[shell]
git submodule init
git submodule update
opam pin syguslib-utils # pin and install the syguslib-utils dependency
opam install . --deps-only # install the rest of the dependencies
```
Once all the dependencies are installed, call ```make``` from the root of the project. The Makefile simply calls `dune build` and creates a shortcut to the binary executable.

Synduce also requires:
-  [**Z3**](https://github.com/Z3Prover/z3) (> 4.8.10) for solving some SMT problems during bounded checking of recursive functions.
-  [**CVC4**](https://cvc4.github.io) (>= 1.8) for some induction proofs, SMT problems and SyGuS problem solving.
-  (Optional) [**CVC5**](https://cvc5.github.io): Synduce can use CVC5 instead of CVC4, but has been tested mostly with CVC4.

*Synduce* detects where your binaries are using `which` by calling `which z3` and `which cvc4`, so please make sure that the solver executable names are `z3` and `cvc4` (and not `cvc4-1.8` for example).
If these solvers are not present in the environment, then Synduce will produce a runtime error.

Note that *Synduce* does not rely on any specific API and instead communicates with the solver through the SMTLib v2.6 and SyGus Lib v2 language standards.

## Ubuntu
The script `setup.sh` installs all the dependencies and packages from a fresh Ubuntu installation.


# Documentation
The documentation for the library in the `dev` branch is [here](https://victornicolet.github.io/Synduce/Synduce/index.html).
- `./bin/` contains all the sources for the executable,
- `./src/` contains all the sources for the libraries. The `lang` folder is where you will find most of the language definitions and `algo` the algorithmic components.
- `./benchmarks/` contains benchmarks and sample inputs. `parse_examples/parsing.pmrs` is an example of the input syntax for the PMRS with recursive type definitions. The syntax is similar to Caml, except for the recursion scheme declarations. Call `./benchmarks/report.py` to see a list of benchmarks. The `./benchmarks/unrealizable` folder contains examples on which the tool either fails or runs forever because no solution exists. You will find benchmarks with predicates on input type in the `benchmarks/constraints` folder.

# Evaluation
## Benchmarks
You can run the set of benchmarks of Synduce by executing the following command:
```
./benchmarks/test.py -b all
```
This can take up to an hour depending on your computer. If you just want to perform a quick check that everything is running properly, you can use a smaller set of benchmarks:
```
./benchmarks/test.py -b small
```
You can also benchmark a single file. For example, if you want to run `benchmarks/list/mss.ml` 10 times, using cvc5 as backend solver instead of cvc4, run:
```
./benchmarks/test.py --single list/mss.ml+"--cvc5" -n 10
```
The argument to `--single` should be the path to the file in the benchmarks folders, and if necessary, add options to pass to the tool by adding +"options".
For a single benchmark, the script prints a line of the form:
```
    (1 / 1) ✅ mss                                      ×10 runs, average:  9.158s ± 208ms       R: ++✓
```
Indicating the average runtime of running the tool on the benchmark with a delta. The string after the R: represents the refinement rounds necessary to produce the solution. A "+" indicates that the approximation needed to be strenghtened, a "." indicates weakening and "^" indicates lifting. For a successful benchmark, the string should end by a check mark, indicating the solution was successfully checked in the last round.
## CAV21
The `cav21` release is the version of the tool described in the CAV'21 paper `Counterexample-Guided Partial Bounding for Recursive Function Synthesis`.


## PLDI22
The `pldi22` release corresponds to the tool as described in the PLDI'22 paper `Recursion Synthesis with Unrealizability Witnesses'.

# Basic Usage
`./Synduce -h` should get you started.

The subfolders of the `benchmarks` folder contains input examples. An input problem is defined by three components: a reference function, a representation function and a recursion skeleton. By default, the tool looks for a reference function called `spec`, a representation function `repr` and a recursion skeleton `target`.

The file should start by defining the input types (which must be recursive types) of the reference
and the recursion skeleton. If the input type of the reference function is `tau` and the input type
of the recursion skeleton is `theta`, the representation function must be a function from `theta` to
`tau`.

The tool accepts inputs defined in two different formats: using a syntax specific for pattern-matching recursion scheme (PRMS) (files with `.pmrs` extension) or an OCaml file (`.ml` extension). The PMRS syntax is more feature complete, but we are actively working on the ML syntax.


## ML syntax
See the `.ml` files in the benchmarks folder. Specifications are supported through syntax extensions: optional objects such as invariants are specified through attributes, and mandatory components (functions to be synthesized) are written using `[%synt name-of-the-function]`.

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
Running `./Synduce benchmarks/list/sum.ml` returns the solution in less than a second:
```ocaml
let s0  = 0
let f0 x = x
let join x40 x42 = x40 + x42
let rec hsum = function
    | CNil -> s0
    | Single(a) -> f0 a
    | Concat(x, y) -> join (hsum x) (hsum y)
```

### Ensures attribute

A function can be given attributes that provide more information about their input and output domains.
For example, in `benchmarks/list/mts.ml`, the first output element of the `mts` function is always positive
and larger than the second output element:
```ocaml
let rec mts = function
  | Nil -> (0, 0)
  | Cons (hd, tl) ->
      let amts, asum = mts tl in
      let new_sum = asum + hd in
      (max amts new_sum, new_sum)
  (* the mts is always positive *)
  [@@ensures fun (x, y) -> x >= 0 && x >= y]
```
This is specified by the `[@@ensures predicate]` where `predicate` is a function from the output domain of
the function to which the attribute is attached, to booleans.


### Requires attribute
A requires attributes can be provided to constrain the inputs that a function can accept. For an example, see `benchmarks/constraints/bst/contains.ml`.
The target recursion skeleton here is expected to be only applied to trees that are binary search trees. The user specifies their intent by adding the attribute `[@@requires is_bst]` in the following:
```ocaml
let target y t =
  let rec g = function
    | Leaf a -> [%synt xi_0] y a
    | Node (a, l, r) -> if y < a then [%synt xi_1] (g l) else [%synt xi_2] y a (g l) (g r)
  in
  g t
  [@@requires is_bst]
```
When a `requires` is specified on the function to be synthesized (the target function), then
the tool effectively solves the problem '∀ x : Θ. T_inv(x) ⇒ target(x) = spec(repr(x))` where
`T_inv` is the function given to the requires clause.

### Functions with additional parameters

The tool is not limited to unary functions. An example of a benchmark where the function also accepts and additional argument can be found in `benchmarks/list/sum.ml`.
The syntax to write functions with additional non-recursible parameters is of the form:
```ocaml
let f a b c t =
    (* The last argument t must be the one used in the recursion.
    The other arguments a,b,c are "parameters" of the pattern-matching recursion scheme. *)
    (* Define mutually recursive functions. Parameters are in scope. *)
    let rec aux = function _ -> (* .. *)
    and aux2 = (* function defintion .. *)
    in
    (* Call the main recursive function *)
    aux t
```

**General remark on writing functions with multiple parameters for recursion**
If one wants to write a function that accepts two trees as arguments one can always write a function that accept as input a type with a single constructor that combines two trees. In general, the user of the tool can provide functions with multiple input parameters, with the specificity that in this case the function application cannot be partial.
In practice, this also potentially limits the applicability of recursion elimination and partial bounding, as the toplevel function that can be eliminated is now the function that accepts a specific datatype that is not the main datatype of interest.
An example can be found in `benchmarks/numbers/int_nat_twosum.ml`.

## PMRS syntax

`Synduce` uses a special syntax to specify recursion schemes (Pattern Matching Recursion Schemes).
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
