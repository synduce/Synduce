# Synduce

*Synduce* is a program synthesizer: you specify a reference function, and a target recursion skeleton, and *Synduce* completes it for you.

## Building the tool

Synduce is mainly written in OCaml using the `dune` build system and `opam` to install package dependencies.
You will need a recent [OCaml](https://ocaml.org/releases/4.11.1.html) (>= 4.08.0) installation and the [OCaml Package Manager (opam)](https://opam.ocaml.org) to get started.
The Ocaml dependencies of this project can be installed via opam (```opam install . --deps-only```).
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
- `./benchmarks/` contains benchmarks and sample inputs. Call `./benchmarks/report.py -e` to see a list of interesting benchmarks. The `./benchmarks/unrealizable` folder contains unrealizable benchmarks. You will find benchmarks with predicates on input type in the `benchmarks/constraints` folder.

# Basic Usage

The tool accepts input files written in OCaml with specific construct for synthesis.

The subfolders of the `benchmarks` folder contains input examples. An input problem is defined by
three components: a reference function, a representation function and a recursion skeleton. By
default, the tool looks for a reference function called `spec`, a representation function `repr` and
a recursion skeleton `target`. Otherwise, if one want to synthesize a function `g` such that it is
equivalent to some function `f` composed with representation function `r`, then the user should
place an assertion `assert (g = f @@r)` in the input file.

The functions given to the tool should terminate on any input and only use structural recursion.

The file should start by defining the input types (which must be recursive types) of the reference
and the recursion skeleton. If the input type of the reference function is `tau` and the input type
of the recursion skeleton is `theta`, the representation function must be a function from `theta` to
`tau`.

Given an input file `example.ml`, simply run the tool as follows:

```./Synduce example.ml```

For example:

```./Synduce benchmarks/list/mps.ml```

Prints the solution to the problem.

## Caml Inputs
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
If the ensures predicate is necessary, but the user has not provided it, the tool will attempt to synthesize it.


### Requires attribute
A requires attributes can be provided to constrain the inputs that a function can accept. In other
words, it allows the user to specify a data invariant on the inputs. For an example, see
`benchmarks/constraints/bst/contains.ml`. The data invariant only has an effect if it is specified
for the target recursion skeleton (at the main function).

The target recursion skeleton in the example is expected to be only applied to trees that are binary
search trees. The user specifies their intent by adding the attribute `[@@requires is_bst]` in the
following:
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

The tool is not limited to unary functions. An example of a benchmark where the function also accepts additional argument can be found in `benchmarks/list/sum.ml`.
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
The tool will only consider functions that it can convert to pattern-matching recursion schemes
where the main symbol is a unary function on some recursive type. In other words, the parameters are
allowed as arguments in the wrapping function `f` but the recursive function `aux` must only accept
one argument.

**General remark on writing functions with multiple parameters for recursion**
If one wants to write a function that accepts two trees as arguments one can always write a function
that accept as input a type with a single constructor that combines two trees. In general, the user
of the tool can provide functions with multiple input parameters, with the specificity that in this
case the function application cannot be partial. In practice, this also potentially limits the
applicability of recursion elimination and partial bounding, as the toplevel function that can be
eliminated is now the function that accepts a specific datatype that is not the main datatype of
interest. An example can be found in `benchmarks/numbers/int_nat_twosum.ml`.

## Options
`./Synduce -h` lists all the options available in the tool. The most interesting options are the synthesis algorithm selection options:
- by default, Synduces uses the partial bounding algorithm as described in [CAV21](https://link.springer.com/chapter/10.1007/978-3-030-81685-8_39) and [PLDI22](https://pldi22.sigplan.org/) papers.
- `--segis` switches to using the Symbolic CEGIS algorithm. This can sometimes be faster for unrealizable benchmarks or benchmarks that have very simple solutions.
- `--cegis` switches to using the Concrete CEGIS algorithm. This is not recommended.
Other options include:
- `-d` for debugging output and `-v` for verbose output. Debugging output is especially useful to inspect the solver calls made by the tools. The verbose output is more useful for inspecting the internal representation of the objects in the algorithm.
- there are several options for turning some syntax-guided synthesis problem preprocessing optimizations. `-s` turns off an option that splits the systems of equations into independent systems, `-t` turns off the breaking down of tuples into individual constraints.

## PMRS syntax

`Synduce` also uses a special syntax to specify recursion schemes (Pattern Matching Recursion Schemes).
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

## Output of the tool
By default, the tool will output minimal information about the synthesis process, which is:
- First, it prints the definition of the problem it will attempt to solve. This allows the user to
  check that the tool is considering the problem expected (in particular, the functions have been
  provided in the right order and have the expected types).
- Some status messages, with the number of refinement rounds so far. For example, the example
  `benchmarks/list/mps.ml` is solved in a single round while `benchmarks/list/mss.ml` is solver in 3
  rounds.
- Any lemma that has been synthesized. Lemmas are either predicates about the output of the reference
  function, or about the input data when a data invariant has been provided as a requires
  attribute.
- The summary of the solver usage. For example:
  ```
  INFO : Total time spent in solvers:
        Z3-SMT     [  39 instances] 4.999s
        CVC-SyGuS  [  18 instances] 17.763s
        > TOTAL    [  57 instances]: 22.762
  ```
  This means the tool used 57 solver instances to verify and synthesize the solution. Note that the
  time reported here is larger than the time required to solve the benchmark since solvers are
  called in parallel.
- The final answer, which can be either a solution or an affirmation that the benchmark is
  unrealizable.
  - When a benchmark is realizable, the tool prints the solution, by first printing the solutions
    for the components of the recursion skeleton and then the recursion skeleton.
  - When a benchmark is unrealizable, the tool answers that it is unrealizable and if possible,
    provides a hint to the user on how the recursion skeleton could be changed to make the problem
    realizable (see for example `benchmarks/unrealizable/contains.ml`).


### Unrealizable Benchmarks

# Evaluation
## Benchmarks
You can run the set of benchmarks of Synduce by executing the following command:
```
./benchmarks/test.py -b all
```
This can take up to an hour depending on your computer. If you just want to perform a quick check that everything is running properly, you can use a smaller set of benchmarks:
```
./benchmarks/test.py -b simple
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
The folder `extras/cav21` contains scripts and a readme to reproduce the results presented in the CAV21 paper (use the `cav21` release).
Please move the scripts to the root folder before executing them.

## PLDI22
The file `README_EVALUATION.txt` contains the instructions to reproduce the results presented in the PLDI22 paper (in the `pldi22` release).
The artifact submitted for evaluation is available on [Zenodo]().
