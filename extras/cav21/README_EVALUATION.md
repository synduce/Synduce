# Kick-the-tires phase

The reviewers can make sure that the artifact runs properly by running a few scripts and testing the
tool on a simple benchmark.
This readme is focused on explaining which scripts can be run to reproduce the results in the paper.
For more information on how to build the tool from source, and how to use the tool, more information
can be found in `README.md`.
All the scripts for evaluation should be run from the root folder. All the scripts generate data in
the `review_data` folder, which you can delete at any point to clean up the experimental
data you generated.
The benchmarking scripts require `python3` to be installed (see `benchmarks/test.py` and
`benchmarks/report.py`).

## Preliminary results

The script `kick_the_tires.sh` runs a subset of the benchmarks presented in the paper, and produces
a text version of Table 1. Running `./kick_the_tires.sh` should take no more than a minute.
- The script first generates experimental data for 17 of the 43 benchmarks of the paper.
- Then it reports a summary of the experiments: you observe see that the 17 benchmarks have been run
  successfully (Synduce and the baseline have not timed out).
- A text version of Table 1 is printed. The missing data for the other benchmarks is indicated by a
  question mark "?" where relevant. You should see the 17 benchmarks for which this first phase
  collects data, among the 43 benchmarks. The synthesis times reported should be less than a second.

## Testing the tool

You can try running the tool on one of the benchmarks, for example the `sum` example in the list
parallelization category:
```
./Synduce benchmarks/list/sumhom.pmrs
```
The tool first prints out a summary of the problem it needs to solve: the reference function `spec`,
the target recursion skeleton `target` and the representation function `repr`. It then starts
solving the problem by synthesizing functions for the unknown components of the recursion skeleton,
in this case `odot`, `f_0` and `s_0`. A more detailed description of how to use the tool is given
later and in the `README.md` file of the tool.
The tool runs correctly if the last message printed shows the solution, and in how much time it was
found. The reviewer should expect a message of the form:
```
 INFO : Solution found in 0.0777s (96.1% verifying):
target⟨odot, f_0, s_0⟩(): int clist -> int =
{
  ‣ target t   ⟹  h t
    h  CNil  ⟹  0
    h  Single(a)  ⟹  a
    h  Concat(x, y)  ⟹  (h x) + (h y)

  }
```

# Evaluating Synduce

The main claim of the paper is that Synduce successfully synthesizes solutions for a wide range of
benchmarks, with a significant improvement over a baseline method that is itself implemented in the
tool. This claim is backed by the results presented in Table 1. The Appendix also contains
additional experimental results in Tables 2 and 3. The reviewer may want to reproduce the results
presented by running the tools on the benchmarks. Naturally, we expect the results of reproducing
the experiments in a virtual machine and on a different computer to be different from the ones
presented in the paper. However, Synduce should still perform better than the baseline in general.
We also expect that the tool should not time out on the set of benchmarks used in the paper.

We have set the timeout to 4min for the review (instead of 10min in the paper) to allow the
reviewer to run more benchmarks. This parameter can be changed in the `benchmaarks/test.py` file.

## Experimental Results

The following instructions explain how to run the experiments to reproduce the results presented in
Tables 1, 2 and 3 of the paper.

### Reproducing the results of Table 1

To reproduce the results of Table 1, we provide a script that will run the necessary experiments and
produce a textual version of Table 1 (the same as the table produced in the kick-the-tires phase).

Executing `./table1.sh` runs the tool and the baseline algorithm on each benchmark once. The log is
stored in `review_data/log.txt` and running the script more than once will append more data to the
log. If the script is run several times without deleting the log file, the table contains
information that is averaged over all the runs. Running the script should take 1 and 2 hours
(approx. 75min on the laptop used for evaluation in the paper).

The text version of Table 1 is stored in `review_data/table1.txt`.

### Reproducing the results of Table 2 and 3

Given the limited time given to the reviewers to reproduce the results in our paper, we selected a
subset of benchmarks on which the experiments can be run to reproduce partially the results of
Tables 2 and 3. Note that these tables are part of the Appendix.

The reviewer can run `./table2_partial.sh` (and `./table3_partial.sh`). The experimental data is
appended to `review_data/log.txt` and the script then generates the tables in
`review_data/table2.txt` (resp. `review_data/table3.txt`). Running either of those scripts will take
between 1 and 2 hours.

If the reviewer wants to spend more time generating their own experimental data, they can use the
`./table2_full.sh` and `./table3_full.sh` scripts. Running the full scripts will take more than a
day.

### Results from our experimental data

We included a sample of our experimental data in `benchmarks_log_cav21.txt`. The reviewer can
generate the tables using this data by calling the `benchmarks/report.py` script as follows:
```
./benchmarks/report.py benchmarks/benchmarks_log_cav21.txt review_data/tmp.csv TABLE_NO review_data/table.txt
```
The script will generate Table `TABLE_NO` (= 1, 2 or 3) in the `review_data/table.txt` file, storing
temporary results in `review_data/tmp.csv`. The reviewer should observe the same results as the
ones reported in the paper, with possibly a few differences as we collected more data for some
benchmarks. One difference is that the *mts* benchmark of the *Enforcing tail recursion* category
has been corrected, and Synduce synthesizes a solution even without the optimizations on (the
numbers in Table 3 will differ). This has been signaled during the rebuttal period.


## Using the tool on the benchmarks

The folder containing the tool contains a `README.md` file explaining how to use the tool. In this
file, we give an overview and some information specific to this review.

The tool prints a help message if called with the `h` option, e.g.:
```
./Synduce -h
Usage : Synduce [options] input_file
Options:
    -h --help                      Print this message.
    -v --verbose                   Print verbose.
    -i --info-off                  Print timing information only.
  Otimizations off/on:
    -s --no-splitting              Do not split systems into subsystems.
       --no-syndef                 Do not use syntactic definitions.
    -t --no-detupling              Turn off detupling.
    -c --simple-init               Initialize T naively.
       --segis                    Use the Abstract CEGIS algorithm. Turns bmc on.
       --cegis                    Use the Concrete CEGIS algorithm. Turns bmc on.
       --no-simplify               Don't simplify equations with partial evaluation.
       --no-gropt                  Don't optimize grammars.
  Bounded checking:
       --use-bmc                   Use segis bounded model checking (bmc mode).
    -b --bmc                       Maximum depth of terms for bounded model checking, in bmc mode.
    -v --verification              Number of expand calls for bounded model checking, in opt mode.
  Debugging:
       --parse-only                Just parse the input.
       --show-vars                 Print variables and their types at the end.
-> Try:
./Synduce benchmarks/list/mps.ml
```
The reviewers may be interested in running the tool with the `--segis` option to run the baseline
algorithm (symbolic CEGIS). The `--cegis` flag runs the concrete CEGIS algorithm that is
discussed in Table 2.

The flags that need to be used to turn the optimizations discussed for Table 3 are also explained in
the help message(`-s` and `-t` are used together to turn off detupling and splitting systems of
equations into subsystems).

Note that in the version submitted for review, grammar optimizations are off by default and the
`--no-gropt` flag has no effect. The `--no-simplify` option is also irrelevant.

### Location of individual benchmarks

The benchmarks are stored in the `benchmarks` folder, each subfolder containing a category of
benchmarks. Running `./benchmarks/report.py` will print a table that indicates where the file for
each benchmark given in Table 1 is stored (after a usage message for the script, which is used to
generate the tables).

### Running the tool on a benchmark

For example, the reviewer can run the tool on the `mips` example used in Section 1 (the input file
is `benchmarks/tree/mips.pmrs`). To do so, run `./Synduce benchmarks/tree/mips.ml`. Synthesis should
take less than a second. In the following, we detail the output produced by the tool.

The first message is a confirmation of the synthesis problem the tool is trying to solve: it must
find the implementation of `join1` and `s0` such that for any tree of integers `x`, the reference
function `spec` composed with the representation function `repr` is equivalent to the recursion
skeleton `target`:
```
 INFO :  Ψ (join1, s0) := ∀ x : int tree. (spec o repr)(x) = target(x)
```
The tool then prints the definition of the different components given in the input file:
```
 INFO : spec⟨⟩(): int tree -> (int * int){fun (x, y) -> (y ≥ 0) && (y ≥ x)} =
    {
      ‣ main t   ⟹  f (0, 0) t
        f s Nil  ⟹  s
        f s Node(a, l, r)  ⟹  (fun (sum1, m1) ->  f (sum1 + a, max (sum1 + a) m1) r) (f s l)

      }
 INFO : target⟨join1, s0⟩(): int tree -> (int * int) =
    {
      ‣ main_mips t1   ⟹  mips t1
        mips  Nil  ⟹  s0
        mips  Node(a, l, r)  ⟹  join1 a (mips l) (mips r)

      }
 INFO : repr(x) = x

 ```
 In this case, the representation function is identity. The two other functions are pattern matching
 recursion schemes. For more information on the input format(s), see the `README.md` file provided
 with the tool.

 The tool then starts the refinement loop. In this case, it succeeds in 2 steps and finds a
 solution.
 ```
 INFO : Refinement step 1.
 INFO : Checking solution...
 INFO : ... finished in 0.0080s
 INFO : Refinement step 2.
 INFO : Checking solution...
 INFO : ... finished in 0.0350s
 INFO : Solution found in 0.3067s (14.0% verifying):
target⟨join1, s0⟩(): int tree -> (int * int) =
{
  ‣ main_mips t1   ⟹  mips t1
    mips  Nil  ⟹  (0, 0)
    mips  Node(a, l, r)  ⟹
      (a + ((mips l).0 + (mips r).0), max (mips l).1 (a + ((mips l).0 + (mips r).1)))

  }

```
The solution is the input recursion skeleton `target` where the unknowns `join1` and `s0` have been
replaced by their definition. We use the notation `s.0` to access the first element of the tuple
`s` in the *output* format.
