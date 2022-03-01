# Getting Started

The software artifact is packaged as an Open Virtual Appliance (.ova) file
[https://en.wikipedia.org/wiki/Open_Virtualization_Format] with Xubuntu 21.04.
The credentials are:
username: pldi22-reviewer
password: pldi22ae
The synthesis tool can be found in `/home/pldi22-reviewer/Synduce`. Note that the
tool is named Sebis in the paper for anonimity reasons, and the artifact builds
upon and extends the tool Synduce. In this artifact, we use the name Synduce for
the tool.

To get started, open a terminal and change directory to the root folder of the tool
`/home/pldi22-reviewer/Synduce`.
The tool has already been compiled, but the README.md in the tool directory contains
general instructions on how to build the tool and run it.
We have prepared a script for the reviewers to use to check that the installation is
working as expected.
Running the following command:

`./0-kick-the-tires.sh`

Runs the tool on a small set of 24 benchmarks. All benchmarks should pass. All
benchmarks have a solution, except the last one.
The last line of the output should look like:

✅ All 24 benchmarks passed in 3.9 s.

Naturally, the running time may vary, but it should be within 10s.
The reviewer cam also run the tool on individual benchmarks, for example:

./Synduce benchmarks/constraints/bst/count_lt.ml

Should print a solution in less than a second.

# Step-by-step Instructions

This section contains step-by-step instructions to reproduce the results presented
in the paper. More precisely, we provide instruction to reproduce:
- the sequence of calls to the tool described in Section 2 that leads the
programmer to find a solution for the problem of optimizing the count-between
function on binary search trees.
- the experimental results that are presented in Section 8 in Figures 3 and 4. These
results are repeated in Appendix C.2 and C.3 in table form (Table 1 and 2).

## Reproducing the example of Section 2



## Reproducing the experimental results of Section 8
