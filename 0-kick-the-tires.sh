#!/bin/sh
# If everything is installed properly, the script should run without errors.
# Try to execute Synduce on a simple benchmarks.
./Synduce benchmarks/constraints/bst/count_lt.ml -i

# Run the testing script on a small set of benchmarks.
./benchmarks/test.py -b small
