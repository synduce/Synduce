#!/bin/sh
# This benchmarks runs the full set of experiments with a 400s timeout.

# Create the directory where we'll write the results
mkdir -p full-results
export RESULTS_LOCAL_COPY=$PWD/full-results/

# Write the timeout value in the python file
echo "timeout_value = 400" >> benchmarks/timeout_v.py

# Generate the data ... this can take a long time
./benchmarks/test.py -t 1
# Generate the graphs and tables
./benchmarks/report.py -y
