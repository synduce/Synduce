#!/bin/sh
# This benchmarks runs a reduced set of experiments with a shorter timeout

# Create the directory where we'll write the results
mkdir -p short-results
export RESULTS_LOCAL_COPY=$HOME/Synduce/short-results/

# Write the timeout value in the python file
echo "timeout_value = 100" >> benchmarks/timeout_v.py

# Generate the data ... this can take a long time
./benchmarks/test.py -t 0
# Generate the graphs and tables
./benchmarks/report.py -y
