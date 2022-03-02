#!/bin/bash
source .venv/bin/activate
# This benchmarks runs a reduced set of experiments with a shorter timeout of 60s
# Create the directory where we'll write the results
mkdir -p short-results
export RESULTS_LOCAL_COPY=$PWD/short-results/

# Write the timeout value in the python file
echo "timeout_value = 60" >> benchmarks/timeout_v.py

# Generate the data ... this can take a long time
./benchmarks/test.py -t 0
# Generate the graphs and tables
./benchmarks/report.py -y
