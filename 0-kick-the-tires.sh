#!/bin/bash
# If everything is installed properly, the script should run without errors.
source .venv/bin/activate
# Run the testing script on the small set of benchmarks.
./benchmarks/test.py -b small
# It should terminate with (the running time may vary):
# ✅ All 24 benchmarks passed in 3.9 s.

# Running the scripts generating the experimental results...
# Create the directory where we'll write the results.
mkdir -p paper-results
export RESULTS_LOCAL_COPY=$PWD/paper-results/
# Set the timeout value that was used for the results reported in the paper.
echo "timeout_value = 400" >> benchmarks/timeout_v.py
# Generate the figures and tables from the csv containing the results.
./benchmarks/report.py -i benchmarks/data/exp/paper_results.csv -y
