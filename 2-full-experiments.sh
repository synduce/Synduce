#!/bin/bash
source .venv/bin/activate
# This benchmarks runs the full set of experiments with a 400s timeout.
# Create the directory where we'll write the results
TARGET_DIR=full-results
mkdir -p $TARGET_DIR
export RESULTS_LOCAL_COPY=$PWD/$TARGET_DIR

# Write the timeout value in the python file
echo "timeout_value = 400" >> benchmarks/timeout_v.py

# Generate the data ... this can take a long time
./benchmarks/test.py -t 1
# Generate the graphs and tables
./benchmarks/report.py -y
# Compile the pdf tables
pdflatex $TARGET_DIR/table1.tex
mv table1.pdf $TARGET_DIR
pdflatex $TARGET_DIR/table2.tex
mv table2.pdf $TARGET_DIR
