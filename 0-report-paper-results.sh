#!/bin/bash
# If everything is installed properly, the script should run without errors.
source .venv/bin/activate
# Running the scripts generating the experimental results...
# Create the directory where we'll write the results.
TARGET_DIR=paper-results
mkdir -p $TARGET_DIR
export RESULTS_LOCAL_COPY=$PWD/$TARGET_DIR
# Set the timeout value that was used for the results reported in the paper.
echo "timeout_value = 400" > benchmarks/timeout_v.py
# Generate the figures and tables from the csv containing the results.
./benchmarks/report.py -i benchmarks/data/exp/paper_results.csv -y
# Compile the pdf tables
pdflatex $TARGET_DIR/table1.tex > /dev/null
pdflatex $TARGET_DIR/table1.tex > /dev/null
mv table1.pdf $TARGET_DIR
rm table1*
pdflatex $TARGET_DIR/table2.tex > /dev/null
pdflatex $TARGET_DIR/table2.tex > /dev/null
mv table2.pdf $TARGET_DIR
rm table2*
echo "See tables and figures in $TARGET_DIR/"
