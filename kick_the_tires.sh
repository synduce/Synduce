#!/bin/sh
LOG_FILE="review_data/log1.txt"
CSV_FILE="review_data/table1_phase1.csv"
TABLE1="review_data/table1_phase1.txt"

mkdir -p "review_data"
echo "Running small set of benchmarks and store results in $LOG_FILE"
./benchmarks/test.py 1 0 >> $LOG_FILE
echo "Creating Table 1 in $TABLE1"
./benchmarks/report.py $LOG_FILE $CSV_FILE 1 $TABLE1
echo "TABLE 1:"
cat $TABLE1