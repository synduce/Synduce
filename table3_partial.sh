#!/bin/sh
LOG_FILE="review_data/log.txt"
CSV_FILE="review_data/tmp.csv"
TABLE3="review_data/table3.txt"

mkdir -p "review_data"
echo "█ > Running reduced set of benchmarks for Table 3 and storing results in $LOG_FILE"
./benchmarks/test.py 3 1 >> $LOG_FILE
echo "█ > Creating Table 3 in $TABLE3, and showing the file contents."
./benchmarks/report.py $LOG_FILE $CSV_FILE 3 $TABLE3
rm $CSV_FILE
echo "█ > TABLE 3:"
cat $TABLE3
echo "█ > Finished."
echo "█ > Results in $TABLE3"