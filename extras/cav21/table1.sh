#!/bin/sh
LOG_FILE="review_data/log.txt"
CSV_FILE="review_data/tmp.csv"
TABLE1="review_data/table1.txt"

mkdir -p "review_data"
echo "█ > Running all the benchmarks and storing results in $LOG_FILE"
./benchmarks/test.py 1 >> $LOG_FILE
echo "█ > Creating Table 1 in $TABLE1, and showing the file contents."
./benchmarks/report.py $LOG_FILE $CSV_FILE 1 $TABLE1
rm $CSV_FILE
echo "█ > TABLE 1:"
cat $TABLE1
echo "█ > Finished."
echo "█ > Results in $TABLE1"