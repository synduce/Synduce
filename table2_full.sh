#!/bin/sh
LOG_FILE="review_data/log.txt"
CSV_FILE="review_data/tmp.csv"
TABLE2="review_data/table2.txt"

mkdir -p "review_data"
echo "█ > Running full set of benchmarks for Table 2 and storing results in $LOG_FILE"
./benchmarks/test.py 2 >> $LOG_FILE
echo "█ > Creating Table 2 in $TABLE2, and showing the file contents."
./benchmarks/report.py $LOG_FILE $CSV_FILE 2 $TABLE2
rm $CSV_FILE
echo "█ > TABLE 2:"
cat $TABLE2
echo "█ > Finished."
echo "█ > Results in $TABLE2"