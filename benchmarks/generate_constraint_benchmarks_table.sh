#!/bin/sh
python3 ./benchmarks/test.py -t 5 -o /tmp/results.txt
python3 ./benchmarks/report_constraint.py -i /tmp/results.txt -o benchmarks/table_cstr.txt -t 1
rm -f /tmp/results.txt
