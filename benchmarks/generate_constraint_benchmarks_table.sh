#!/bin/bash
python3 ./benchmarks/test.py -t 4 -o /tmp/results.txt
python3 ./benchmarks/report_nonclassic.py -i /tmp/results.txt -o benchmarks/table_cstr.txt -t 1
rm -f /tmp/results.txt
