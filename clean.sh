#!/bin/sh
echo "timeout_value = 400" > benchmarks/timeout_v.py
rm -rf paper-results
rm -rf full-results
rm -rf short-results
rm -f benchmarks/data/exp/bench_*
rm -f benchmarks/data/exp/paper_results_*
rm -f table1*
rm -f table2*