#!/bin/sh
python3 benchmarks/test.py -1 > test/tmp.log
python3 test/readlog.py test/tmp.log
rm test/tmp.log