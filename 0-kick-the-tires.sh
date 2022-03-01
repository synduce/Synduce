#!/bin/sh
# If everything is installed properly, the script should run without errors.
# Run the testing script on the small set of benchmarks.
./benchmarks/test.py -b small
# It should terminate with (the running time may vary):
# ✅ All 24 benchmarks passed in 3.9 s.
