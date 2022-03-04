#!/bin/bash
# If everything is installed properly, the script should run without errors.
source .venv/bin/activate
# Run the testing script on the small set of benchmarks.
./benchmarks/report.py --explain
