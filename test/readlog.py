#!/usr/bin/env python3
import sys
import datetime

timeout_time = 600.0


def run_check(input_file):
    benchmark_data = {}
    with open(input_file) as raw:
        lines = raw.readlines()
        num_lines = len(lines)
        benchfile = "none"
        benchmark_terminated = False
        for i in range(num_lines):
            s = lines[i]
            if s.startswith("B:"):
                if not benchmark_terminated and not benchfile == "none":
                    print("❌ %s" % benchfile)
                benchmark_terminated = False
                inst = s.strip().strip("B:").split(",")
                benchfile = inst[0]
            elif s.strip() == "success":
                benchmark_terminated = True
                print("✅ %s" % benchfile)


def benchsort(x):
    return x[0] + x[1] + x[2]


if __name__ == "__main__":
    input_file = sys.argv[1]
    run_check(input_file)
