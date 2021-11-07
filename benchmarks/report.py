#!/usr/bin/env python3
import sys
import datetime
import argparse
from definitions import *


def make_table_5(input_file, output_file):
    print("============== SUMMARY ================")
    print("Summary of relative improvement of Synduce over baseline.")
    print("improvement = baseline synt. time / Synduce synt. time")
    print("∞ means baseline timed out, but Synduce did not")
    print("-∞ means Synduce timed out, but baseline did not,")
    print("! means both timed out.")
    print("---------------------------------------------")
    # Table 5 line format
    # benchmark, se2gis_algo_name, time, delta, rounds, N, B, verif, segis_algo_name, time, delta, rounds, N, B ,verif
    #     0            1            2      3       4    5  6    7         8            9     10     11     12 13 14
    table = []
    segis_timeouts = 0
    se2gis_timeouts = 0
    speedups = 0
    print("%40s, %5s,  %5s : %5s" %
          ("Benchmark", "SE2GIS", " SEGIS", "Speedup"))
    with open(input_file, 'r') as csv:
        for line in csv.readlines():
            info = line.split(",")
            if len(info) == 15:
                benchmark = info[0]
                se2gis_result = {
                    "time": info[2],
                    "delta": info[3],
                    "rounds": info[4],
                    "N": info[5],
                    "B": info[6],
                    "verif": info[7],
                }
                segis_result = {
                    "time": info[9],
                    "delta": info[10],
                    "rounds": info[11],
                    "N": info[12],
                    "B": info[13],
                    "verif": info[14],
                }

                table.append([benchmark, se2gis_result, segis_result])

                a = se2gis_result['time']
                b = segis_result['time']

                if floti(a) < floti(b):
                    speedups += 1

                if floti(a) == timeout_value:
                    se2gis_timeouts += 1

                if floti(b) == timeout_value:
                    segis_timeouts += 1

                print("%40s, %5s, %7s : %5s" %
                      (benchmark, a, b, speedup(a, b)))

    print(f"{segis_timeouts} timeouts for SEGIS, {se2gis_timeouts} timeouts for SEGIS.")
    print(f"SE2GIS is faster on {speedups} benchmarks.")


if __name__ == "__main__":
    aparser = argparse.ArgumentParser()
    aparser.add_argument(
        "-i", "--input", help="The input file produced by running test.py", type=str, default="benchmarks/constraints_bench.txt")
    aparser.add_argument(
        "-o", "--output", help="The output text file for the table.", type=str, default="benchmarks/table.txt")
    aparser.add_argument(
        "-t", "--table", help="Table number that the script must generate.", type=int, choices=[1, 2, 3, 4, 5], default=1)
    aparser.add_argument(
        "-c", "--csv", help="The output csv file for results.", type=str, default="benchmarks/constraints_results.csv")
    aparser.add_argument(
        "-e", "--explain", help="Explain where the benchmarks are stored.", action="store_true")

    args = aparser.parse_args()

    if args.explain:
        explain()
        exit()

    input_file = args.input
    output_file = args.csv
    table_no = args.table
    tex_out = args.output

    if not table_no or table_no < 0 or table_no > 5:
        print("Please provide a table number between 1 and 5.")
        exit()

    if table_no == 5:
        make_table_5(input_file, output_file)
