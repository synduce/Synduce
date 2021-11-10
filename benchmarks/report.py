#!/usr/bin/env python3
import sys
import os
from datetime import datetime
import argparse
from definitions import *
import matplotlib.pyplot as plt


caption = "Experimental Results.  Benchmarks are grouped by categories introduced in Section \\ref{sec:cstudies}. All times are in seconds. The best time is highlighted in bold font.  A '-' indicates timeout ($>$ 10 min). The ``B'' column indicates if using bounded checking was used to classify a counterexample or validate a lemma. The refinements is a sequence of 'r' (refinement) and 'c' (coarsening) or 'l' (lifting). Experiments are run on %s" % experimental_setup


def empty_exp_data(info):
    #        category   benchmark NB time  ref. time  ref.
    return f"{info[0]}&{info[1]}& ? & ?   & ?  & ?  & ?  \\\\ %chktex 26\n"


def timefix(x):
    if x in timeout_names:
        return "-"
    else:
        return x


def roundfix(s):
    if s == "None":
        s = "-"
    else:
        s = s[:-1]
    s = s.replace("^", "l")
    s = s.replace("+", "r")
    s = s.replace(".", "c")
    return s


def roundcount(s):
    if s == "None":
        s = "-"
    else:
        s = s[:-1]
    return str(len(s))


def with_exp_data(info, data, data2):
    #    se2gis_result = {
    #                 "time": info[2],
    #                 "delta": info[3],
    #                 "rounds": info[4],
    #                 "N": info[5],
    #                 "B": info[6],
    #                 "verif": info[7],
    #             }
    if data['B'] == "✓":
        req_bounding = "y"
    else:
        req_bounding = "n"

    rounds = roundfix(data['rounds'])
    rounds2 = roundcount(data2['rounds'])

    return f"{info[0]} & {info[1]} & { req_bounding } & {timefix(data['time'])} & {rounds} & {timefix(data2['time'])}  & {rounds2}  \\\\ \n"


def make_tex_table(data, output_file_name):
    tex = open(output_file_name, 'w+')
    tex.write("%s ====================================\n" % '%')
    tex.write(
        "%s This table has been automatically produced by the tool on %s.\n" %
        ('%', str(datetime.now())))
    tex.write("%s ====================================\n" % '%')
    # open table
    tex.write("\t{\n")
    tex.write("\t\t\\begin{longtable}[h]{|c|c|c|c|c||c|c|}\n")
    tex.write("\t\t\t\\hline\n")
    tex.write(
        "\t\t\t \multirow{2}{*}{Class} &\
                \multirow{2}{*}{Benchmark} & \
                \multirow{2}{*}{B?} & \
                \multicolumn{2}{c||}{\\tool} & \
                \multicolumn{2}{c|}{Baseline}\\\\ \n")
    tex.write("\t\t\t\\cline{4-7}\n")
    tex.write(
        "\t\t\t &   & & time & refinements & time & \\#'r' \\\\ \n")
    speedups = []
    for benchmark_class, benchmarks in show_benchmarks:
        tex.write("\t\t\t\\hline\n")
        for benchmark, benchmark_info in benchmarks:
            benchmark_file = benchmark_class + "/" + benchmark
            experimental_data = data.get(benchmark_file)
            if experimental_data is None:
                tex.write(empty_exp_data(benchmark_info))
            else:
                tex.write(with_exp_data(benchmark_info,
                          experimental_data[0], experimental_data[1]))

    tex.write("\t\t\t\\hline\n")
    tex.write("\t\caption{%s}\label{table:experiments}\n" % caption)
    tex.write("\t\t\end{longtable}\n")
    tex.write("\t}\n")
    tex.close()


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

    segis_timeouts = 0
    se2gis_timeouts = 0
    speedups = 0
    print("%54s, %7s,  %5s : %5s" %
          ("Benchmark", "SE2GIS", " SEGIS", "Speedup"))

    se2gis_series = []
    segis_series = []

    input_name = input_file.split(".")[0]
    quantile_file = input_name + "_quantile.pdf"
    scatter_file = input_name + "_scatter.pdf"
    scatter_no_timeouts_file = input_name + "_no_timeouts_scatter.pdf"
    tex_table = input_name + "_table.tex"

    table = {}

    with open(input_file, 'r') as csv:
        for line in csv.readlines():
            info = line.split(",")
            if len(info) == 15:
                benchmark = info[0].split(".")[0]
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

                table[benchmark] = (se2gis_result, segis_result)

                a = se2gis_result['time']
                b = segis_result['time']

                if floti(a) < floti(b):
                    speedups += 1

                if floti(a) == timeout_value:
                    se2gis_timeouts += 1

                if floti(b) == timeout_value:
                    segis_timeouts += 1

                se2gis_series.append(floti(a))
                segis_series.append(floti(b))

                print("%54s, %7s, %7s : %5s" %
                      (benchmark, a, b, speedup(a, b)))

    # Plot a scatter plot with a diagonal line
    fig, ax = plt.subplots(figsize=(6, 6))
    ax.plot(segis_series, se2gis_series, "x", color="firebrick")
    ax.set_xscale("log")
    ax.set_yscale("log")
    ax_min = 0.5*min(segis_series + se2gis_series)
    ax_max = 5*max(segis_series + se2gis_series)
    ax.set(xlim=(ax_min, ax_max),
           ylim=(ax_min, ax_max))
    ax.set(xlabel="Synthesis time using SEGIS (log)",
           ylabel="Synthesis time using SE²GIS (log)")
    ax.plot([0, 1], [0, 1], color="black",
            linestyle="dotted", transform=ax.transAxes)
    ax.set_aspect('equal')
    fig.savefig(scatter_file)

    # Plot a scatter plot with a diagonal line, omitting timeouts
    no_timeouts = list(filter(lambda d : d[0] != timeout_value and d[1] != timeout_value, zip(segis_series, se2gis_series)))
    segis_series_no_timeout = [d[0] for d in no_timeouts]
    se2gis_series_no_timeout = [d[1] for d in no_timeouts]
    fig, ax = plt.subplots(figsize=(6, 6))
    ax.plot(segis_series_no_timeout, se2gis_series_no_timeout, "x", color="firebrick")
    ax_min = 0.5*min(segis_series_no_timeout + se2gis_series_no_timeout)
    ax_max = 5*max(segis_series_no_timeout + se2gis_series_no_timeout)
    ax.set(xlim=(ax_min,ax_max),
           ylim=(ax_min, ax_max))
    ax.set_xscale("log")
    ax.set_yscale("log")
    ax.set(xlabel="Synthesis time using SEGIS (log)",
           ylabel="Synthesis time using SE²GIS (log)")
    diag_max = int(min(max(segis_series_no_timeout), max(se2gis_series_no_timeout)))
    ax.plot([0, 1], [0, 1], color="black",
            linestyle="dotted", transform=ax.transAxes)
    ax.plot(list(range(diag_max)), list(range(diag_max)), color="black",
            linestyle="dotted")
    ax.set_aspect('equal')
    fig.savefig(scatter_no_timeouts_file)

    # Plot a quantile plot
    fig, ax = plt.subplots(figsize=(9, 6))
    s1 = [x for x in sorted(se2gis_series) if x < timeout_value]
    s2 = [x for x in sorted(segis_series) if x < timeout_value]
    ax.plot(s1, color="firebrick", label="SE²GIS", linestyle="solid")
    ax.plot(s2, color="mediumblue", label="SEGIS", linestyle="dotted")
    ax.set(xlabel="Number of benchmarks solved.", ylabel="Time.")
    ax.legend(fontsize=14)
    fig.savefig(quantile_file)

    # Output a tex table
    make_tex_table(table, tex_table)

    print(f"Number of benchmarks: {len(segis_series)}")
    print(f"{segis_timeouts} timeouts for SEGIS, {se2gis_timeouts} timeouts for SE2GIS.")
    print(f"SE2GIS is faster on {speedups} benchmarks.")
    print(f"Tex table    : { tex_table}  ")
    print(f"Quantile plot: {quantile_file}")
    print(f"Scatter plot : {scatter_file}")
    print(f"Scatter plot (omitting timeouts) : {scatter_no_timeouts_file}")


def select_last_known_experimental_data(table_no):
    candidates = []
    for root, dirs, files in os.walk("benchmarks/data/exp"):
        for file in files:
            comps = file.split("_")
            if len(comps) > 1 and file.endswith(".csv"):
                try:
                    date_generated = datetime.strptime(
                        comps[1].strip(), timestamp_definition)
                    file_table_no = comps[2].split(".")[0][-1]
                    if table_no == int(file_table_no):
                        candidates.append((date_generated, file))
                except ValueError as e:
                    pass

    candidates = sorted(candidates, key=lambda x: x[0], reverse=True)
    if len(candidates) > 0:
        return f"benchmarks/data/exp/{candidates[0][1]}"
    else:
        return None


if __name__ == "__main__":
    aparser = argparse.ArgumentParser()
    aparser.add_argument(
        "-i", "--input", help="The input file produced by running test.py", type=str, default=None)
    aparser.add_argument(
        "-o", "--output", help="The output text file for the table.", type=str, default="benchmarks/table.txt")
    aparser.add_argument(
        "-t", "--table", help="Table number that the script must generate.", type=int, choices=[0, 1, 2, 3, 4, 5], default=0)
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

    if (table_no is None) or table_no < 0 or table_no > 5:
        print("Please provide a table number between 0 and 5.")
        exit()

    if not input_file:
        input_file = select_last_known_experimental_data(table_no)
        print(f"Input file selected: {input_file}")

    if table_no == 5 or table_no == 0 and input_file:
        make_table_5(input_file, output_file)
