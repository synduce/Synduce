#!/usr/bin/env python3
from datetime import datetime
import matplotlib.pyplot as plt
from commons import *
from definitions import *


def make_tex_table(exp_setup, data, output_file_name):
    tex = open(output_file_name, 'w+')
    tex.write("%s ====================================\n" % '%')
    tex.write(
        "%s This table has been automatically produced by the tool on %s.\n" %
        ('%', str(datetime.now())))
    tex.write("%s ====================================\n" % '%')
    # open table
    tex.write("\t{\small\n")
    tex.write("\t\t\\begin{longtable}[h]{|c|c|c|c|c||c|c||c|c|}\n")
    tex.write("\t\t\t\\hline\n")
    tex.write(
        "\t\t\t \multirow{2}{*}{Class} &\
                \multirow{2}{*}{Benchmark} & \
                \multirow{2}{*}{I?} & \
                \multicolumn{2}{c||}{\setwogis} & \
                \multicolumn{2}{c|}{SEGIS+UC} & \
                \multicolumn{2}{c|}{SEGIS}\\\\ \n")
    tex.write("\t\t\t\\cline{4-9}\n")
    tex.write(
        "\t\t\t &   & & time & steps & time & \\#'r' & time & \\#'r' \\\\ \n")
    speedups = []
    for benchmark_class, benchmarks in show_benchmarks.items():
        tex.write("\t\t\t\\hline\n")
        for benchmark, benchmark_info in benchmarks.items():
            benchmark_file = benchmark_class + "/" + benchmark
            experimental_data = data.get(benchmark_file)
            if experimental_data is None:
                tex.write(empty_exp_data(benchmark_info))
            else:
                if len(experimental_data) >= 3:
                    tex.write(with_exp_data(benchmark_info,
                                            experimental_data[0], experimental_data[1], experimental_data[2]))
                else:
                    tex.write(with_exp_data(benchmark_info,
                                            experimental_data[0], experimental_data[1], None))

    tex.write("\t\t\t\\hline\n")
    tex.write("\t\caption{%s}\label{table:experiments}\n" % caption(exp_setup))
    tex.write("\t\t\end{longtable}\n")
    tex.write("\t}\n")
    tex.close()


def make_tex_unrealizables_table(exp_setup, data, output_file_name):
    tex = open(output_file_name, 'w+')
    tex.write("%s ====================================\n" % '%')
    tex.write(
        "%s This table has been automatically produced by the tool on %s.\n" %
        ('%', str(datetime.now())))
    tex.write("%s ====================================\n" % '%')
    # open table
    tex.write("\t{\small\n")
    tex.write("\t\t\\begin{longtable}[h]{|c|c|c|c||c|c|}\n")
    tex.write("\t\t\t\\hline\n")
    tex.write(
        "\t\t\t\multirow{2}{*}{Benchmark} & \
                \multirow{2}{*}{I?} & \
                \multicolumn{2}{c||}{\setwogis} & \
                \multicolumn{2}{c|}{SEGIS+UC}\\\\ \n")
    tex.write("\t\t\t\\cline{3-6}\n")
    tex.write(
        "\t\t\t & & time & steps & time & \\#'r' \\\\ \n")
    speedups = []
    for benchmark_class, benchmarks in unrealizable_show_set.items():
        tex.write("\t\t\t\\hline\n")
        for benchmark, benchmark_info in benchmarks.items():
            benchmark_file = benchmark_class + "/" + benchmark
            experimental_data = data.get(benchmark_file)
            if experimental_data is None:
                tex.write(empty_exp_data(benchmark_info, False))
            else:
                tex.write(with_exp_data(benchmark_info,
                          experimental_data[0], experimental_data[1], None, False))

    tex.write("\t\t\t\\hline\n")
    tex.write(
        "\t\caption{%s}\label{table:unrealizable-experiments}\n" % caption_unrealizable(exp_setup))
    tex.write("\t\t\end{longtable}\n")
    tex.write("\t}\n")
    tex.close()


def quantile_plot(quantile_file, segis_series, se2gis_series, segis0_series):
    fig, ax = plt.subplots(figsize=(6, 4))
    s1 = [x for x in sorted(
        se2gis_series) if x < timeout_value]
    s2 = [x for x in sorted(
        segis_series) if x < timeout_value]
    s3 = [x for x in sorted(
        segis0_series) if x < timeout_value]
    ax.plot(s1, color="purple", label=kw_tool_main_algo_v2,
            linestyle="solid", linewidth=1.5)
    ax.plot(s2, color="darkorange", label=kw_segis_uc_short,
            linestyle="solid", linewidth=1.5)
    ax.plot(s3, color="green", label=kw_segis_short,
            linestyle="solid", linewidth=1.5)
    ax.set_xlabel("Number of benchmarks solved", fontsize=plot_fontsize)
    ax.set_ylabel("Time", fontsize=plot_fontsize)
    ax.legend(fontsize=plot_fontsize)
    fig.savefig(quantile_file, bbox_inches='tight')


def quantile_plot2(quantile_file, segis_series, se2gis_series):
    fig, ax = plt.subplots(figsize=(6, 2))
    s1 = [x for x in sorted(
        se2gis_series) if x < timeout_value]
    s2 = [x for x in sorted(
        segis_series) if x < timeout_value]
    ax.plot(s1, color="purple", label=kw_tool_main_algo_v2, marker='o')
    # linestyle="solid", linewidth=1.5)
    ax.plot(s2, color="darkorange", label=kw_segis_uc_short, marker='o')
    # linestyle="solid", linewidth=1.5)
    ax.set_xlabel("Number of unrealizable benchmarks solved",
                  fontsize=plot_fontsize)
    ax.set_ylabel("Time", fontsize=plot_fontsize)
    ax.legend(fontsize=plot_fontsize)
    fig.savefig(quantile_file, bbox_inches='tight')


def save_scatter_plot(scatter_file, segis_series, se2gis_series,
                      segis_unrealizable_series, se2gis_unrealizable_series, timeouts=False):

    if timeouts:
        fig, ax = plt.subplots(figsize=(4, 4))
        ax.plot(segis_series, se2gis_series, "x", color="firebrick")
        ax.plot(segis_unrealizable_series,
                se2gis_unrealizable_series, "x", color="blue")
        ax.set_xscale("log")
        ax.set_yscale("log")
        all_points = segis_series + se2gis_series + \
            se2gis_unrealizable_series + segis_unrealizable_series
        ax_min = 0.5*min(all_points)
        ax_max = 5*max(all_points)
        ax.set(xlim=(ax_min, ax_max),
               ylim=(ax_min, ax_max))
        ax.set_xlabel(f"Synthesis time using {kw_segis_uc_short} (log)",
                      fontsize=plot_fontsize)
        ax.set_ylabel(f"Synthesis time using {kw_tool_main_algo_v2} (log)",
                      fontsize=plot_fontsize)

        ax.plot([0, 1], [0, 1], color="black",
                linestyle="dotted", transform=ax.transAxes)
        ax.set_aspect('equal')
        fig.savefig(scatter_file, bbox_inches='tight')
    else:
        no_timeouts = list(filter(lambda d: d[0] != timeout_value and d[1] != timeout_value, zip(
            segis_series, se2gis_series)))
        no_timeouts_unr = list(filter(lambda d: d[0] != timeout_value and d[1] != timeout_value, zip(
            segis_unrealizable_series, se2gis_unrealizable_series)))
        segis_series_no_timeout = [d[0] for d in no_timeouts]
        se2gis_series_no_timeout = [d[1] for d in no_timeouts]
        segis_unr_series_no_timeout = [d[0] for d in no_timeouts_unr]
        se2gis_unr_series_no_timeout = [d[1] for d in no_timeouts_unr]
        fig, ax = plt.subplots(figsize=(4, 4))
        ax.plot(segis_series_no_timeout,
                se2gis_series_no_timeout, "x", color="firebrick")
        ax.plot(segis_unr_series_no_timeout,
                se2gis_unr_series_no_timeout, "x", color="blue")
        all_points_notimeout = segis_series_no_timeout + se2gis_series_no_timeout + \
            se2gis_unr_series_no_timeout + segis_unr_series_no_timeout
        ax_min = 0.5*min(all_points_notimeout)
        ax_max = 1.5*max(all_points_notimeout)
        ax.set(xlim=(ax_min, ax_max),
               ylim=(ax_min, ax_max))
        ax.set_xscale("log")
        ax.set_yscale("log")
        ax.set_xlabel(f"Synthesis time using {kw_segis_uc_short} (log)",
                      fontsize=plot_fontsize)
        ax.set_ylabel(f"Synthesis time using {kw_tool_main_algo_v2} (log)",
                      fontsize=plot_fontsize)
        diag_max = int(min(max(segis_series_no_timeout),
                           max(se2gis_series_no_timeout)))
        ax.plot([0, timeout_value], [0, timeout_value], color="grey",
                linestyle="dotted")

        ax.set_aspect('equal')
        fig.savefig(scatter_file, bbox_inches='tight')


def make_table_5(input_file, output_file):
    print("============== SUMMARY - T5 ================")
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

    exp_setup = experimental_setup_2
    input_name = input_file.split(".")[0]
    quantile_file = input_name + "_quantile.pdf"
    quantile_unrealizable_file = input_name + "_unrealizable_quantile.pdf"
    scatter_file = input_name + "_scatter.pdf"
    scatter_no_timeouts_file = input_name + "_no_timeouts_scatter.pdf"
    tex_table = input_name + "_table.tex"
    tex_table2 = input_name + "_table_unrealizable.tex"

    unrealizable_benchmarks = 0

    table = {}

    se2gis_series = []
    segis_series = []
    se2gis_unrealizable_series = []
    segis_unrealizable_series = []
    segis0_series = []
    segis0_unrealizable_series = []

    with open(input_file, 'r') as csv:
        for line in csv.readlines():
            if line.startswith("SETUP:"):
                exp_setup = line[5:]
                continue
            info = line.split(",")
            if len(info) >= 15:
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

                # We may have data for SEGIS base
                segis0_data = False
                c = "N/A"
                if len(info) == 22:
                    segis0_data = True
                    segis0_result = {
                        "time": info[16],
                        "delta": info[17],
                        "rounds": info[18],
                        "N": info[19],
                        "B": info[20],
                        "verif": info[21],
                    }
                    c = segis0_result['time']

                if segis0_data:
                    table[benchmark] = (
                        se2gis_result, segis_result, segis0_result)
                else:
                    table[benchmark] = (se2gis_result, segis_result)

                a = se2gis_result['time']
                b = segis_result['time']

                if floti(a) < floti(b) and floti(a) < floti(c):
                    speedups += 1

                if floti(a) == timeout_value:
                    se2gis_timeouts += 1

                if floti(b) == timeout_value:
                    segis_timeouts += 1

                if unrealizable(info[4]):
                    unrealizable_benchmarks += 1
                    se2gis_unrealizable_series.append(floti(a))
                    segis_unrealizable_series.append(floti(b))
                    if segis0_data:
                        segis0_unrealizable_series.append(floti(c))
                else:
                    se2gis_series.append(floti(a))
                    segis_series.append(floti(b))
                    if segis0_data:
                        segis0_series.append(floti(c))

                warning_b = c == "N/A" and floti(
                    b) < timeout_value and not unrealizable(info[4])

                if warning_b:
                    print("%54s, %7s, %7s, %7s: %5s !!!" %
                          (benchmark, a, b, c, speedup(a, b)))
                else:
                    print("%54s, %7s, %7s, %7s: %5s" %
                          (benchmark, a, b, c, speedup(a, b)))

    # Plot a scatter plot with a diagonal line
    save_scatter_plot(scatter_file, segis_series, se2gis_series,
                      segis_unrealizable_series, se2gis_unrealizable_series, timeouts=True)

    # Plot a scatter plot with a diagonal line, omitting timeouts
    save_scatter_plot(scatter_no_timeouts_file, segis_series, se2gis_series,
                      segis_unrealizable_series, se2gis_unrealizable_series)

    segis_series = segis_series + segis_unrealizable_series
    se2gis_series = se2gis_series + se2gis_unrealizable_series
    segis0_series = segis0_series + segis0_unrealizable_series
    # Plot two quantile plots
    quantile_plot(quantile_file, segis_series, se2gis_series, segis0_series)
    quantile_plot2(quantile_unrealizable_file,
                   segis_unrealizable_series, se2gis_unrealizable_series)
    # Output a tex table for  realizable benchmarks
    make_tex_table(exp_setup, table, tex_table)
    # Output a tex table for unrealizable benchmarks
    make_tex_unrealizables_table(exp_setup, table, tex_table2)

    print(
        f"Number of benchmarks: {len(segis_series)} ({unrealizable_benchmarks} unrealizable cases)")
    print(
        f"SEGIS solves {len([x for x in segis0_series if x < timeout_value])}")
    print(
        f"SEGIS+UC solves {len([x for x in segis_series if x < timeout_value])}")
    print(
        f"SE2GIS solves {len([x for x in se2gis_series if x < timeout_value])}")
    print(f"{segis_timeouts} timeouts for SEGIS, {se2gis_timeouts} timeouts for SE2GIS.")
    print(f"SE2GIS is faster on {speedups} benchmarks.")
    print(f"Tex table    : { tex_table}  ")
    print(f"Quantile plot: {quantile_file}")
    print(
        f"Quantile unrealizable benchmarks plot: {quantile_unrealizable_file}")
    print(f"Scatter plot : {scatter_file}")
    print(f"Scatter plot (omitting timeouts) : {scatter_no_timeouts_file}")
    return {
        "quantile": quantile_file,
        "quantile_unrealizable": quantile_unrealizable_file,
        "scatter": scatter_file,
        "scatter_no_timeouts": scatter_no_timeouts_file,
        "table": tex_table,
        "table2": tex_table2
    }
