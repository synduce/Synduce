from commons import *
from definitions import *
import matplotlib.pyplot as plt


show_algos = ['se2gis', 'segis', 'cegis']

option_color = {
    'all': 'blue',
    'init': 'cyan',
    'split': 'green',
    'syn': 'orange',
    'off': 'red'

}

algo_linestyle = {
    'se2gis': 'solid',
    'segis': 'dotted',
}

algo_name = {
    'se2gis': kw_tool_main_algo,
    'segis': kw_segis_short,
    'cegis': kw_cegis_short,
}

option_name = {
    'all': 'all',
    'init': 'init',
    'split': 'split',
    'syn': 'syntx',
    'off': 'off'
}


def t2_caption(timeout_value):
    return f"Comparison of the {kw_tool_main_algo},{kw_segis} and {kw_cegis} algorithms on the benchmarks for the FMSD22 journal paper.\n"


def cactus_plot_table2(cactus_file, se2gis_series, segis_series, cegis_series):
    fig, ax = plt.subplots(figsize=(6, 4))
    s1 = [x for x in sorted(
        se2gis_series) if x < timeout_value]
    s2 = [x for x in sorted(
        segis_series) if x < timeout_value]
    s3 = [x for x in sorted(
        cegis_series) if x < timeout_value]
    ax.plot(s1, color="purple", label=f"{kw_tool_main_algo} ({len(s1)})",
            linestyle="solid", linewidth=1.5)
    ax.plot(s2, color="darkorange", label=f"{kw_segis_short} ({len(s2)})",
            linestyle="solid", linewidth=1.5)
    ax.plot(s3, color="green", label=f"{kw_cegis_short} ({len(s3)})",
            linestyle="solid", linewidth=1.5)
    ax.set_xlabel("Number of benchmarks solved", fontsize=plot_fontsize)
    ax.set_ylabel("Time", fontsize=plot_fontsize)
    ax.legend(fontsize=plot_fontsize)
    fig.savefig(cactus_file, bbox_inches='tight')


def save_scatter_plot_table2(scatter_file, se2gis_series, segis_series, timeouts=False):

    if timeouts:
        fig, ax = plt.subplots(figsize=(4, 4))
        ax.plot(segis_series, se2gis_series, "x", color="firebrick")
        ax.set_xscale("log")
        ax.set_yscale("log")
        all_points = segis_series + se2gis_series
        ax_min = 0.5*min(all_points)
        ax_max = 5*max(all_points)
        ax.set(xlim=(ax_min, ax_max),
               ylim=(ax_min, ax_max))
        ax.set_xlabel(f"{kw_segis_short} synthesis time (log)",
                      fontsize=plot_fontsize)
        ax.set_ylabel(f"{kw_tool_main_algo} synthesis time (log)",
                      fontsize=plot_fontsize)

        ax.plot([0, 1], [0, 1], color="black",
                linestyle="dotted", transform=ax.transAxes)
        ax.set_aspect('equal')
        fig.savefig(scatter_file, bbox_inches='tight')
    else:
        no_timeouts = list(filter(lambda d: d[0] < timeout_value and d[1] < timeout_value, zip(
            segis_series, se2gis_series)))
        baseline_slower = len([x for x in no_timeouts if x[0] > x[1]])
        print(
            f"Scatter plot 2: Baseline slower on {baseline_slower} benchmarks")
        segis_series_no_timeout = [d[0] for d in no_timeouts]
        se2gis_series_no_timeout = [d[1] for d in no_timeouts]

        fig, ax = plt.subplots(figsize=(4, 4))
        ax.plot(segis_series_no_timeout,
                se2gis_series_no_timeout, "x", color="firebrick")

        all_points_notimeout = segis_series_no_timeout + se2gis_series_no_timeout
        ax_min = 0.5*min(all_points_notimeout)
        ax_max = 1.5*max(all_points_notimeout)
        ax.set(xlim=(ax_min, ax_max),
               ylim=(ax_min, ax_max))
        ax.set_xscale("log")
        ax.set_yscale("log")
        ax.set_xlabel(f"{kw_segis_short} synthesis time (log)",
                      fontsize=plot_fontsize)
        ax.set_ylabel(f"{kw_tool_main_algo} synthesis time(log)",
                      fontsize=plot_fontsize)
        diag_max = int(min(max(segis_series_no_timeout),
                           max(se2gis_series_no_timeout)))
        ax.plot([0, timeout_value], [0, timeout_value], color="grey",
                linestyle="dotted")

        ax.set_aspect('equal')
        fig.savefig(scatter_file, bbox_inches='tight')


def make_text_table(table2, exp_params, series, timeouts):
    def table_line(out, bench_name, algos):
        timings = ""
        for algo in algos:
            time = floti(algos[algo]['time'])
            if time == exp_params["timeout"]:
                time_str = "-"
            else:
                time_str = f"{time:3.2f}"
            timings += f"{time_str:10s}"
        out.write(f"{bench_name:35s}{timings}\n")

    def sep(out):
        out.write(f"{dash:-<90s}\n")

    with open("benchmarks/data/table_baselines_comparison.txt", "w") as out:
        out.write(t2_caption(exp_params["timeout"]))
        out.write("=== Summary ===\n")
        out.write(
            f"Number of benchmarks: {len(series['segis'])}\n")
        out.write(
            f"{kw_segis_short} solves {len([x for x in series['segis'] if x < timeout_value])}\n")
        out.write(
            f"{kw_cegis_short} solves {len([x for x in series['cegis'] if x < timeout_value])}\n")
        out.write(
            f"{kw_tool_main_algo} solves {len([x for x in series['se2gis'] if x < timeout_value])}\n")
        out.write(
            f"{timeouts['cegis']} timeouts for {kw_cegis_short}, {timeouts['segis']} timeouts for {kw_segis_short}, {timeouts['se2gis']} timeouts for {kw_tool_main_algo}.\n")
        out.write(
            f"{kw_tool_main_algo} is faster on {timeouts['speedups']} benchmarks.\n")
        num_benchmarks = len(table2)
        out.write("=== Table ===\n")
        # === Write the algorihms header
        headers = ""
        empty_header = ""
        for algo in show_algos:
            headers += f"{algo_name[algo]:10s}"
        out.write(f"{empty_header:35s}{headers}\n")
        sep(out)
        # === Write the benchmark running times for each option
        for bench in table2:
            table_line(out, bench, table2[bench])

# "TABLE 2" with three different algorithms


def parse_line_v1(info):
    se2gis_result = parse_alg(info, 0)
    segis_result = parse_alg(info, 7)
    cegis_result = parse_alg(info, 14)
    return (se2gis_result, segis_result, cegis_result)


def parse_line_v2(info):
    se2gis_result = parse_alg_v2(info, 0)
    segis_result = parse_alg_v2(info, 8)
    cegis_result = parse_alg_v2(info, 16)
    return (se2gis_result, segis_result, cegis_result)


def make_table_2(input_file, output_file):
    print("============== SUMMARY - T2 ================")
    print("Summary of relative improvement of Synduce over baselines SEGIS and CEGIS.")
    print("improvement = SEGIS synt. time / Synduce synt. time")
    print("∞ means SEGIS timed out, but Synduce did not")
    print("-∞ means Synduce timed out, but SEGIS did not,")
    print("! means both timed out.")
    print("---------------------------------------------")
    # Table 2 line format
    # benchmark, se2gis_algo_name, time, delta, rounds, N, B, verif,
    #     0            1            2      3       4    5  6    7
    # segis_algo_name, time, delta, rounds, N, B ,verif
    #      8            9     10     11     12 13 14
    # cegis_algo_name, time, delta, rounds, N, B ,verif
    #      15            16     17     18     19 20 21

    se2gis_timeouts = 0
    segis_timeouts = 0
    cegis_timeouts = 0
    speedups = 0
    print("%54s, %8s,  %8s,  %8s: %5s" %
          ("Benchmark", kw_tool_main_algo, kw_segis_short, kw_cegis_short, "Speedup"))

    exp_setup = experimental_setup_2
    input_name = input_file.split(".")[0]
    cactus_file = input_name + "_cactus.pdf"
    scatter_file = input_name + "_scatter.pdf"
    scatter_no_timeouts_file = input_name + "_no_timeouts_scatter.pdf"
    tex_table = input_name + "_table.tex"

    exp_params = {}
    exp_params['timeout'] = timeout_value

    unrealizable_benchmarks = 0

    table2 = {}

    se2gis_series = []
    segis_series = []
    se2gis_unrealizable_series = []
    segis_unrealizable_series = []
    cegis_series = []
    cegis_unrealizable_series = []

    with open(input_file, 'r') as csv:
        for line in csv.readlines():
            if line.startswith("SETUP:"):
                exp_params['setup'] = line[5:]
                continue
            if line.startswith("TIMEOUT:"):
                exp_params['timeout'] = int(line[8:])
                continue
            info = line.split(",")
            if len(info) >= 15:
                benchmark = info[0].split(".")[0]
                info = repair(info)
                table2[benchmark] = {}

                # Line of length 22 for older results
                if len(info) == 22:
                    se2gis_result, segis_result, cegis_result = parse_line_v1(
                        info)
                    table2[benchmark]['se2gis'] = se2gis_result
                    table2[benchmark]['segis'] = segis_result
                    table2[benchmark]['cegis'] = cegis_result
                elif len(info) == 25:
                    se2gis_result, segis_result, cegis_result = parse_line_v2(
                        info)
                    table2[benchmark]['se2gis'] = se2gis_result
                    table2[benchmark]['segis'] = segis_result
                    table2[benchmark]['cegis'] = cegis_result

                if len(info) > 15:
                    a = se2gis_result['time']
                    b = segis_result['time']
                    c = cegis_result['time']

                    if floti(a) < floti(b) and floti(a) < floti(c):
                        speedups += 1

                    if floti(a) == timeout_value:
                        se2gis_timeouts += 1

                    if floti(b) == timeout_value:
                        segis_timeouts += 1

                    if floti(c) == timeout_value:
                        cegis_timeouts += 1

                    if unrealizable(info[4]):
                        unrealizable_benchmarks += 1
                        se2gis_unrealizable_series.append(floti(a))
                        segis_unrealizable_series.append(floti(b))
                        cegis_unrealizable_series.append(floti(c))
                    else:
                        se2gis_series.append(floti(a))
                        segis_series.append(floti(b))
                        cegis_series.append(floti(c))

                    print("%54s, %8s, %8s, %8s: %5s" %
                          (benchmark, a, b, c, speedup(a, b)))

    # Plot a scatter plot with a diagonal line
    save_scatter_plot_table2(scatter_file, se2gis_series,
                             segis_series, timeouts=True)

    # Plot a scatter plot with a diagonal line, omitting timeouts
    save_scatter_plot_table2(scatter_no_timeouts_file,
                             se2gis_series, segis_series)

    segis_series = segis_series + segis_unrealizable_series
    se2gis_series = se2gis_series + se2gis_unrealizable_series
    cegis_series = cegis_series + cegis_unrealizable_series
    # Plot one quantile plot
    cactus_plot_table2(cactus_file, se2gis_series,
                       segis_series, cegis_series)

    # Output a tex table for  realizable benchmarks
    # make_tex_table(exp_setup, table2, tex_table)
    # Output a tex table for unrealizable benchmarks
    # make_tex_unrealizables_table(exp_setup, table2, tex_table2)
    series = {'cegis': cegis_series,
              'segis': segis_series, 'se2gis': se2gis_series}
    timeouts = {'cegis': cegis_timeouts,
                'segis': segis_timeouts, 'se2gis': se2gis_timeouts, 'speedups': speedups}
    make_text_table(table2, exp_params, series, timeouts)

    print(
        f"Number of benchmarks: {len(segis_series)}")
    print(
        f"{kw_segis_short} solves {len([x for x in segis_series if x < timeout_value])}")
    print(
        f"{kw_cegis_short} solves {len([x for x in cegis_series if x < timeout_value])}")
    print(
        f"{kw_tool_main_algo} solves {len([x for x in se2gis_series if x < timeout_value])}")
    print(f"{cegis_timeouts} timeouts for {kw_cegis_short}, {segis_timeouts} timeouts for {kw_segis_short}, {se2gis_timeouts} timeouts for {kw_tool_main_algo}.")
    print(f"{kw_tool_main_algo} is faster on {speedups} benchmarks.")
    print(f"Tex table    : {tex_table}  ")
    print(f"Cactus plot: {cactus_file}")
    print(f"Scatter plot : {scatter_file}")
    print(f"Scatter plot (omitting timeouts) : {scatter_no_timeouts_file}")
    return {
        "cactus": cactus_file,
        "scatter": scatter_file,
        "scatter_no_timeouts": scatter_no_timeouts_file,
        "table": tex_table,
    }
