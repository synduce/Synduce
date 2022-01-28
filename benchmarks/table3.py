from commons import *
from definitions import *
import matplotlib.pyplot as plt
import numpy as np

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
    'segis': kw_segis_short
}

option_name = {
    'all': 'all',
    'init': 'init',
    'split': 'split',
    'syn': 'syntx',
    'off': 'off'
}

time_threshold = 2.0

non_algo_keyw = ['benchmarks', 'verif_ratios']

t3_caption = f"Comparison of the {kw_tool_main_algo} and {kw_segis} algorithms on the benhcmarks for the FMSD22 journal paper.\n"


def parse_line(info) -> dict:
    algos = {}
    algos['se2gis'] = {}
    algos['segis'] = {}
    i = 1
    while True:
        num_read, algo, option, block = parse_block(info, i)
        if block is None:
            return algos
        else:
            i += num_read
            algos[algo][option] = block


def cactus_plot_table3(cactus_file, series, exp_params):
    plot_fontsize = 11
    fig, ax = plt.subplots(figsize=(8, 3))

    for algo in series:
        if algo not in non_algo_keyw:
            for opt in series[algo]:
                if opt == 'init':
                    pass
                else:
                    k = sorted([x for x in series[algo]
                               [opt] if x < exp_params["timeout"]])
                    s_label = f"{algo_name[algo]:8s} -{option_name[opt]:8s} ({len(k)})"
                    linewidth = 1.0
                    ax.plot(k, label=s_label,
                            linestyle=algo_linestyle[algo],
                            color=option_color[opt], linewidth=linewidth)
    ax.set_xlabel("Number of benchmarks solved", fontsize=plot_fontsize)
    ax.set_ylabel("Time", fontsize=plot_fontsize)
    ax.legend(fontsize=plot_fontsize)
    fig.savefig(cactus_file, bbox_inches='tight')


def avg_significant(series, exp_params):
    indices = []
    for i, x in enumerate(series['benchmarks']):
        n = series['se2gis']['all'][i] + series['se2gis']['off'][i]
        if n > time_threshold and series['se2gis']['all'][i] < exp_params["timeout"]:
            indices += [(i, n)]
    return [x[0] for x in sorted(indices, key=(lambda x: x[1]))]


def normalize(serie, exp_params):
    local_timeout_value = exp_params["timeout"]
    return [x if x < local_timeout_value else 0 for x in serie]


def barchart_plot_table3(barchart_file, series, exp_params):
    fig, ax = plt.subplots(figsize=(8, 4))
    indices = avg_significant(series, exp_params)
    benchmarks = [series['benchmarks'][i][:8] for i in indices]
    x = np.arange(len(benchmarks))  # the label locations
    width = 0.1
    s1 = normalize([series['se2gis']['all'][i] for i in indices], exp_params)
    s2 = normalize([series['se2gis']['off'][i] for i in indices], exp_params)
    s3 = normalize([series['segis']['all'][i] for i in indices], exp_params)
    s4 = normalize([series['segis']['off'][i] for i in indices], exp_params)

    r1 = ax.bar(x - 1.5*width, s1, width,
                label=algo_name['se2gis'], color='blue')
    r2 = ax.bar(x - 0.5*width, s2, width,
                label=f"{ algo_name['se2gis'] } -off", color='cyan')
    r3 = ax.bar(x + 0.5*width, s3, width,
                label=algo_name['segis'], color='red')
    r4 = ax.bar(x + 1.5*width, s4, width,
                label=f"{algo_name['segis']} -off", color='orange')

    ax.set_ylabel('Time')
    ax.set_xticks(x)
    ax.set_xticklabels(benchmarks, rotation=60, fontsize=4)
    ax.legend()

    fig.savefig(barchart_file, bbox_inches='tight')


def verifchart_plot_table3(verifchart_file, series, exp_params):
    fig, ax = plt.subplots(figsize=(6, 4))
    points = sorted(series['verif_ratios'], key=lambda x: x[0])
    veriftimes = [x[1] for x in points]
    runtimes = [x[0] for x in points]
    x = [i+1 for i, _ in enumerate(points)]
    ax.plot(x, veriftimes, color='red', label='Verification time')
    ax.plot(x, runtimes, color='blue', label='Total synthesis time')
    ax.set_xlabel("Benchmarks", fontsize=plot_fontsize)
    ax.set_ylabel("Time", fontsize=plot_fontsize)
    ax.legend(fontsize=plot_fontsize)
    fig.savefig(verifchart_file, bbox_inches='tight')


def make_text_table(table, series, exp_params):
    with open("benchmarks/data/table3.txt", "w") as out:
        out.write(t3_caption)
        out.write("=== Summary ===\n")
        num_benchmarks = len(table)
        out.write(
            f"Number of benchmarks: {num_benchmarks}\n")
        out.write("Solved benchmark per option:\n")
        for a in series:
            if a not in non_algo_keyw:
                for opt in series[a]:
                    num_solved = len(
                        [x for x in series[a][opt] if x < exp_params["timeout"]])
                    if num_solved > 0:
                        out.write(
                            f"{algo_name[a] :10s} with {option_name[opt] :5s} solves {num_solved:4d}  ({100.0 * (1.0 * num_solved / (1.0 * num_benchmarks)) :3.1f} %).\n")
        out.write("=== Table ===\n")
        out.write("Coming soon")


def make_table_3(input_file, output_file):
    print("============== SUMMARY - T3 ================")
    print("Summary ablation study on set of benchmarks.")
    print("---------------------------------------------")

    input_name = input_file.split(".")[0]

    table = {}

    series = {}
    series['benchmarks'] = []
    series['verif_ratios'] = []
    series['se2gis'] = {}
    series['segis'] = {}
    series['se2gis']['all'] = []
    series['se2gis']['init'] = []
    series['se2gis']['split'] = []
    series['se2gis']['syn'] = []
    series['se2gis']['off'] = []
    series['segis']['all'] = []
    series['segis']['init'] = []
    series['segis']['split'] = []
    series['segis']['syn'] = []
    series['segis']['off'] = []

    fast_count = {}
    fast_count['overall'] = {}
    fast_count['se2gis'] = {}
    fast_count['segis'] = {}

    exp_params = {
        "timeout": timeout_value}

    with open(input_file, 'r') as csv:
        for line in csv.readlines():
            if line.startswith("SETUP:"):
                exp_setup = line[5:]
                continue
            if line.startswith("TIMEOUT:"):
                exp_params["timeout"] = int(line.split(":")[1].strip())
                continue
            info = [b.strip() for b in line.split(",")]
            if len(info) >= 15:
                benchmark = info[0].split(".")[0]
                # Line of length 22 for older results
                try:
                    algos = parse_line(info)
                except IndexError:
                    print(f"Error in benchmark {benchmark}")
                    algos = None
                table[benchmark] = algos
                fastest_time = exp_params["timeout"]
                fastest_algo = 'none'
                fastest_option = 'all'
                fastest_segis = 0
                fastest_segis_opt = 'all'
                fastest_segis_pb = 0
                fastest_segis_pb_opt = 'all'
                series['benchmarks'] += [benchmark]
                for algo in algos:
                    for optchoice in algos[algo]:
                        runtime = algos[algo][optchoice]['time'] if algos[algo][optchoice] else exp_params["timeout"]
                        veriftime = algos[algo][optchoice]['verif'] if algos[algo][optchoice] else exp_params["timeout"]

                        rt = floti(runtime, timeout=exp_params["timeout"])

                        series[algo][optchoice].append(rt)

                        if rt < fastest_time:
                            fastest_time = rt
                            fastest_algo = algo
                            fastest_option = optchoice

                        if algo == 'segis':
                            if rt < fastest_segis:
                                fastest_segis = rt
                                fastest_segis_opt = optchoice
                        else:
                            if rt < fastest_segis_pb:
                                fastest_segis_pb = rt
                                fastest_segis_pb_opt = optchoice
                            if optchoice == 'all':
                                series['verif_ratios'] += [
                                    (rt, floti(veriftime, timeout=exp_params["timeout"]))]

                fast_count['overall'][benchmark] = (
                    fastest_algo, fastest_option)
                fast_count['segis'][benchmark] = fastest_segis_opt
                fast_count['se2gis'][benchmark] = fastest_segis_pb_opt

                print("%54s, fastest with %s, %s" %
                      (benchmark, fastest_algo, fastest_option))

    make_text_table(table, series, exp_params)

    cactus_file = input_name + "_cactus.pdf"
    cactus_plot_table3(cactus_file, series, exp_params)

    barchart_file = input_name + "_barchart.pdf"
    barchart_plot_table3(barchart_file, series, exp_params)

    verifchart_file = input_name + "_verif.pdf"
    verifchart_plot_table3(verifchart_file, series, exp_params)

    num_benchmarks = len(table)
    print(
        f"Number of benchmarks: {num_benchmarks}")
    print("Solved benchmark per option:")
    for a in series:
        if a not in non_algo_keyw:
            for opt in series[a]:
                num_solved = len(
                    [x for x in series[a][opt] if x < exp_params["timeout"]])
                print(
                    f"{a :10s} with {opt :5s} solves {num_solved:4d}  ({100.0 * (1.0 * num_solved / (1.0 * num_benchmarks)) :3.1f} %).")

    print(f"Cactus plot:\n{cactus_file}")
    print(f"Bar chart plot:\n{barchart_file}")
    print(f"Verif times plot:\n{verifchart_file}")

    return {
        "cactus": cactus_file,
        "barchart": barchart_file,
        "verif": verifchart_file
    }
