from commons import *
from definitions import *
import matplotlib.pyplot as plt
import numpy as np

option_color = {
    'all': 'blue',
    'init': 'cyan',
    'split': 'green',
    'syntx': 'orange',
    'off': 'red'

}

algo_linestyle = {
    'segis+PB': 'solid',
    'segis': 'dotted',
}

time_threshold = 2.0


def parse_line(info) -> dict:
    algos = {}
    algos['segis+PB'] = {}
    algos['segis'] = {}
    algos['segis+PB']['all'] = parse_alg_v2(info, 0)
    algos['segis+PB']['init'] = parse_alg_v2(info, 8)
    algos['segis+PB']['split'] = parse_alg_v2(info, 16)
    algos['segis+PB']['syntx'] = parse_alg_v2(info, 24)
    algos['segis+PB']['off'] = parse_alg_v2(info, 32)
    algos['segis']['all'] = parse_alg_v2(info, 40)
    algos['segis']['init'] = parse_alg_v2(info, 48)
    algos['segis']['split'] = parse_alg_v2(info, 56)
    algos['segis']['syntx'] = parse_alg_v2(info, 64)
    algos['segis']['off'] = parse_alg_v2(info, 72)

    return algos


def cactus_plot_table3(cactus_file, series):
    fig, ax = plt.subplots(figsize=(8, 6))

    for algo in series:
        if algo != 'benchmarks':
            for opt in series[algo]:
                if algo == 'segis' and opt == 'init':
                    pass
                else:
                    k = sorted([x for x in series[algo]
                               [opt] if x < timeout_value])
                    s_label = f"{algo:8s} -{opt}"
                    linewidth = 1.0
                    ax.plot(k, label=s_label,
                            linestyle=algo_linestyle[algo],
                            color=option_color[opt], linewidth=linewidth)
    ax.set_xlabel("Number of benchmarks solved", fontsize=plot_fontsize)
    ax.set_ylabel("Time", fontsize=plot_fontsize)
    ax.legend(fontsize=plot_fontsize)
    fig.savefig(cactus_file, bbox_inches='tight')


def avg_significant(series):
    indices = []
    for i, x in enumerate(series['benchmarks']):
        n = series['segis+PB']['all'][i] + series['segis+PB']['off'][i]

        if n > time_threshold and series['segis+PB']['all'][i] < timeout_value:
            indices += [(i, n)]
    return [x[0] for x in sorted(indices, key=(lambda x: x[1]))]


def normalize(serie):
    return [x if x < timeout_value else 0 for x in serie]


def barchart_plot_table3(barchart_file, series):

    fig, ax = plt.subplots(figsize=(8, 4))
    indices = avg_significant(series)
    benchmarks = [series['benchmarks'][i][:8] for i in indices]
    x = np.arange(len(benchmarks))  # the label locations
    width = 0.1
    s1 = normalize([series['segis+PB']['all'][i] for i in indices])
    s2 = normalize([series['segis+PB']['off'][i] for i in indices])
    s3 = normalize([series['segis']['all'][i] for i in indices])
    s4 = normalize([series['segis']['off'][i] for i in indices])

    r1 = ax.bar(x - 1.5*width, s1, width, label='segis+PB', color='blue')
    r2 = ax.bar(x - 0.5*width, s2, width,
                label='segis+PB -off', color='cyan')
    r3 = ax.bar(x + 0.5*width, s3, width, label='segis', color='red')
    r4 = ax.bar(x + 1.5*width, s4, width,
                label='segis -off', color='orange')

    ax.set_ylabel('Time')
    ax.set_xticks(x)
    ax.set_xticklabels(benchmarks, rotation=60, fontsize=4)
    ax.legend()

    fig.savefig(barchart_file, bbox_inches='tight')


def make_table_3(input_file, output_file):
    print("============== SUMMARY - T3 ================")
    print("Summary ablation study on set of benchmarks.")
    print("---------------------------------------------")

    input_name = input_file.split(".")[0]

    table = {}

    series = {}
    series['benchmarks'] = []
    series['segis+PB'] = {}
    series['segis'] = {}
    series['segis+PB']['all'] = []
    series['segis+PB']['init'] = []
    series['segis+PB']['split'] = []
    series['segis+PB']['syntx'] = []
    series['segis+PB']['off'] = []
    series['segis']['all'] = []
    series['segis']['init'] = []
    series['segis']['split'] = []
    series['segis']['syntx'] = []
    series['segis']['off'] = []

    fast_count = {}
    fast_count['overall'] = {}
    fast_count['segis+PB'] = {}
    fast_count['segis'] = {}

    with open(input_file, 'r') as csv:
        for line in csv.readlines():
            if line.startswith("SETUP:"):
                exp_setup = line[5:]
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

                fastest_time = timeout_value
                fastest_algo = 'none'
                fastest_option = 'all'
                fastest_segis = 0
                fastest_segis_opt = 'all'
                fastest_segis_pb = 0
                fastest_segis_pb_opt = 'all'
                series['benchmarks'] += [benchmark]
                for algo in algos:
                    for optchoice in algos[algo]:
                        runtime = algos[algo][optchoice]['time'] if algos[algo][optchoice] else timeout_value
                        series[algo][optchoice].append(floti(runtime))
                        if floti(runtime) < fastest_time:
                            fastest_time = floti(runtime)
                            fastest_algo = algo
                            fastest_option = optchoice
                        if algo == 'segis':
                            if floti(runtime) < fastest_segis:
                                fastest_segis = floti(runtime)
                                fastest_segis_opt = optchoice
                        else:
                            if floti(runtime) < fastest_segis_pb:
                                fastest_segis_pb = floti(runtime)
                                fastest_segis_pb_opt = optchoice

                fast_count['overall'][benchmark] = (
                    fastest_algo, fastest_option)
                fast_count['segis'][benchmark] = fastest_segis_opt
                fast_count['segis+PB'][benchmark] = fastest_segis_pb_opt

                print("%54s, fastest with %s, %s" %
                      (benchmark, fastest_algo, fastest_option))

    cactus_file = input_name + "_cactus.pdf"
    cactus_plot_table3(cactus_file, series)

    barchart_file = input_name + "_barchart.pdf"
    barchart_plot_table3(barchart_file, series)

    num_benchmarks = len(table)
    print(
        f"Number of benchmarks: {num_benchmarks}")
    print("Solved benchmark per option:")
    for a in series:
        if a != 'benchmarks':
            for opt in series[a]:
                num_solved = len(
                    [x for x in series[a][opt] if x < timeout_value])
                print(
                    f"{a :10s} with {opt :5s} solves {num_solved}  ({100.0 * (1.0 * num_solved / (1.0 * num_benchmarks)) :3.1f} %).")

    print(f"Cactus plot:\n{cactus_file}")
    print(f"Bar chart plot:\n{barchart_file}")

    return {
        "cactus": cactus_file,
        "barchart": barchart_file,
    }
