from commons import *
from definitions import *


def parse_line(info) -> dict:
    algos = {}
    algos['segis+pb'] = {}
    algos['segis'] = {}
    algos['segis+pb']['all'] = parse_alg_v2(info, 0)
    algos['segis+pb']['ini'] = parse_alg_v2(info, 8)
    algos['segis+pb']['st'] = parse_alg_v2(info, 16)
    algos['segis+pb']['d'] = parse_alg_v2(info, 24)
    algos['segis+pb']['off'] = parse_alg_v2(info, 32)
    algos['segis']['all'] = parse_alg_v2(info, 40)
    algos['segis']['ini'] = parse_alg_v2(info, 48)
    algos['segis']['st'] = parse_alg_v2(info, 56)
    algos['segis']['d'] = parse_alg_v2(info, 64)
    algos['segis']['off'] = parse_alg_v2(info, 72)

    return algos


def make_table_3(input_file, output_file):
    print("============== SUMMARY - T3 ================")
    print("Summary ablation study on set of benchmarks.")
    print("---------------------------------------------")

    input_name = input_file.split(".")[0]

    table = {}

    series = {}
    series['segis+pb'] = {}
    series['segis'] = {}
    series['segis+pb']['all'] = []
    series['segis+pb']['ini'] = []
    series['segis+pb']['st'] = []
    series['segis+pb']['d'] = []
    series['segis+pb']['off'] = []
    series['segis']['all'] = []
    series['segis']['ini'] = []
    series['segis']['st'] = []
    series['segis']['d'] = []
    series['segis']['off'] = []

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

                for algo in algos:
                    for optchoice in algos[algo]:
                        runtime = algos[algo][optchoice]['time'] if algos[algo][optchoice] else timeout_value
                        if floti(runtime) < timeout_value:
                            series[algo][optchoice].append(benchmark)
                        if floti(runtime) < fastest_time:
                            fastest_time = floti(runtime)
                            fastest_algo = algo
                            fastest_option = optchoice

                print("%54s, fastest with %s, %s" %
                      (benchmark, fastest_algo, fastest_option))

    # Plot a scatter plot with a diagonal line
    # save_scatter_plot_table2(scatter_file, se2gis_series, segis_series, timeouts = True)

    # Plot a scatter plot with a diagonal line, omitting timeouts
   # save_scatter_plot_table2(scatter_no_timeouts_file, se2gis_series, segis_series)

    # segis_series = segis_series + segis_unrealizable_series
    # se2gis_series = se2gis_series + se2gis_unrealizable_series
    # cegis_series = cegis_series + cegis_unrealizable_series
    # Plot one quantile plot
    # cactus_plot_table2(cactus_file, se2gis_series,
                # segis_series, cegis_series)
    # Output a tex table for  realizable benchmarks
    # make_tex_table(exp_setup, table2, tex_table)
    # Output a tex table for unrealizable benchmarks
    # make_tex_unrealizables_table(exp_setup, table2, tex_table2)
    num_benchmarks = len(table)
    print(
        f"Number of benchmarks: {num_benchmarks}")
    print("Solved benchmark per option:")
    for a in series:
        for opt in series[a]:
            num_solved = len(series[a][opt])
            print(
                f"{a :10s} with {opt :5s} solves {num_solved}  ({100.0 * (1.0 * num_solved / (1.0 * num_benchmarks)) :3.1f} %).")
    return {}
