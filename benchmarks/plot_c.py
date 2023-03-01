#!/usr/bin/env python3
import sys
import os
from datetime import datetime
import argparse
import matplotlib.pyplot as plt
import numpy as np
import shutil
from test_c import ResultObject, wmean
# Local files
from commons import *
from definitions import *


all_algos = [
    "all", "no-rstar", "no-predicate-reuse",
    # "no-predicate-reuse-no-rstar",
    "bottom-up", "dfs", "bottom-up-dfs",
    "bu-no-rstar",
    "bu-no-predicate-reuse",
    "bu-no-predicate-reuse-no-rstar",

]

algo_color = {
    "all": "red",
    "no-rstar": "blue",
    "no-predicate-reuse": "purple",
    # "no-predicate-reuse-no-rstar": "green",
    "bottom-up": "gray",
    "dfs": "black",
    "bottom-up-dfs": "yellow",
    "bu-no-rstar": "magenta",
    "bu-no-predicate-reuse": "deeppink",
    "bu-no-predicate-reuse-no-rstar": "lightpink",

}

algo_label = {
    "all": "all",
    "no-rstar": "no rstar",
    "no-predicate-reuse": "no predicates",
    # "no-predicate-reuse-no-rstar": "no rstar, no predicates",
    "bottom-up": "bottom up",
    "dfs": "dfs",
    "bottom-up-dfs": "bottom up, dfs",
    "bu-no-rstar": "bottom up, no rstar",
    "bu-no-predicate-reuse": "bot.up. no p-reuse",
    "bu-no-predicate-reuse-no-rstar": "bot.up no rstar, no p-reuse",
}


def add_or_append(mydict, key, data):
    if key in mydict:
        mydict[key] = mydict[key] + [data]
    else:
        mydict[key] = [data]


def fullly_solved_quantile_plot(series):
    fig, ax = plt.subplots(figsize=(6, 4))
    for algo, serie in series.items():
        s = [x for x in sorted(serie) if x > 0.0]
        ax.plot(s, color=algo_color[algo], label=f"{algo_label[algo]}({len(s)})",
                linestyle="solid", linewidth=1.5)

    ax.set_xlabel("Number of benchmarks solved", fontsize=plot_fontsize)
    ax.set_ylabel("Time", fontsize=plot_fontsize)
    ax.legend(fontsize=plot_fontsize)
    fig.savefig("fully_solved_quantile.pdf", bbox_inches='tight')


def hist_plot(results):
    fig = plt.figure()
    ax = fig.add_axes([0, 0, 1, 1])
    X = 0
    for k, v in results.items():
        ipos = 0.00
        for rk in v:
            algo = rk.optims
            cnt = rk.rstar_hits + rk.num_attempts
            ax.bar(X + ipos, cnt, color=algo_color[algo], width=1)
            ipos += 0.1
            X += 2

    fig.savefig("bar_chart.pdf")


def make_report(input_file, output_file):
    # Stats
    avg_rstar_hits = 0
    avg_rstar_hit_count = 0
    max_rstar_hit = 0

    results = {}
    with open(input_file, 'r') as inc:
        for line in inc.readlines():
            res = ResultObject.parse(line)
            short_name = "/".join(res.name.split("/")[-2:])
            if res:
                add_or_append(results, short_name, res)
                # Collect stats
                if res.optims == "all":
                    avg_rstar_hits = wmean(
                        avg_rstar_hit_count, avg_rstar_hits, 1, res.hit_ratio())
                    avg_rstar_hit_count += 1
                    max_rstar_hit = max(max_rstar_hit, res.hit_ratio())

    complete_results = {}
    for k, v in results.items():
        algos = [res.optims for res in v]
        if sorted(algos) == sorted(all_algos):
            complete_results[k] = sorted(v, key=lambda x: x.optims)

    print(f"Total results: {len(complete_results)}")

    hist_plot(complete_results)

    # Which algorithm "solves more" ?
    score = {}
    for algo in all_algos:
        score[algo] = 0
    for k, v in complete_results.items():
        mc = -1
        balg = "all"
        for result in v:
            cc = result.rstar_hits + result.num_attempts
            if cc > mc:
                balg = result.optims
                mc = cc
        if mc >= 0:
            score[balg] += 1
    print("Best algo on # benchmarks:")
    for algo in all_algos:
        print(f"{algo} : {score[algo]}")

    # Series for quantile plot:
    series = {}
    for algo in all_algos:
        series[algo] = []
        for k, v in complete_results.items():
            for result in v:
                if result.optims == algo:
                    series[algo] += [result.elapsed]
    fullly_solved_quantile_plot(series)

    print(
        f"Average R* hits: {avg_rstar_hits * 100.0 : 3.1f}% (Max: {max_rstar_hit * 100.0 : 3.1f}%)")


if __name__ == "__main__":
    aparser = argparse.ArgumentParser()
    aparser.add_argument(
        "-i", "--input", help="The input file produced by running test.py", type=str, default=None)
    aparser.add_argument(
        "-o", "--output", help="The output text file for the table.", type=str, default="benchmarks/table.txt")
    aparser.add_argument(
        "-c", "--csv", help="The output csv file for results.", type=str, default="benchmarks/constraints_results.csv")
    aparser.add_argument(
        "-e", "--explain", help="Explain where the benchmarks are stored.", action="store_true")
    aparser.add_argument(
        "-y", "--copy", help="Copy figures to $SYND_LOCAL_COPY/figures and $SYND_LOCAL_COPY/tables.", action="store_true"
    )

    args = aparser.parse_args()

    input_file = args.input
    output_file = args.csv
    tex_out = args.output

    filenames = make_report(input_file, output_file)
