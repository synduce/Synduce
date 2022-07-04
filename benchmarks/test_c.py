#!/usr/bin/env python3
# -*- coding: UTF-8 -*-
import os
from signal import pause
import sys
import time
import argparse
# Local helper modules
from definitions import *
from runner import *

from configurations_set_search import *

global tmp_outfile
tmp_outfile = "benchmarks/tmp.txt"


def wmean(na, a, nb, b):
    return ((a * na + b * nb) / (na + nb))


class ResultObject(object):
    def __init__(self, name) -> None:
        self.name = name
        self.runs = 0
        self.total_confs = 0
        self.rstar_hits = 0
        self.lemma_reuse = 0
        self.num_attempts = 0
        self.num_solutions = 0
        self.num_unrealizable = 0
        self.num_failures = 0
        self.elapsed = 0.0
        self.finished = 0
        self.found_best = 0
        self.optims = "all"

    def merge(self, other):
        na = self.runs
        nb = other.runs
        if nb == 0:
            pass
        else:
            self.total_confs = max(self.total_confs, other.total_confs)
            self.rstar_hits = wmean(
                na, self.rstar_hits, nb, other.rstar_hits)
            self.lemma_reuse = wmean(
                na, self.lemma_reuse, nb, other.lemma_reuse)
            self.num_attempts = wmean(
                na, self.num_attempts, nb, other.num_attempts)
            self.num_solutions = wmean(
                na, self.num_solutions, nb, other.num_solutions)
            self.num_unrealizable = wmean(
                na, self.num_unrealizable, nb, other.num_unrealizable)
            self.num_failures = wmean(
                na, self.num_failures, nb, other.num_failures)
            self.elapsed = wmean(
                na, self.elapsed, nb, other.elapsed)
            self.finished = wmean(
                na, self.finished, nb, other.finished)
            self.found_best = wmean(
                na, self.found_best, nb, other.found_best)
            self.runs += other.runs

    def pretty_str(self, optim):
        self.optims = optim
        s = f"{self.name : <25s} {optim: <30s} Comp:{self.finished} "
        s += f"T: {self.elapsed :4.3f}"
        s += f" ({int(self.num_attempts)} / {int(self.total_confs)}) ?:{self.found_best} "
        s += f" [S:{int(self.num_solutions)}+U:{int(self.num_unrealizable)}+F:{int(self.num_failures)}]"
        s += f" R*:{self.rstar_hits}, P*:{self.lemma_reuse}"
        return s

    def csvline(self, benchfile, optims):
        s = f"{benchfile},{optims},{self.finished},"
        s += f"{self.elapsed :4.3f},"
        s += f"{int(self.num_attempts)},{int(self.total_confs)},"
        s += f"{int(self.num_solutions)},{int(self.num_unrealizable)},{int(self.num_failures)},"
        s += f"{self.rstar_hits},{self.lemma_reuse},{self.found_best}"
        return s

    def hit_ratio(self,):
        if self.num_attempts > 0:
            return (self.rstar_hits / (self.num_attempts + self.rstar_hits))
        else:
            return 0

    def parse(csvline):
        cells = csvline.split(",")
        if len(cells) < 11:
            return None
        else:
            try:
                res = ResultObject(str(cells[0]))
                res.optims = str(cells[1])
                res.finished = float(cells[2])
                res.elapsed = float(cells[3])
                res.num_attempts = int(cells[4])
                res.total_confs = int(cells[5])
                res.num_solutions = int(cells[6])
                res.num_unrealizable = int(cells[7])
                res.num_failures = int(cells[8])
                res.rstar_hits = float(cells[9])
                res.lemma_reuse = float(cells[10])
                if len(cells) > 11:
                    res.covratio = float(cells[11])
                return res
            except Exception as e:
                print(e)
                return None

    def set_init(self, cells):
        self.runs = 1
        self.total_confs = int(cells[1].split(":")[1])

    def set_partial_result(self, cells):
        self.num_attempts += 1
        # 0 problem id
        # 1 rstar hits
        self.rstar_hits = int(cells[1].split(":")[1])
        # 2 lemma reuse
        self.lemma_reuse = int(cells[2].split(":")[1])
        # 3 algo type
        # 4 elapsed
        # 5 verif elapsed
        # 6 solution kind
        kind = int(cells[6].split(":")[1])
        if len(cells) >= 8:
            self.found_best = 1 if bool(cells[7].split(":")[1]) else 0

        if kind == 0:
            self.num_solutions += 1
        elif kind == 1:
            self.num_unrealizable += 1
        else:
            self.num_failures += 1

    def set_final_result(self, cells):
        self.finished = 1
        # 0 elapsed time
        self.elapsed = float(cells[0].split(":")[1])
        # 1 num solutions
        self.num_solutions = int(cells[1].split(":")[1])
        # 2 num unrealizable
        self.num_unrealizable = int(cells[2].split(":")[1])
        # 3 num failures
        self.num_failures = int(cells[3].split(":")[1])
        # 4 num rstar hits
        self.rstar_hits = int(cells[4].split(":")[1])
        # 5 lemma reuse
        self.lemma_reuse = int(cells[5].split(":")[1])
        if len(cells) >= 7:
            # 6 coverage ratio
            self.found_best = 1 if bool(cells[6].split(":")[1]) else 0


def run_once(name, command):
    if os.path.exists(tmp_outfile):
        os.remove(tmp_outfile)
    process = subprocess.Popen(
        command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    while process.poll() is None:
        time.sleep(0.1)

    res = ResultObject(name)

    try:
        with open(tmp_outfile, 'r') as input_file:
            for line in input_file.readlines():
                cells = line.split(",")
                # This should be the first line
                if cells[0].startswith("problem-file:"):
                    res.set_init(cells)
                # This shoud be an intermediate result line
                elif cells[0].startswith("id:"):
                    res.set_partial_result(cells)
                # This is the final line
                elif cells[0].startswith("finished:"):
                    res.set_final_result(cells)
    except:
        pass

    return res


def run_n_times(n, name, command):
    res = ResultObject(name)
    res.runs = 0
    for _ in range(n):
        res.merge(run_once(name, command))
    return res


def run_benchmarks(input_files, optims, num_runs=1, csv_output=None, exit_err=False):
    benchmark_cnt = 0
    benchmark_total = len(input_files) * len(optims)
    errors = []
    start = time.time()
    prev_bench_cat = "x"
    for filename_with_opt in input_files:
        filename = filename_with_opt[0]
        category = os.path.dirname(filename)
        extra_opt = filename_with_opt[1]

        csvline_all_algos = []
        for optim in optims:
            benchmark_cnt += 1
            # Print benchmark name, algorithm used and optimization options.
            bench_id = "%s,%s" % (filename, optim[0])
            progress = f"{benchmark_cnt}/{benchmark_total}"
            benchfile = os.path.realpath(os.path.join("benchmarks", filename))

            command = f"{timeout} {exec_path} -A -O {tmp_outfile} {optim[1]} {extra_opt} {benchfile}"

            bench_cat = "->".join(bench_id.split(".")[0].split("/")[:-1])
            if not bench_cat == prev_bench_cat:
                print(f"\n⏺ Category: {bench_cat}")
                prev_bench_cat = bench_cat

            # Run the benchmark n times.
            benchname = filename.split("/")[-1]
            res = run_n_times(num_runs, benchname, command)

            print(res.pretty_str(optim[0]))

            if csv_output:
                csv_output.write(res.csvline(benchfile, optim[0]) + "\n")

    elapsed = time.time() - start
    if len(errors) <= 0:
        print(
            f"\n✅ All {benchmark_cnt} benchmarks passed in {elapsed: 5.1f} s.")
    else:
        print(
            f"\n❌ {len(errors)} errors out of {benchmark_cnt} benchmarks in {elapsed: 5.1f} s.")
        print("Problematic benchmarks with algorithm:")
        for e in errors:
            print(e)
        if exit_err:
            exit(-1)


if __name__ == "__main__":
    root = os.getcwd()
    exec_path = os.path.join(root, "_build/default/bin/Synduce.exe")

    sys.stdout.flush()
    aparser = argparse.ArgumentParser()

    aparser.add_argument(
        "-o", "--output", help="Dump Synduce output in -i mode to file (appending to file).", type=str, default=-1)
    aparser.add_argument(
        "-b", "--benchmarks", help="Run a set of benchmarks.", type=str,
        choices=["all", "options0", "options1", "options2"], nargs="+", default=None)
    aparser.add_argument(
        "-c", "--compare", help="Compare with segis or cegis", type=str,
        choices=["segis", "cegis", "segis0"], default=None, nargs="+")
    aparser.add_argument(
        "--single", help="Run the lifting benchmark in benchmarks/[FILE]", type=str, default=None)
    aparser.add_argument(
        "-m", "--multi", help="Run the benchmark with multiple solution.", default=False, action="store_true")
    aparser.add_argument(
        "-n", "--num-runs", help="Run each benchmark NUM times.", type=int, default=1
    )
    aparser.add_argument(
        "-t", "--test-set", help="Test set (1,2,3)", type=int, default=1)
    aparser.add_argument(
        "-x", "--unrealizable", help="Benchmark is unrealizable.", default=True, action="store_false"
    )

    aparser.add_argument(
        "--summary", help="Give a summary of benchmarks.", action="store_true")

    aparser.add_argument(
        "-T", "--timeout", help="Set the timeout in seconds.", type=int, default=600)
    aparser.add_argument(
        "-Z", "--solve-timeout", help="Set the single instance solver timeout in seconds.",
        type=float, default=-1.0)
    args = aparser.parse_args()

    csv_output = None
    if args.output is not None:
        try:
            csv_output = open(args.output, 'a+', encoding='utf-8')
        except:
            pass

    if args.solve_timeout > 0:
        solve_timeout = args.solve_timeout
    else:
        solve_timeout = 600.0

    optims = [
        ["all", f"--solve-timeout={solve_timeout}"]]

    optims_set_0 = [
        ["all", f"--solve-timeout={solve_timeout}"],
        ["no-rstar", f"--multi-no-rstar --solve-timeout={solve_timeout}"],
        ["no-predicate-reuse",
            f"--reuse-predicates-off --solve-timeout={solve_timeout}"],
    ]
    optims_set_1 = [
        ["bottom-up", f"--multi-strategy=bu --solve-timeout={solve_timeout}"],
        ["bu-no-rstar",
            f"--multi-strategy=bu --multi-no-rstar --solve-timeout={solve_timeout}"],
        ["dfs", f"--multi-use-dfs --solve-timeout={solve_timeout}"],
    ]

    optims_set_2 = [
        ["bottom-up-dfs",
         f"--multi-strategy=bu --multi-use-dfs --solve-timeout={solve_timeout}"],
        ["bu-no-predicate-reuse",
            f"--multi-strategy=bu --reuse-predicates-off --solve-timeout={solve_timeout}"],
        ["bu-no-predicate-reuse-no-rstar",
            f"--multi-strategy=bu --reuse-predicates-off --multi-no-rstar --solve-timeout={solve_timeout}"],
    ]

    bench_set = incomplete_benchmarks_set0

    test_set = args.test_set

    if args.benchmarks and "all" in args.benchmarks:
        run_benchmarks(bench_set, optims, csv_output=csv_output,
                       num_runs=args.num_runs)
    elif test_set == 1:
        tmp_outfile = "benchmarks/tmp0.txt"
        run_benchmarks(bench_set, optims_set_0, csv_output="output0.csv",
                       num_runs=args.num_runs)
    elif test_set == 2:
        tmp_outfile = "benchmarks/tmp1.txt"
        run_benchmarks(bench_set, optims_set_1, csv_output="output1.csv",
                       num_runs=args.num_runs)
    elif test_set == 3:
        tmp_outfile = "benchmarks/tmp2.txt"
        run_benchmarks(bench_set, optims_set_2, csv_output="output2.csv",
                       num_runs=args.num_runs)
    else:
        tmp_outfile = "benchmarks/tmpsmall.txt"
        run_benchmarks(incomplete_benchmarks, optims, csv_output=csv_output,
                       num_runs=args.num_runs)
