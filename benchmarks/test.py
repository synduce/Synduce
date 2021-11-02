#!/usr/bin/env python3
# -*- coding: UTF-8 -*-
import os
import sys
import time
import argparse
import subprocess
import json
# Local helper modules
import definitions
from parsing import DataObj


def run_one_old(progress, bench_id, command, algo, optim, filename, extra_opt, errors, raw_output):
    if raw_output is not None:
        raw_output.write(f"B:{bench_id}\n")

    print(f"{progress : >11s}  {bench_id} 🏃", end="\r")
    sys.stdout.flush()

    process = subprocess.Popen(
        command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    buf = ""
    major_step_count = 1
    minor_step_count = 1
    prev_major_step = 1
    # Poll process for new output until finished
    while True:
        nextline = process.stdout.readline()
        if process.poll() is not None:
            break
        sys.stdout.flush()
        line = nextline.decode('utf-8')
        # Decode the line information
        stats = line.split(",")
        if len(stats) >= 3:
            try:
                major_step_count = int(stats[0])
            except:
                major_step_count = None
        is_result_line = (line == "success") or (
            (major_step_count is not None) and (major_step_count > 0))
        if is_result_line:
            if major_step_count > prev_major_step:
                minor_step_count = 1
            else:
                minor_step_count += 1
            prev_major_step = major_step_count
            sp = " "
            print(
                f"{progress : >11s}.. benchmarks/{filename} {extra_opt} {algo[1]} {optim[1]} 🏃 at step {major_step_count}:{minor_step_count}", end="\r")
            buf += line
        if raw_output is not None and is_result_line:
            raw_output.write(line)

    print("", end="\r")
    output = buf.strip().split("\n")
    elapsed = -1
    if len(output) >= 2 and output[-1] == "success":
        elapsed = float(output[-2].split(",")[2])
    else:
        errors += [bench_id]
    return errors, elapsed


def run_one(progress, bench_id, command, algo, optim, filename, extra_opt):
    print(f"{progress : >11s}  {bench_id} 🏃", end="\r")
    sys.stdout.flush()

    process = subprocess.Popen(
        command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    info = None
    # Poll process for new output until finished
    while True:
        info = DataObj(json.loads(process.stdout.readline()))
        if process.poll() is not None or info.is_successful:
            break
        print(
            f"{progress : >11s}.. benchmarks/{filename} {extra_opt} {algo[1]} {optim[1]} 🏃 at step {info.major_step_count}:{info.minor_step_count}", end="\r")

    return info


def run_n(progress, bench_id, command, algo,
          optim, filename, extra_opt, errors, num_runs, csv_output):

    total_elapsed = 0
    max_e = 0
    min_e = timeout_value
    estim = 0
    delta = 0
    sp = " "
    bench_parts = bench_id.split(".")[0].split("/")
    bench_category = "->".join(bench_parts[:-1])
    bench_name = bench_parts[-1]
    print(
        f"{progress : >11s} {bench_name: <25s}", end="\r")
    for i in range(num_runs):
        info = run_one(progress, bench_id, command,
                       algo, optim, filename, extra_opt)
        if info.is_successful:
            total_elapsed += info.elapsed
            max_e = max(info.elapsed, max_e)
            min_e = min(info.elapsed, min_e)
            estim = float(estim * i + info.elapsed) / float(i + 1)

            msg = f"[estimate: {estim: 4.3f} s] ({i}/{num_runs} runs){sp : <30s}"
            print(msg, end="\r")
            sys.stdout.flush()
        else:
            print(f"{progress : >11s} ❌ {bench_id : <70s}{sp : <10s}", end="\r")
            sys.stdout.flush()
            return (errors + [bench_id]),  info.elapsed, 0

    elapsed = total_elapsed / num_runs
    delta = 1000 * max(abs(max_e - elapsed), abs(min_e-elapsed))
    sp = " "
    if elapsed > 0:
        delta_str = f"{delta : .0f}ms"
        if (float(delta) / (1000.0 * elapsed)) > 0.05:
            delta_str = f"{delta_str} !"
        else:
            delta_str = f"{delta_str}  "
        timing = f"average: {elapsed: 4.3f}s ±{delta_str}"
        msg = f"{progress : >11s} ✅ {bench_name : <40s} ×{num_runs} runs, {str(timing): <30s} R: {info.get_refinement_summary()} {sp : <40s} "
        print(msg)
    else:
        print(f"{progress: >11s} ❌ {bench_id : <120s}")
    sys.stdout.flush()

    return errors,  elapsed, delta


def run_benchmarks(input_files, algos, optims, num_runs=1, raw_output=None, exit_err=False):
    benchmark_cnt = 0
    benchmark_total = len(input_files) * len(algos) * len(optims)
    errors = []
    start = time.time()
    prev_bench_cat = "x"
    for filename_with_opt in input_files:
        filename = filename_with_opt[0]
        category = os.path.dirname(filename)
        extra_opt = filename_with_opt[1]
        for algo in algos:
            for optim in optims:
                benchmark_cnt += 1
                # Benchmark generation optional
                gen_opt = ""
                if generate_benchmarks:
                    if not os.path.exists(os.path.dirname("test/tmp_benchmarks/%s" % filename)):
                        os.makedirs(os.path.dirname(
                            "test/tmp_benchmarks/%s" % filename))
                    gen_opt = "--generate-benchmarks=\"test/tmp_benchmarks\""
                # Optional solution generation
                soln_file_opt = ""
                if generate_solutions:
                    if not os.path.exists(os.path.dirname("extras/solutions/%s" % filename)):
                        os.makedirs(os.path.dirname(
                            "extras/solutions/%s" % filename))
                    soln_file_opt = "-o extras/solutions/%s/" % category

                # Print benchmark name, algorithm used and optimization options.
                bench_id = "%s,%s+%s" % (filename, algo[0], optim[0])
                progress = f"({benchmark_cnt} / {benchmark_total})"
                command = ("%s %s %s -j --json-progress %s %s %s %s %s" %
                           (definitions.timeout, exec_path, algo[1], optim[1], extra_opt,
                            os.path.realpath(os.path.join(
                                "benchmarks", filename)),
                            soln_file_opt, gen_opt))
                bench_cat = "->".join(bench_id.split(".")[0].split("/")[:-1])
                if not bench_cat == prev_bench_cat:
                    print(f"\n⏺ Category: {bench_cat}")
                    prev_bench_cat = bench_cat
                # Run the benchmark n times.
                errors, elapsed, delta = run_n(progress, bench_id, command, algo,
                                               optim, filename, extra_opt, errors, num_runs, raw_output)

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

    table_info = "Tables: #1 is for CAV21 paper Table 1,\
    # 2 is for CAV21 paper Table 2,\
    # 3 is for CAV21 paper Table 3,\
    # 4 is for testing benchmarks with constraints, with partial bounding only,\
    # 5 is for testing benchmarks with constraints, with p-bounding and Symbolic-CEGIS."

    sys.stdout.flush()
    aparser = argparse.ArgumentParser()
    aparser.add_argument(
        "--cvc5", help="Force use of CVC5 (useful if you can't install CVC4 on Mac M1)", action="store_true")
    aparser.add_argument(
        "--generate-benchmarks", help="Generate SyGuS benchmarks.", action="store_true")
    aparser.add_argument(
        "--generate-solutions", help="Generate solutions in extras/solution.", action="store_true")
    aparser.add_argument(
        "--kick-the-tires", help="Run a subset of benchmarks.", action="store_true")
    aparser.add_argument(
        "-o", "--output", help="Dump Synduce output in -i mode to file (appending to file).", type=str, default=-1)
    aparser.add_argument(
        "-b", "--benchmarks", help="Run the lifting benchmarks.", type=str,
        choices=["all", "constraint", "lifting", "base", "small"], default="small")
    aparser.add_argument(
        "-n", "--num-runs", help="Run each benchmark NUM times.", type=int, default=1
    )
    aparser.add_argument(
        "-t", "--table", help=table_info, type=int, default=-1)
    aparser.add_argument(
        "--summary", help="Give a summary of benchmarks.", action="store_true")

    aparser.add_argument(
        "-T", "--timeout", help="Set the timeout in seconds.", type=int, default=600)
    args = aparser.parse_args()

    if args.summary:
        definitions.summarize()
        exit()

    # Algorithm set selection by table number.
    table_no = args.table
    # Optional output settings.
    generate_solutions = args.generate_solutions
    generate_benchmarks = args.generate_benchmarks

    run_test_only = table_no == -1 or args.run

    raw_output = None
    if args.output is not None:
        try:
            raw_output = open(args.output, 'a+', encoding='utf-8')
        except:
            pass

    # Set number of runs and timeout
    runs = args.num_runs
    timeout_value = args.timeout

    # Benchmark set selection
    run_lifting_benchmarks = False
    run_constraint_benchmarks = False
    run_base_benchmarks = False
    run_kick_the_tires_only = False

    if args.benchmarks == "constraint":
        run_constraint_benchmarks = True
    elif args.benchmarks == "lifting":
        run_lifting_benchmarks = True
    elif args.benchmarks == "base":
        run_base_benchmarks = True
    elif args.benchmarks == "small":
        run_kick_the_tires_only = True
    elif args.benchmarks is not None:  # e.g. benchmarks = "all"
        run_lifting_benchmarks = True
        run_constraint_benchmarks = True
        run_base_benchmarks = True

    # Background solver selection : cvc4 by default.
    cvc = "--cvc4"
    if args.cvc5:
        cvc = "--cvc5"

    bench_set = []
    # Running specific sets if we're supposed to.
    if run_kick_the_tires_only:
        algos = [["partbnd", cvc]]
        optims = [["all", ""]]
        run_benchmarks(definitions.kick_the_tires_set, algos,
                       optims, raw_output=raw_output, num_runs=runs)
        exit(0)

    if run_lifting_benchmarks:
        algos = [["partbnd", cvc]]
        optims = [["all", ""]]
        bench_set += definitions.lifting_benchmarks

    if run_constraint_benchmarks:
        algos = [["partbnd", cvc]]
        optims = [["all", ""]]
        bench_set += definitions.constraint_benchmarks

    if run_base_benchmarks:
        algos = [["partbnd", cvc]]
        optims = [["all", ""]]
        bench_set += definitions.base_benchmark_set

    # If we were supposed to run a specific set of benchmarks, we're done
    if run_base_benchmarks or run_constraint_benchmarks or run_lifting_benchmarks:
        run_benchmarks(bench_set, algos,
                       optims, raw_output=raw_output, num_runs=runs)
        exit()

    # === === TABLES and CAV runs === ===
    if run_test_only:
        algos = [["partbnd", ""]]
        optims = [["all", cvc]]

    # Table 1 / CAV 21 paper : compare Synduce and Baseline
    elif table_no == 1:

        algos = [
            ["partbnd", "--no-gropt"],
            ["acegis", "--acegis --no-gropt"]
        ]
        optims = [["all", ""]]

    # Table 2 / CAV 21 paper : compare Synduce, Baseline and Concrete CEGIS
    elif table_no == 2:

        algos = [
            ["partbnd", "--no-gropt"],
            ["acegis", "--acegis --no-gropt"],
            ["ccegis", "--ccegis --no-gropt"]
        ]
        optims = [["all", ""]]

    # Table 3 / CAV21 paper : compare Synduce, Baseline with optimizations on/off
    elif table_no == 3:

        algos = [
            ["partbnd", "--no-gropt"],
            ["acegis", "--acegis --no-gropt"],
        ]

        optims = [
            ["all", ""],
            ["ini", "-c --no-gropt"],
            ["st", "-st --no-gropt"],
            ["d", "--no-syndef --no-gropt"],
            ["off", "-st --no-syndef --no-gropt"]
        ]

    # Table 4 / Test
    elif table_no == 4:
        algos = [["partbnd"]]
        optims = [["all", ""]]

    # Table 5 / Test with cvc4 against baseline comparison
    elif table_no == 5:
        algos = [["partbnd", "--cvc4"], ["acegis", "--acegis --cvc4"]]
        optims = [["all", ""]]

    # No table - just run the base algorithm.
    else:
        algos = [["partbnd", ""]]
        optims = [["all", ""]]

    # Benchmark set selection.

    if args.kick_the_tires:
        input_files = definitions.kick_the_tires_set

    else:
        if table_no == 2:
            input_files = definitions.reduced_benchmark_set_table2
        elif table_no == 3:
            input_files = definitions.reduced_benchmark_set_table3
        elif table_no == 4:
            input_files = definitions.constraint_benchmarks
        elif table_no == 5:
            input_files = definitions.constraint_benchmarks + definitions.lifting_benchmarks
        elif run_test_only:
            input_files = definitions.benchmark_set
        else:
            input_files = definitions.kick_the_tires_set

    run_benchmarks(input_files, algos, optims, raw_output=raw_output,
                   exit_err=True, num_runs=runs)
