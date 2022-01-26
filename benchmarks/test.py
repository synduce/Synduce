#!/usr/bin/env python3
# -*- coding: UTF-8 -*-
import os
import sys
import time
import argparse
import subprocess
import json
# Local helper modules
from definitions import *
from parsing import DataObj


def run_one(progress, bench_id, command, algo, optim, filename, extra_opt):
    print(f"{progress : >11s}  {bench_id} 🏃", end="\r")
    sys.stdout.flush()

    process = subprocess.Popen(
        command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    info = None
    last_refinement_string = ""
    last_verif_time = 0.0
    last_elapsed = 0.0
    last_info = None
    # Poll process for new output until finished
    while True:
        try:
            line = process.stdout.readline()
            data = json.loads(line)
            info = DataObj(data)
            last_refinement_string = info.get_refinement_summary()
            last_verif_time = info.verif_elapsed
            last_elapsed = info.elapsed
            last_info = info
        except Exception as e:
            info = DataObj({})
            info.is_successful = False
            info.verif_elapsed = last_verif_time
            info.elapsed = last_elapsed
            break

        if process.poll() is not None or info.is_successful:
            break
        print(
            f"{progress : >11s}.. benchmarks/{filename} {extra_opt} {algo[1]} {optim[1]} 🏃 at step {info.major_step_count}:{info.minor_step_count}", end="\r")

    return last_info


def run_n(progress, bench_id, realizable, command, algo,
          optim, filename, extra_opt, errors, num_runs, csv_output):

    total_elapsed = 0.0
    total_last_step_elapsed = 0.0
    verif_elapsed = 0.0
    max_e = 0
    min_e = timeout_value
    estim = 0
    delta = 0
    sp = " "
    bench_parts = bench_id.split(".")[0].split("/")
    bench_name = bench_parts[-1]
    info = DataObj({})
    info.is_successful = False
    print(
        f"{progress : >11s} {bench_name: <25s}", end="\r")

    # Run the tests num_run times
    for i in range(num_runs):
        try:
            info = run_one(progress, bench_id, command,
                           algo, optim, filename, extra_opt)
        except:
            info = DataObj({})
            info.is_successful = False

        if (info is not None and
            info.is_successful and
            ((realizable and not info.is_unrealizable) or
                (not realizable and info.is_unrealizable))):
            total_elapsed += info.elapsed
            total_last_step_elapsed += info.last_elapsed
            verif_elapsed += info.verif_elapsed
            max_e = max(info.elapsed, max_e)
            min_e = min(info.elapsed, min_e)
            estim = float(estim * i + info.elapsed) / float(i + 1)

            msg = f"[estimate: {estim: 4.3f} s] ({i}/{num_runs} runs){sp : <30s}"
            print(msg, end="\r")
            sys.stdout.flush()
        else:
            break

    elapsed = total_elapsed / num_runs
    last_step_elapsed = total_last_step_elapsed / num_runs
    verif_elapsed = verif_elapsed / num_runs
    delta = 1000 * max(abs(max_e - elapsed), abs(min_e-elapsed))
    sp = " "
    csvline = "?,?,?,?,?,?"

    if info is not None and info.is_successful and ((realizable and not info.is_unrealizable) or (not realizable and info.is_unrealizable)):
        delta_str = f"{delta : .0f}ms"
        if (float(delta) / (1000.0 * elapsed)) > 0.05:
            delta_str = f"{delta_str} !"
        else:
            delta_str = f"{delta_str}  "
        timing = f"average: {elapsed: 4.3f}s ±{delta_str}"

        if info.proved_by_induction:
            p_by_induction = "✓"
        else:
            p_by_induction = "~"

        if info.classified_by_induction:
            c_by_induction = "✓"
        else:
            c_by_induction = "~"
        refinement_rounds = info.get_refinement_summary()
        if "." in refinement_rounds:
            induction_info = f"B:{c_by_induction},B':{p_by_induction}"
        else:
            induction_info = "        "

        msg = f"{progress : >11s} ✅ {info.algo: <6s} : {bench_name : <33s} ×{num_runs} runs, {str(timing): <30s} {induction_info} | R: {refinement_rounds} {sp : <20s} "
        print(msg)
        csvline = f"{elapsed: 4.3f}, {last_step_elapsed: 4.3f}, {delta : .0f},{refinement_rounds},{c_by_induction},{p_by_induction},{verif_elapsed : 4.3f}"
    else:
        errors += [bench_id]
        print(f"{progress: >11s} ❌ {bench_id : <90s}")
        if info is not None:
            csvline = f"N/A,{info.last_elapsed: 4.3f},N/A,f{info.major_step_count},N/A,N/A,{info.verif_elapsed : 4.3f}"
        else:
            csvline = f"N/A,N/A,N/A,N/A,N/A,N/A,N/A"

    sys.stdout.flush()

    return errors,  elapsed, csvline


def run_benchmarks(input_files, algos, optims, num_runs=1, csv_output=None, exit_err=False):
    benchmark_cnt = 0
    benchmark_total = len(input_files) * len(algos) * len(optims)
    errors = []
    start = time.time()
    prev_bench_cat = "x"
    for filename_with_opt in input_files:
        filename = filename_with_opt[0]
        category = os.path.dirname(filename)
        extra_opt = filename_with_opt[1]
        if len(filename_with_opt) >= 3:
            realizable = filename_with_opt[2]
        else:
            realizable = True
        csvline_all_algos = []
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
                           (timeout, exec_path, algo[1], optim[1], extra_opt,
                            os.path.realpath(os.path.join(
                                "benchmarks", filename)),
                            soln_file_opt, gen_opt))
                bench_cat = "->".join(bench_id.split(".")[0].split("/")[:-1])
                if not bench_cat == prev_bench_cat:
                    print(f"\n⏺ Category: {bench_cat}")
                    prev_bench_cat = bench_cat
                # Run the benchmark n times.
                errors, elapsed, csvline = run_n(progress, bench_id, realizable, command, algo,
                                                 optim, filename, extra_opt, errors, num_runs, csv_output)
                csvline_all_algos += [f"{algo[0]}:{optim[0]}", csvline]

        if csv_output:
            line = ",".join([filename] + csvline_all_algos) + "\n"
            csv_output.write(line)

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
    # 4 is for testing benchmarks with constraints, with p-boudning and symbolic CEGIS,\
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
        "-b", "--benchmarks", help="Run a set of benchmarks.", type=str,
        choices=["all", "constraint", "lifting", "base", "unr", "small"], nargs="+", default=None)
    aparser.add_argument(
        "-c", "--compare", help="Compare with segis or cegis", type=str,
        choices=["segis", "cegis", "segis0"], default=None, nargs="+")
    aparser.add_argument(
        "--single", help="Run the lifting benchmark in benchmarks/[FILE]", type=str, default=None)
    aparser.add_argument(
        "-n", "--num-runs", help="Run each benchmark NUM times.", type=int, default=1
    )
    aparser.add_argument(
        "-x", "--unrealizable", help="Benchmark is unrealizable.", default=True, action="store_false"
    )
    aparser.add_argument(
        "-t", "--table", help=table_info, type=int, default=-1)
    aparser.add_argument(
        "--summary", help="Give a summary of benchmarks.", action="store_true")

    aparser.add_argument(
        "-T", "--timeout", help="Set the timeout in seconds.", type=int, default=600)
    args = aparser.parse_args()

    if args.summary:
        summarize()
        exit()

    # Optional output settings.
    generate_solutions = args.generate_solutions
    generate_benchmarks = args.generate_benchmarks

    csv_output = None
    if args.output is not None:
        try:
            csv_output = open(args.output, 'a+', encoding='utf-8')
        except:
            pass

    # Set number of runs and timeout
    runs = args.num_runs
    timeout_value = args.timeout

    # Benchmark set selection
    run_lifting_benchmarks = False
    run_constraint_benchmarks = False
    run_base_benchmarks = False
    run_unrealizable = False
    run_kick_the_tires_only = False

    if args.benchmarks is not None:
        if "constraint" in args.benchmarks:
            run_constraint_benchmarks = True
        if "lifting" in args.benchmarks:
            run_lifting_benchmarks = True
        if "base" in args.benchmarks:
            run_base_benchmarks = True
        if "small" in args.benchmarks:
            run_kick_the_tires_only = True
        if "unr" in args.benchmarks:
            run_unrealizable = True
        if "all" in args.benchmarks:  # e.g. benchmarks = "all"
            run_lifting_benchmarks = True
            run_constraint_benchmarks = True
            run_base_benchmarks = True

    # Background solver selection : cvc4 by default.
    cvc = "--cvc4"
    if args.cvc5:
        cvc = "--cvc5"

    other_alg = []
    print(args.compare)
    if args.compare is not None:
        if "segis" in args.compare:
            other_alg += [["segis", "--segis " + cvc]]
        if "segis0" in args.compare:
            other_alg += [["segis0", "--segis -u " + cvc]]
        if "cegis" in args.compare:
            other_alg += [["cegis", "--cegis " + cvc]]

    # Run a single file if --single has an argument
    if args.single and args.single != "":
        print("Running single file")
        algos = [["se2gis", cvc]] + other_alg
        optims = [["all", ""]]
        binfo = str(args.single).split("+")

        if len(binfo) == 1:
            binfo = [str(binfo[0]), "", args.unrealizable]
        run_benchmarks([binfo], algos, optims,
                       csv_output=csv_output, num_runs=runs)
        exit()

    bench_set = []
    # Running specific sets if we're supposed to.
    if run_kick_the_tires_only:
        algos = [["se2gis", cvc]] + other_alg
        optims = [["all", ""]]
        run_benchmarks(kick_the_tires_set, algos,
                       optims, csv_output=csv_output, num_runs=runs)
        exit(0)

    # If we were supposed to run a specific set of benchmarks, we're done
    if run_base_benchmarks or run_constraint_benchmarks or run_lifting_benchmarks or run_unrealizable:
        algos = [["se2gis", cvc]] + other_alg

        if run_lifting_benchmarks:
            optims = [["all", ""]]
            bench_set += lifting_benchmarks

        if run_constraint_benchmarks:
            optims = [["all", ""]]
            bench_set += constraint_benchmarks

        if run_base_benchmarks:
            optims = [["all", ""]]
            bench_set += base_benchmark_set

        if run_unrealizable:
            optims = [["all", ""]]
            bench_set += unrealizable_benchmarks

        run_benchmarks(bench_set, algos,
                       optims, csv_output=csv_output, num_runs=runs)
        exit()

    # === === TABLES and CAV runs === ===

    # Algorithm set selection by table number.
    table_no = args.table
    print(f"Running Table {table_no} Benchmarks.")

    # Table 1 / CAV 21 paper : compare Synduce and Baseline
    if table_no == 1:

        algos = [
            ["se2gis", ""],
            ["segis", "--segis"]
        ]
        optims = [["all", ""]]

    # Table 2 / CAV 21 paper : compare Synduce, Baseline and Concrete CEGIS
    elif table_no == 2:

        algos = [
            ["se2gis", ""],
            ["segis", "--segis"],
            ["cegis", "--cegis"]
        ]
        optims = [["all", ""]]

    # Table 3 / CAV21 paper : compare Synduce, Baseline with optimizations on/off
    elif table_no == 3:

        algos = [
            ["se2gis", ""],
            ["segis", "--segis"],
        ]

        optims = [
            ["all", ""],
            ["ini", "-c"],
            ["split", "-st"],  # equation system splitting optimizations
            ["syn", "--no-syndef --no-gropt --no-rew"],  # syntactic optimizations
            ["off", "-st --no-syndef --no-gropt --no-rew"]
        ]

    # Table 4 / Test
    elif table_no == 4:
        algos = [["se2gis", "--cvc4"], ["segis", "--segis --cvc4"]]
        optims = [["all", ""]]

    # Table 5 / Test with cvc4 against baseline comparison
    elif table_no == 5:
        algos = [["se2gis", "--cvc4"],
                 ["segis", "--segis --cvc4"],
                 ["segis0", "--segis --cvc4 -u"]]
        optims = [["all", ""]]

    # No table - just run the base algorithm.
    else:
        algos = [["se2gis", "--cvc4"], ["segis", "--segis --cvc4"]]
        optims = [["all", ""]]

    # If no csv output has been provided, generate a timestamped output file
    if not csv_output:
        timestr = time.strftime(timestamp_definition)
        filename = f"benchmarks/data/exp/bench_{timestr}_table{table_no}.csv"
        print(f"No output file given, I will be writing in {filename}")
        csv_output = open(filename, 'a+', encoding='utf-8')

    # Benchmark set selection.
    input_files = []
    if args.kick_the_tires:
        input_files = kick_the_tires_set

    else:
        if table_no == 2:
            input_files = base_benchmark_set
        elif table_no == 3:
            input_files = base_benchmark_set
        elif table_no == 4:
            input_files = constraint_benchmarks
        elif table_no == 5:
            input_files = constraint_benchmarks + unrealizable_benchmarks
        else:
            input_files = kick_the_tires_set

    run_benchmarks(input_files, algos, optims, csv_output=csv_output,
                   exit_err=True, num_runs=runs)
    if csv_output:
        csv_output.close()
