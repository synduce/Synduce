#!/usr/bin/env python3
# -*- coding: UTF-8 -*-
import os
import sys
import time
import argparse
import subprocess

# Timeout for all experiments.
timeout_value = 400
# Maximum 4gb memory - this should not be limiting!
memout_value = 8000 * (2 ** 10)  # 4GB memory limit

if sys.platform.startswith('linux'):
    timeout = ("timeout %i" %
               (timeout_value))
elif sys.platform.startswith('darwin'):
    timeout = ("timelimit -t%i" % timeout_value)
else:
    print("Platform %s not supported" % sys.platform)
    exit(-1)

kick_the_tires_set = [
    ["list/sumhom.pmrs", ""],
    ["ptree/sum.pmrs", ""],
    ["tree/sumtree.pmrs", ""],
    ["tailopt/sum.pmrs", ""],
    ["treepaths/sum.pmrs", ""],
    ["treepaths/height.pmrs", ""],
    ["treepaths/maxPathWeight.pmrs", ""],
    ["ptree/mul.pmrs", ""],
    ["tree/maxtree.pmrs", ""],
    ["tree/min.pmrs", ""],
    ["tree/maxtree2.pmrs", ""],
    ["list/sumodds.pmrs", ""],
    ["list/prodhom.pmrs", ""],
    ["list/polyhom.pmrs", ""],
    ["list/hamming.pmrs", ""],
    ["list/sumevens.pmrs", ""],
    ["list/lenhom.pmrs", ""],
    ["list/last.pmrs", ""],
    ["constraints/sortedlist/count_lt.ml", ""],
    ["constraints/bst/count_lt.ml", "-NB"],
    ["list/largest_diff_sorted_list_nohead.ml", ""],
    ["list/poly_no_fac.ml", ""],
]

reduced_benchmark_set_table2 = [
    ["list/sumhom.pmrs", ""],
    ["ptree/sum.pmrs", ""],
    ["tree/sumtree.pmrs", ""],
    ["tailopt/sum.pmrs", ""],
    ["tailopt/mps.pmrs", ""],
    ["treepaths/sum.pmrs", ""],
    ["treepaths/height.pmrs", ""],
    ["treepaths/leftmostodd.pmrs", "-b 6"],
    ["ptree/mul.pmrs", ""],
    ["tree/maxtree.pmrs", ""],
    ["tree/min.pmrs", ""],
    ["tree/poly.pmrs", ""],
    ["list/sumevens.pmrs", ""],
    ["list/polyhom.pmrs", ""],
    ["list/minhom.pmrs", ""],
    ["list/last.pmrs", ""],
    ["list/msshom.pmrs", ""],
    ["tree/sorted.pmrs", "-t"],
    ["tree/mips.pmrs", ""],
]

reduced_benchmark_set_table3 = [
    ["list/sumhom.pmrs", ""],
    ["ptree/sum.pmrs", ""],
    ["tree/sumtree.pmrs", ""],
    ["tailopt/sum.pmrs", ""],
    ["tailopt/mps.pmrs", ""],
    ["treepaths/sum.pmrs", ""],
    ["treepaths/height.pmrs", ""],
    ["ptree/mul.pmrs", ""],
    ["tree/maxtree.pmrs", ""],
    ["tree/min.pmrs", ""],
    ["list/polyhom.pmrs", ""],
    ["list/minhom.pmrs", ""],
    ["list/mtshom.pmrs", ""],
    ["tree/mips.pmrs", ""],
]

constraint_benchmarks = [
    # sortedlist
    ["constraints/sortedlist/min.ml", ""],
    ["constraints/sortedlist/max.ml", ""],
    ["constraints/sortedlist/count_lt.ml", ""],
    ["constraints/sortedlist/index_of.ml", ""],
    ["constraints/sortedlist/is_intersection_empty.ml", ""],
    ["constraints/sortedlist/largest_diff.ml", ""],
    ["constraints/sortedlist/smallest_diff.ml", ""],
    # constantlist
    ["constraints/constantlist/index_of.ml", ""],
    ["constraints/constantlist/contains.ml", ""],
    # evenlist
    ["constraints/evenlist/parity_of_first.ml", ""],
    ["constraints/evenlist/parity_of_last.ml", ""],
    ["constraints/evenlist/first_odd.ml", ""],
    ["constraints/evenlist/parity_of_sum.ml", ""],
    # bst
    ["constraints/bst/contains.ml", ""],
    ["constraints/bst/count_lt.ml", "-NB"],
    ["constraints/bst/count_between.ml", "-NB --no-gropt"],
    ["constraints/bst/most_frequent_v1.ml", ""],
    ["constraints/bst/from_list_contains.ml", ""],
    ["constraints/bst/from_list_max.ml", "-NB -n 100"],
    ["constraints/bst/sum_gt_by_key.ml", "-NB -n 100"],
    # balanced_tree
    ["constraints/balanced_tree/node_count.ml", "-N"],
    ["constraints/balanced_tree/height.ml", "-N"],
    ["constraints/balanced_tree/height_v2.ml", "-NB"],
    # symmetric tree
    ["constraints/symmetric_tree/sum.ml", "-N"],
    ["constraints/symmetric_tree/height.ml", "-N"],
    ["constraints/symmetric_tree/min.ml", "-N"],
    # memo
    ["constraints/memo/tree_size.ml", "-NB"],
    ["constraints/memo/constant.ml", ""],
    ["constraints/memo/max_contains.ml", "-NB"],
    ["constraints/memo/count_lt.ml", "-NB -n 50"],
    ["constraints/memo/max_sum_gt.ml", "-NB"],
    ["constraints/memo/proper_indexation_sum_lt_pos_v2.ml", ""],
    ["constraints/memo/proper_indexation_sum_lt_pos.ml", ""],
    # empty_right
    ["constraints/empty_right_subtree/contains.ml", "-N"],
    # alist
    ["constraints/alist/count_eq2.ml", "-NB"],
    ["constraints/alist/count_eq.ml", ""],
    ["constraints/alist/sums.ml", ""],
    ["constraints/alist/most_frequent.ml", ""],
    # even_tree
    ["constraints/even_tree/sum_of_parities.ml", "-NB"],
    ["constraints/even_tree/parity_of_max.ml", ""],
    # program
    ["constraints/program/typecheck.ml", ""]
]

base_benchmark_set = [
    # Combine
    ["combine/mts.pmrs", ""],
    ["combine/mts_and_mps.pmrs", ""],
    # Compressed list
    ["compressed_list/sum.ml", ""],
    # Indexed list
    ["indexed_list/search.ml", ""],
    ["indexed_list/position_polynomial.ml", ""],
    ["indexed_list/sum_elts_lt_pos.ml", ""],
    # Misc
    ["misc/count_between.ml", ""],
    ["misc/composed_unkwns.ml", ""],
    ["misc/simple_nnf.ml", ""],
    ["misc/unknowns_are_ids.ml", ""],
    # List
    ["list/alist_sum.ml", ""],
    ["list/atoi.ml", ""],
    ["list/bal.ml", ""],
    ["list/hamming.pmrs", ""],
    ["list/last.pmrs", ""],
    ["list/lenhom.pmrs", ""],
    ["list/line_of_sight.pmrs", ""],
    ["list/maxcount.pmrs", ""],
    ["list/maxhom.pmrs", ""],
    ["list/mincount.pmrs", ""],
    ["list/minhom.pmrs", ""],
    ["list/mpshom.pmrs", ""],
    ["list/mtshom.pmrs", ""],
    ["list/mts_and_mps_hom.pmrs", ""],
    ["list/msshom.pmrs", ""],
    ["list/prodhom.pmrs", ""],
    ["list/polyhom.pmrs", ""],
    ["list/search.pmrs", ""],
    ["list/sndminhom.pmrs", ""],
    ["list/sumgt.ml", ""],
    ["list/sumhom.pmrs", ""],
    ["list/sumodds.pmrs", ""],
    ["list/sumevens.pmrs", ""],
    ["list/zero_after_one.ml", ""],
    ["list/zeros_ones.ml", ""],
    # List to tree
    ["list/issorted.pmrs", "-t"],
    ["list_to_tree/search.pmrs", ""],
    ["list_to_tree/search_v2.pmrs", ""],
    ["list_to_tree/search_v3.pmrs", ""],
    ["list_to_tree/mls.pmrs", ""],
    # Numbers
    ["numbers/int_nat_toint.ml", ""],
    ["numbers/int_nat_twosum.ml", ""],
    # Ptrees
    ["ptree/maxheads.pmrs", ""],
    ["ptree/maxlast.pmrs", ""],
    ["ptree/maxsum.pmrs", ""],
    ["ptree/mul.pmrs", ""],
    ["ptree/sum.pmrs", ""],
    # Sorting lists
    ["sort_list/min.ml", ""],
    ["sort_list/max.ml", ""],
    ["sort_list/sumgtz.ml", ""],
    # Tail optimization
    ["tailopt/mts.pmrs", ""],
    ["tailopt/mps.pmrs", ""],
    ["tailopt/sum.pmrs", ""],
    # Terms
    ["terms/height.ml", ""],
    # Trees
    ["tree/maxPathWeight.pmrs", ""],
    ["tree/maxtree.pmrs", ""],
    ["tree/maxtree2.pmrs", ""],
    ["tree/min.pmrs", ""],
    ["tree/minmax.pmrs", ""],
    ["tree/mips.pmrs", ""],
    ["tree/mits.pmrs", ""],
    ["tree/mpps.pmrs", "--no-gropt"],
    ["tree/poly.pmrs", ""],
    ["tree/poly2.ml", ""],
    ["tree/sorted.pmrs", "-t"],
    ["tree/sorted_2.ml", ""],
    ["tree/sumtree.pmrs", ""],
    # Tree paths
    ["treepaths/height.pmrs", ""],
    ["treepaths/leftmostodd.pmrs", "-b 6"],
    ["treepaths/maxPathWeight.pmrs", ""],
    ["treepaths/maxPathWeight2.pmrs", ""],
    ["treepaths/mips.pmrs", ""],
    ["treepaths/sum.pmrs", ""],
    # Zippers
    ["zippers/list_sum.ml", ""],
    ["zippers/list_sum_basic.ml", ""],
]

lifting_benchmarks = [
    # Indexed list
    ["indexed_list/position_polynomial_no_index.ml", ""],
    ["indexed_list/search_no_index.ml", ""],
    ["indexed_list/sum_elts_lt_pos_no_len.ml", ""],
    # Automatic parallelization
    ["list/atoi_no_fac.ml", "--no-gropt"],
    ["list/is_sorted_no_last.ml", ""],
    ["list/largest_diff_sorted_list_nohead.ml", ""],
    ["list/mps_no_sum.ml", ""],
    ["list/poly_no_fac.ml", ""],
    ["list/zero_after_one_no.ml", ""],
    # Tail optimizations
    ["tailopt/mps_no_sum.ml", ""],
    # Combining traversals
    ["combine/mts_and_mps_nosum.ml", ""],
    # Switching tree traversals
    ["tree/gradient.ml", ""],
    ["tree/mits_nosum.ml", ""],
    # Unimodal lists (might also be due to representation)
    ["unimodal_lists/prod_needs_aux.ml", ""]
]


benchmark_set = constraint_benchmarks + base_benchmark_set + lifting_benchmarks

# Extra extra benchmarks (takes extra time..)
extra_benchmarks = [
    ["list/bal.ml", ""],
    ["list/lpeak.ml", ""],

]


def summarize():
    num_constraint = len(constraint_benchmarks)
    num_base = len(base_benchmark_set)
    num_lifting = len(lifting_benchmarks)
    num_extrs = len(extra_benchmarks)
    total = num_constraint + num_base + num_lifting + num_extrs
    print("%i benchmarks in total:" % total)
    print("\t- %i basic benchmarks (run with --base)." % num_base)
    print("\t\tincluding %i in kick-the-tires set (run with --kick-the-tires)." %
          len(kick_the_tires_set))
    print("\t- %i benchmarks with requires constraints (run with --constraint-benchmarks)." % num_constraint)
    print("\t- %i benchmarks with lifting (run with --lifting-benchmarks)." % num_lifting)
    print("\t- %i extras benchmarks." % num_extrs)


def run_one(progress, bench_id, command, algo, optim, filename, extra_opt, errors, raw_output):
    if raw_output is not None:
        raw_output.write(f"B:{bench_id}\n")

    print(f"{progress : >11s}  {bench_id} 🏃", end="\r")
    sys.stdout.flush()

    process = subprocess.Popen(
        command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    buf = ""
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


def run_n(progress, bench_id, command, algo,
          optim, filename, extra_opt, errors, num_runs, raw_output):

    total_elapsed = 0
    max_elapsed = 0
    min_elapsed = timeout_value
    running_estimate = 0
    delta = 0
    sp = " "
    for i in range(num_runs):
        errors, elapsed = run_one(progress, bench_id, command, algo,
                                  optim, filename, extra_opt, errors, raw_output)
        if elapsed > 0:
            # Update stats
            total_elapsed += elapsed
            max_elapsed = max(elapsed, max_elapsed)
            min_elapsed = min(elapsed, min_elapsed)
            running_estimate = float(
                running_estimate * i + elapsed) / float(i + 1)

            msg = f"{progress : >11s} ✅ {bench_id: <65s}  [estimate: {running_estimate: 4.3f} s] ({i}/{num_runs} runs){sp : <30s}"
            print(msg, end="\r")
            sys.stdout.flush()
        else:
            print(f"\r{progress : >11s} ❌ {bench_id : <70s}{sp : <10s}", end="r")
            sys.stdout.flush()
            return errors,  elapsed, 0

    elapsed = total_elapsed / num_runs
    delta = 1000 * max(abs(max_elapsed - elapsed), abs(min_elapsed-elapsed))
    return errors,  elapsed, delta


def run_benchmarks(input_files, algos, optims, num_runs=1, raw_output=None, exit_err=False):
    benchmark_cnt = 0
    benchmark_total = len(input_files) * len(algos) * len(optims)
    errors = []
    start = time.time()
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
                command = ("%s %s %s -i %s %s %s %s %s" %
                           (timeout, exec_path, algo[1], optim[1], extra_opt,
                            os.path.realpath(os.path.join(
                                "benchmarks", filename)),
                            soln_file_opt, gen_opt))
                # Run the benchmark n times.
                errors, elapsed, delta = run_n(progress, bench_id, command, algo,
                                               optim, filename, extra_opt, errors, num_runs, raw_output)
                if elapsed > 0:
                    sp = " "
                    msg = f"{progress : >11s} ✅ {bench_id: <66s} (×{num_runs})[{elapsed: 4.3f} s ± {delta : .0f}ms]{sp : <30s}"
                    print(msg)
                else:
                    print(f"\r{progress : >11s} ❌ {bench_id : <120s}")
                    sys.stdout.flush()

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
        summarize()
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

    # Running specific sets if we're supposed to.
    if run_kick_the_tires_only:
        algos = [["partbnd", cvc]]
        optims = [["all", ""]]
        run_benchmarks(kick_the_tires_set, algos,
                       optims, raw_output=raw_output, num_runs=runs)
        exit(0)

    if run_lifting_benchmarks:
        algos = [["partbnd", cvc]]
        optims = [["all", ""]]
        run_benchmarks(lifting_benchmarks, algos,
                       optims, raw_output=raw_output, num_runs=runs)

    if run_constraint_benchmarks:
        algos = [["partbnd", cvc]]
        optims = [["all", ""]]
        run_benchmarks(constraint_benchmarks, algos,
                       optims, raw_output=raw_output, num_runs=runs)

    if run_base_benchmarks:
        algos = [["partbnd", cvc]]
        optims = [["all", ""]]
        run_benchmarks(base_benchmark_set, algos,
                       optims, raw_output=raw_output, num_runs=runs)

    # If we were supposed to run a specific set of benchmarks, we're done
    if run_base_benchmarks or run_constraint_benchmarks or run_lifting_benchmarks:
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
        input_files = kick_the_tires_set

    else:
        if table_no == 2:
            input_files = reduced_benchmark_set_table2
        elif table_no == 3:
            input_files = reduced_benchmark_set_table3
        elif table_no == 4:
            input_files = constraint_benchmarks
        elif table_no == 5:
            input_files = constraint_benchmarks + lifting_benchmarks
        elif run_test_only:
            input_files = benchmark_set
        else:
            input_files = kick_the_tires_set

    run_benchmarks(input_files, algos, optims, raw_output=raw_output,
                   exit_err=True, num_runs=runs)
