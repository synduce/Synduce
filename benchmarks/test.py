#!/usr/bin/env python3
import os
import sys
import argparse

# Timeout for all experiments.
timeout_value = 240  # 4min timeout for the review
# Maximum 4gb memory - this should not be limiting!
memout_value = 8000 * (2 ** 10)  # 4GB memory limit

if sys.platform.startswith('linux'):
    timeout = ("./extras/timeout/timeout -t %i --no-info-on-success" %
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
    ["list/last.pmrs", ""]
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
    ["constraints/bst/count_between.ml", "-NB --cvc4"],
    ["constraints/bst/most_frequent_v1.ml", ""],
    ["constraints/bst/from_list_contains.ml", ""],
    ["constraints/bst/from_list_max.ml", "-NB -n 100"],
    ["constraints/bst/sum_gt_by_key.ml", "-NB -n 100"],
    # balanced_tree
    ["constraints/balanced_tree/node_count.ml", "-N"],
    ["constraints/balanced_tree/height.ml", "-N"],
    ["constraints/balanced_tree/height_v2.ml", "-NB --cvc4"],
    # memo
    ["constraints/memo/tree_size.ml", "-NB"],
    ["constraints/memo/constant.ml", ""],
    ["constraints/memo/max_contains.ml", "-NB"],
    ["constraints/memo/count_lt.ml", "-NB -n 50"],
    ["constraints/memo/max_sum_gt.ml", "-NB"],
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
    ["misc/simple_nnf.ml", ""],
    ["misc/unknowns_are_ids.ml", ""],
    ["misc/composed_unkwns.ml", ""],
    ["ptree/sum.pmrs", ""],
    ["tree/sumtree.pmrs", ""],
    ["tailopt/sum.pmrs", ""],
    ["tailopt/mts.pmrs", ""],
    ["tailopt/mps.pmrs", ""],
    ["combine/mts.pmrs", ""],
    ["combine/mts_and_mps.pmrs", ""],
    ["treepaths/sum.pmrs", ""],
    ["treepaths/height.pmrs", ""],
    ["treepaths/mips.pmrs", ""],
    ["treepaths/leftmostodd.pmrs", "-b 6"],
    ["treepaths/maxPathWeight.pmrs", ""],
    ["treepaths/maxPathWeight2.pmrs", ""],
    ["ptree/mul.pmrs", ""],
    ["ptree/maxheads.pmrs", ""],
    ["ptree/maxlast.pmrs", ""],
    ["ptree/maxsum.pmrs", ""],
    ["tree/maxtree.pmrs", ""],
    ["tree/min.pmrs", ""],
    ["tree/minmax.pmrs", ""],
    ["tree/maxtree2.pmrs", ""],
    ["tree/poly.pmrs", "--cvc4"],
    ["tree/maxPathWeight.pmrs", ""],
    ["list/sumhom.pmrs", ""],
    ["list/sumevens.pmrs", ""],
    ["list/zero_after_one.ml", ""],
    ["list/lenhom.pmrs", ""],
    ["list/prodhom.pmrs", ""],
    ["list/polyhom.pmrs", ""],
    ["list/atoi.ml", ""],
    ["list/hamming.pmrs", ""],
    ["list/maxcount.pmrs", "--cvc4"],
    ["list/minhom.pmrs", ""],
    ["list/last.pmrs", ""],
    ["list/mtshom.pmrs", ""],
    ["list/mpshom.pmrs", ""],
    ["list/msshom.pmrs", ""],
    ["list/search.pmrs", ""],
    ["list/line_of_sight.pmrs", ""],
    ["list/mts_and_mps_hom.pmrs", ""],
    ["list/issorted.pmrs", "-t"],
    ["tree/sorted.pmrs", "-t"],
    ["tree/mips.pmrs", ""],
    ["tree/mits.pmrs", ""],
    ["tree/mpps.pmrs", "--cvc4"],
    # ----------- Extra benchmarks ---------------
    ["list_to_tree/search.pmrs", ""],
    ["list_to_tree/search_v2.pmrs", ""],
    ["list_to_tree/search_v3.pmrs", ""],
    ["list_to_tree/mls.pmrs", ""],
    ["misc/count_between.ml", ""],
    ["list/alist_sum.ml", ""],
    ["list/maxhom.pmrs", ""],
    ["list/sumodds.pmrs", ""],
    ["list/sumgt.ml", ""],
    ["list/sndminhom.pmrs", ""],
    ["list/mincount.pmrs", ""],
    ["list/zeros_ones.ml", ""],
    ["zippers/list_sum_basic.ml", ""],
    ["zippers/list_sum.ml", ""],
    ["sort_list/min.ml", ""],
    ["sort_list/max.ml", ""],
    ["sort_list/sumgtz.ml", ""],
    ["numbers/int_nat_twosum.ml", ""],
    ["numbers/int_nat_toint.ml", ""],
    ["terms/height.ml", ""]
]

lifting_benchmarks = [
    ["lifting/mits_nosum.ml", ""],
    ["lifting/mpsl.ml", ""],
    ["lifting/poly.ml", ""],
]


benchmark_set = constraint_benchmarks + base_benchmark_set + lifting_benchmarks

# Extra extra benchmarks (takes extra time..)
extra_benchmarks = [
    ["list/bal.ml", ""],
    ["list/lpeak.ml", ""],
    
]

root = os.getcwd()
exec_path = os.path.join(root, "_build/default/bin/Synduce.exe")

sys.stdout.flush()


if __name__ == "__main__":
    aparser = argparse.ArgumentParser()
    aparser.add_argument(
        "-t", "--table", help="Table number that the script must generate data for.", type=int, default=-1)
    aparser.add_argument(
        "--run", help="Run tests for all benchmarks.", action="store_true")
    aparser.add_argument(
        "--generate-benchmarks", help="Generate SyGuS benchmarks.", action="store_true")
    aparser.add_argument(
        "--generate-solutions", help="Generate solutions in extras/solution.", action="store_true")
    aparser.add_argument(
        "--kick-the-tires", help="Run a subset of benchmarks.", action="store_true")
    args = aparser.parse_args()

    table_no = args.table
    generate_solutions = args.generate_solutions
    generate_benchmarks = args.generate_benchmarks

    run_test_only = table_no == -1 or args.run

    if run_test_only:

        algos = [["requation", ""]]
        optims = [["all", ""]]

    # Table 1 / CAV 21 paper : compare Synduce and Baseline
    elif table_no == 1:

        algos = [
            ["requation", "--no-gropt"],
            ["acegis", "--acegis --no-gropt"]
        ]
        optims = [["all", ""]]

    # Table 2 / CAV 21 paper : compare Synduce, Baseline and Concrete CEGIS
    elif table_no == 2:

        algos = [
            ["requation", "--no-gropt"],
            ["acegis", "--acegis --no-gropt"],
            ["ccegis", "--ccegis --no-gropt"]
        ]
        optims = [["all", ""]]

    # Table 3 / CAV21 paper : compare Synduce, Baseline with optimizations on/off
    elif table_no == 3:

        algos = [
            ["requation", "--no-gropt"],
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
        algos = [["requation"]]
        optims = [["all", ""]]

    # Table 5 / Test with baseline comparison
    elif table_no == 5:
        algos = [["requation", "--cvc4"], ["acegis", "--acegis --cvc4"]]
        optims = [["all", ""]]

    # No table - just run the base algorithm.
    else:
        algos = [["requation", ""]]
        optims = [["all", ""]]

    # Benchmark set selection.

    if args.kick_the_tires:
        input_files = kick_the_tires_set
    else:
        if table_no == 2:
            input_files = reduced_benchmark_set_table2
        elif table_no == 3:
            input_files = reduced_benchmark_set_table3
        elif table_no == 4 or table_no == 5:
            input_files = constraint_benchmarks
        elif run_test_only:
            input_files = benchmark_set
        else:
            input_files = kick_the_tires_set

    for filename_with_opt in input_files:
        filename = filename_with_opt[0]
        category = os.path.dirname(filename)
        extra_opt = filename_with_opt[1]
        for algo in algos:
            for optim in optims:
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
                print("B:%s,%s+%s" % (filename, algo[0], optim[0]))
                sys.stdout.flush()

                os.system("%s %s %s -i %s %s %s %s %s" %
                          (timeout, exec_path, algo[1], optim[1], extra_opt,
                           os.path.realpath(os.path.join(
                               "benchmarks", filename)),
                           soln_file_opt, gen_opt))
