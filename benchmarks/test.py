#!/usr/bin/env python3
import os
import sys

# Timeout for all experiments.
timeout_value = 240  # 4min timeout for the review
# Maximum 4gb memory - this should not be limiting!
memout_value = 8000 * (2 ** 10)  # 4GB memory limit


timeout = ("./extras/timeout/timeout -t %i -m %i --no-info-on-success" %
           (timeout_value, memout_value))

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
    ["constraints/bst/count_lt.ml", "--no-sat-as-unsat"],
    ["constraints/bst/most_frequent_v1.ml", ""],
    # balanced_tree
    ["constraints/balanced_tree/node_count.ml", "-B --no-sat-as-unsat"],
    ["constraints/balanced_tree/height.ml", "-B --no-sat-as-unsat"],
    # memo
    ["constraints/memo/tree_size.ml", "-B --no-sat-as-unsat"],
    ["constraints/memo/constant.ml", ""],
    ["constraints/memo/max_contains.ml", "-B"],
    ["constraints/memo/count_lt.ml", "-B --no-sat-as-unsat -n 50"],
    ["constraints/memo/max_sum_gt.ml", "-B --no-sat-as-unsat"],
    # empty_right
    ["constraints/empty_right_subtree/contains.ml", "-B --no-sat-as-unsat"],
    # alist
    ["constraints/alist/count_eq2.ml", "-B --no-sat-as-unsat"],
    ["constraints/alist/count_eq.ml", ""],
    ["constraints/alist/sums.ml", ""],
    ["constraints/alist/most_frequent.ml", ""],
    # even_tree
    ["constraints/even_tree/sum_of_parities.ml", "-B --no-sat-as-unsat"],
    ["constraints/even_tree/parity_of_max.ml", ""]
]

benchmark_set = [
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
    ["tree/poly.pmrs", ""],
    ["tree/maxPathWeight.pmrs", ""],
    ["list/sumhom.pmrs", ""],
    ["list/sumevens.pmrs", ""],
    ["list/lenhom.pmrs", ""],
    ["list/prodhom.pmrs", ""],
    ["list/polyhom.pmrs", ""],
    ["list/hamming.pmrs", ""],
    ["list/maxcount.pmrs", ""],
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
    ["tree/mpps.pmrs", ""],
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
    ["zippers/list_sum_basic.ml", ""],
    ["zippers/list_sum.ml", ""],
    ["lifting/mits_nosum.ml", ""],
    ["lifting/mpsl.ml", ""],

] + constraint_benchmarks

extra_benchmarks = [
    ["list/bal.ml", ""],
    ["list/lpeak.ml", ""],
]

root = os.getcwd()
exec_path = os.path.join(root, "_build/default/bin/Synduce.exe")

sys.stdout.flush()


if __name__ == "__main__":
    if len(sys.argv) > 1:
        table_no = int(sys.argv[1])
        if table_no == -1:
            algos = [["requation", ""]]
            optims = [["all", ""]]

        elif table_no == 1:
            # Table 1 : compare Synduce and Baseline
            algos = [
                    ["requation", "--no-gropt"],
                    ["acegis", "--acegis --no-gropt"]
            ]
            optims = [["all", ""]]
        elif table_no == 2:
            # Table 2 : compare Synduce, Baseline and Concrete CEGIS
            algos = [
                    ["requation", "--no-gropt"],
                    ["acegis", "--acegis --no-gropt"],
                    ["ccegis", "--ccegis --no-gropt"]
            ]
            optims = [["all", ""]]

        elif table_no == 3:
            # Table 2 : compare Synduce, Baseline with optimizations on/off
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
        elif table_no == 4:
            algos = [["requation", ""]]
            optims = [["all", ""]]
    else:
        print(
            "Usage:python3 test.py TABLE_NO [USE_REDUCED_SET]\n\
            \tRun the experiments to generate data for table TABLE_NO (1, 2, or 3) or -1 for running tests.\n\
            \tIf an additional argument is provided, only a reduced set of benchmarks is run.\n\
            \t\tUSE_REDUCED_SET=0 kick-the-tire benchmarks set\n\
            \t\tUSE_REDUCE_SET != 0 reduced set of benchmarks\n")
        exit()

    if len(sys.argv) > 2:
        if int(sys.argv[2]) == 0:
            input_files = kick_the_tires_set
        else:
            if table_no == 2:
                input_files = reduced_benchmark_set_table2
            elif table_no == 3:
                input_files = reduced_benchmark_set_table3
            elif table_no == 4:
                input_files = constraint_benchmarks
            else:
                input_files = kick_the_tires_set

    else:
        input_files = benchmark_set

    for filename_with_opt in input_files:
        filename = filename_with_opt[0]
        category = os.path.dirname(filename)
        extra_opt = filename_with_opt[1]
        for algo in algos:
            for optim in optims:
                print("B:%s,%s+%s" % (filename, algo[0], optim[0]))
                sys.stdout.flush()
                if table_no > 0:
                    os.system("%s %s %s -i %s %s %s" %
                              (timeout, exec_path, algo[1], optim[1], extra_opt,
                               os.path.realpath(os.path.join("benchmarks", filename))))
                else:
                    if not os.path.exists(os.path.dirname("extras/solutions/%s" % filename)):
                        os.makedirs(os.path.dirname(
                            "extras/solutions/%s" % filename))
                    os.system("%s %s %s -i %s %s %s -o %s" %
                              (timeout, exec_path, algo[1], optim[1], extra_opt,
                               os.path.realpath(os.path.join(
                                   "benchmarks", filename)),
                               "extras/solutions/%s/" % category
                               ))
