#!/usr/bin/env python3
import os
import sys

# Timeout is 10min for all experiments.
timeout_value = 600  # 10min timeout
# Maximum 4gb memory - this should not be limiting!
memout_value = 8000 * (2 ** 10)  # 4GB memory limit
timeout = ("../timeout/timeout -t %i -m %i --no-info-on-success" %
           (timeout_value, memout_value))


reduced_benchmark_set = [
    ["list/sumhom.pmrs", ""],
    ["ptree/sum.pmrs", ""],
    ["tree/sumtree.pmrs", ""],
    ["tailopt/sum.pmrs", ""],
    ["tailopt/mts.pmrs", ""],
    ["tailopt/mps.pmrs", ""],
    ["combine/mts.pmrs", ""],
    ["combine/mts_and_mps.pmrs", ""],
    ["zippers/sum.pmrs", ""],
    ["zippers/height.pmrs", ""],
    ["zippers/mips.pmrs", ""],
    ["zippers/leftmostodd.pmrs", "-b 6"],
    ["zippers/maxPathWeight.pmrs", ""],
    ["ptree/mul.pmrs", ""],
    ["tree/maxtree.pmrs", ""],
    ["tree/min.pmrs", ""],
    ["tree/minmax.pmrs", ""],
    ["tree/maxtree2.pmrs", ""],
    ["tree/poly.pmrs", ""],
    ["list/sumodds.pmrs", ""],
    ["list/prodhom.pmrs", ""],
    ["list/polyhom.pmrs", ""],
    ["list/hamming.pmrs", ""],
    ["list/maxcount.pmrs", ""],
    ["list/minhom.pmrs", ""],
    ["list/mincount.pmrs", ""],
    ["list/last.pmrs", ""],
    ["list/sndminhom.pmrs", ""],
    ["list/mtshom.pmrs", ""],
    ["list/mpshom.pmrs", ""],
    ["list/msshom.pmrs", ""],
    ["list/search.pmrs", ""],
    ["list/line_of_sight.pmrs", ""],
    ["tree/sorted.pmrs", "-t"],
    ["tree/mips.pmrs", ""],
]

benchmark_set = [
    ["list/sumhom.pmrs", ""],
    ["ptree/sum.pmrs", ""],
    ["tree/sumtree.pmrs", ""],
    ["tailopt/sum.pmrs", ""],
    ["tailopt/mts.pmrs", ""],
    ["tailopt/mps.pmrs", ""],
    ["combine/mts.pmrs", ""],
    ["combine/mts_and_mps.pmrs", ""],
    ["zippers/sum.pmrs", ""],
    ["zippers/height.pmrs", ""],
    ["zippers/mips.pmrs", ""],
    ["zippers/leftmostodd.pmrs", "-b 6"],
    ["zippers/maxPathWeight.pmrs", ""],
    ["zippers/maxPathWeight2.pmrs", ""],
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
    ["list/sumodds.pmrs", ""],
    ["list/sumgt.pmrs", ""],
    ["list/sumevens.pmrs", ""],
    ["list/lenhom.pmrs", ""],
    ["list/prodhom.pmrs", ""],
    ["list/polyhom.pmrs", ""],
    ["list/hamming.pmrs", ""],
    ["list/maxhom.pmrs", ""],
    ["list/maxcount.pmrs", ""],
    ["list/minhom.pmrs", ""],
    ["list/mincount.pmrs", ""],
    ["list/last.pmrs", ""],
    ["list/sndminhom.pmrs", ""],
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
    ["list_to_tree/search.pmrs", ""],
    ["list_to_tree/search_v2.pmrs", ""],
    ["list_to_tree/search_v3.pmrs", ""],
    ["list_to_tree/mls.pmrs", ""]
]

root = os.getcwd()
exec_path = os.path.join(root, "_build/default/bin/Atropos.exe")

sys.stdout.flush()

algos = [
    # Grammar optimizations not part of CAV21 and unstable
    ["requation", "--no-gropt"],
    # ["acegis", "--acegis --no-gropt"],
    # ["ccegis", "--ccegis --no-gropt"]
]

woptims = [
    ["all", ""],
    # ["ini", "-c --no-gropt"],
    # ["st", "-st --no-gropt"],
    # ["d", "--no-syndef --no-gropt"],
    # ["off", "-st --no-syndef --no-gropt"]
]

if __name__ == "__main__":
    if len(sys.argv) > 1:
        table_no = int(sys.argv[1])
        if table_no == 1:
            # Table 1 : compare Atropo and Baseline
            algos = [
                    ["requation", "--no-gropt"],
                    ["acegis", "--acegis --no-gropt"]
            ]
            optims = [["all", ""]]
        elif table_no == 2:
            # Table 2 : compare Atropos, Baseline and Concrete CEGIS
            algos = [
                    ["requation", "--no-gropt"],
                    ["acegis", "--acegis --no-gropt"],
                    ["ccegis", "--ccegis --no-gropt"]
            ]
            optims = [["all", ""]]
        elif table_no == 3:
            # Table 2 : compare Atropos, Baseline with optimizations on/off
            algos = [
                ["requation", "--no-gropt"],
                ["acegis", "--acegis --no-gropt"],
            ]

            woptims = [
                ["all", ""],
                ["ini", "-c --no-gropt"],
                ["st", "-st --no-gropt"],
                ["d", "--no-syndef --no-gropt"],
                ["off", "-st --no-syndef --no-gropt"]
            ]
    else:
        print(
            "Usage:python3 test.py TABLE_NO [USE_REDUCED_SET]\n\tRun the experiments to generate data for table TABLE_NO (1, 2, or 3).\n\tIf an additional argument is provided, only a reduced set of benchmarks is run.")
        exit()

    if len(sys.argv) > 2:
        input_files = reduced_benchmark_set
    else:
        intput_files = benchmark_set

    for filename_with_opt in input_files:
        filename = filename_with_opt[0]
        extra_opt = filename_with_opt[1]
        for algo in algos:
            for optim in optims:
                print("B:%s,%s+%s" % (filename, algo[0], optim[0]))
                sys.stdout.flush()
                os.system("%s %s %s -i %s %s %s" %
                          (timeout, exec_path, algo[1], optim[1], extra_opt,
                           os.path.realpath(os.path.join("benchmarks", filename))))
