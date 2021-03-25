#!/usr/bin/env python3
import os
import sys

timeout_value = 600  # 10min timeout
memout_value = 4000 * (2 ** 10)  # 4GB memory limit
timeout = ("../timeout/timeout -t %i -m %i --no-info-on-success" %
           (timeout_value, memout_value))


input_files = [
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
    ["requation", ""],
    # ["acegis", "--acegis"],
    # ["ccegis", "--ccegis"]
]

optims = [
    ["all", ""],
    # ["ini", "-c"],
    # ["st", "-st"],
    # ["d", "--no-syndef"],
    # ["off", "-st --no-syndef"]
]

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
