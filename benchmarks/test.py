import os
import sys

timeout = 600  # 10min timeout

naive_opt = "--use-naive"

input_files = [
    ["tailopt/sum.pmrs", ""],
    ["tailopt/mts.pmrs", "--verification=5"],
    ["tailopt/mps.pmrs", "--verification=5"],
    ["combine/mts.pmrs", "--verification=5"],
    ["combine/mts_and_mps.pmrs", "--verification=5"],
    ["zippers/sum.pmrs", ""],
    ["zippers/height.pmrs", ""],
    ["zippers/mips.pmrs", ""],
    ["zippers/leftmostodd.pmrs", "-b 4"],
    ["zippers/maxPathWeight.pmrs", ""],
    ["zippers/maxPathWeight2.pmrs", ""],
    ["ptree/sum.pmrs", ""],
    ["ptree/mul.pmrs", ""],
    ["ptree/maxheads.pmrs", ""],
    ["ptree/maxlast.pmrs", ""],
    ["ptree/maxsum.pmrs", ""],
    ["tree/sumtree.pmrs", ""],
    ["tree/maxtree.pmrs", ""],
    ["tree/min.pmrs", ""],
    ["tree/last.pmrs", ""],
    ["tree/minmax.pmrs", ""],
    ["tree/maxtree2.pmrs", ""],
    ["tree/poly.pmrs", ""],
    ["tree/maxPathWeight.pmrs", ""],
    ["list/sumhom.pmrs", ""],
    ["list/lenhom.pmrs", ""],
    ["list/prodhom.pmrs", ""],
    ["list/polyhom.pmrs", ""],
    ["list/hamming.pmrs", ""],
    ["list/maxhom.pmrs", ""],
    ["list/minhom.pmrs", ""],
    ["list/last.pmrs", ""],
    ["list/sndminhom.pmrs", ""],
    ["list/mtshom.pmrs", ""],
    ["list/mpshom.pmrs", ""],
    ["list/msshom.pmrs", ""],
    ["list/search.pmrs", ""],
    ["list/line_of_sight.pmrs", ""],
    ["list/mts_and_mps_hom.pmrs", ""],
    ["list/issorted.pmrs", "--detupling-off"],
    ["tree/sorted.pmrs", "--detupling-off"],
    ["tree/mips.pmrs", ""],
    ["tree/mits.pmrs", ""],
    ["tree/mpps.pmrs", ""]
]

root = os.getcwd()
path = os.path.join(root, "_build/default/bin/ReFunS.exe")

sys.stdout.flush()

for filename_with_opt in input_files:
    filename = filename_with_opt[0]
    opt = filename_with_opt[1]
    print("B:%s,requation" % filename)
    sys.stdout.flush()
    os.system("timeout %i %s %s -i %s" %
              (timeout, path, os.path.realpath(os.path.join("benchmarks", filename)), opt))
    if len(sys.argv) < 2:
        print("B:%s,naive" % filename)
        sys.stdout.flush()
        os.system("timeout %i %s %s -i %s %s" %
                  (timeout, path, os.path.realpath(os.path.join("benchmarks", filename)), opt, naive_opt))
