import os
import sys

input_files = [
    ["zippers/sum.pmrs", ""],
    ["zippers/height.pmrs", ""],
    ["zippers/maxPathWeight.pmrs", ""],
    ["zippers/maxPathWeight2.pmrs", ""],
    ["ptree/sum.pmrs", ""],
    ["ptree/mul.pmrs", ""],
    ["tree/sumtree.pmrs", ""],
    ["tree/maxtree.pmrs", ""],
    ["tree/mintree.pmrs", ""],
    ["tree/maxtree2.pmrs", ""],
    ["tree/maxPathWeight.pmrs", ""],
    ["list/sumhom.pmrs", ""],
    ["list/lenhom.pmrs", ""],
    ["list/prodhom.pmrs", ""],
    ["list/polyhom.pmrs", ""],
    ["list/hamming.pmrs", ""],
    ["list/maxhom.pmrs", ""],
    ["list/minhom.pmrs", ""],
    ["list/mtshom.pmrs", ""],
    ["list/mpshom.pmrs", ""],
    ["list/mts_and_mps_hom.pmrs", ""],
    ["list/issorted.pmrs", "--detupling-off"],
    ["tree/sorted.pmrs", "--detupling-off"],
    ["tree/mips.pmrs", ""],
    ["tree/mpps.pmrs", ""]
]

root = os.getcwd()
path = os.path.join(root, "_build/default/bin/ReFunS.exe")

print("folder,file,# refinements, synthesis time")
sys.stdout.flush()

for filename_with_opt in input_files:
    filename = filename_with_opt[0]
    opt = filename_with_opt[1]
    os.system("%s %s -i %s" %
              (path, os.path.realpath(os.path.join("inputs", filename)), opt))
