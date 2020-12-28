import os
import sys

input_files = [
    "zippers/sum.pmrs",
    "zippers/height.pmrs",
    "zippers/maxPathWeight.pmrs",
    "zippers/maxPathWeight2.pmrs",
    "ptree/sum.pmrs",
    "ptree/mul.pmrs",
    "tree/sumtree.pmrs",
    "tree/maxtree.pmrs",
    "tree/mintree.pmrs",
    "tree/maxtree2.pmrs",
    # "tree/sorted.pmrs", # siwtch off detupling to solve
    "tree/maxPathWeight.pmrs",
    "list/sumhom.pmrs",
    "list/lenhom.pmrs",
    "list/prodhom.pmrs",
    "list/polyhom.pmrs",
    "list/maxhom.pmrs",
    "list/minhom.pmrs",
    # "list/issorted.pmrs", # switch off detupling to solve
    "list/mtshom.pmrs",
    "list/mpshom.pmrs",
    "list/mts_and_mps_hom.pmrs"
]

root = os.getcwd()
path = os.path.join(root, "_build/default/bin/ReFunS.exe")

for filename in input_files:
    os.system("%s %s -i" %
              (path, os.path.realpath(os.path.join("inputs", filename))))
