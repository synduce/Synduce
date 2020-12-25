import os
import sys

input_files = [
    "tree/sumtree.pmrs",
    "tree/maxtree.pmrs",
    "tree/mintree.pmrs",
    "tree/maxtree2.pmrs",
    "list/sumhom.pmrs",
    "list/lenhom.pmrs",
    "list/prodhom.pmrs",
    "list/mtshom.pmrs",
    "list/mpshom.pmrs",
    "list/mts_and_mps_hom.pmrs"
]

root = os.getcwd()
path = os.path.join(root, "_build/default/bin/ReFunS.exe")

for filename in input_files:
    os.system("%s %s -i" %
              (path, os.path.realpath(os.path.join("inputs", filename))))
