#!/usr/bin/env python3
import sys
import os
from datetime import datetime
import argparse
import matplotlib.pyplot as plt
import shutil
# Local files
from commons import *
from definitions import *
from table1 import *
from table2 import *
from table3 import *
from table5 import *
from table6 import *


def select_last_known_experimental_data(table_no):
    candidates = []
    for root, dirs, files in os.walk("benchmarks/data/exp"):
        for file in files:
            comps = file.split("_")
            if len(comps) > 1 and file.endswith(".csv"):
                try:
                    date_generated = datetime.strptime(
                        comps[1].strip(), timestamp_definition)
                    file_table_no = comps[2].split(".")[0][-1]
                    if table_no == int(file_table_no):
                        candidates.append((date_generated, file))
                except ValueError as e:
                    pass

    candidates = sorted(candidates, key=lambda x: x[0], reverse=True)
    if len(candidates) > 0:
        return f"benchmarks/data/exp/{candidates[0][1]}"
    else:
        return None


if __name__ == "__main__":
    aparser = argparse.ArgumentParser()
    aparser.add_argument(
        "-i", "--input", help="The input file produced by running test.py", type=str, default=None)
    aparser.add_argument(
        "-o", "--output", help="The output text file for the table.", type=str, default="benchmarks/table.txt")
    aparser.add_argument(
        "-t", "--table", help="Table number that the script must generate.", type=int, choices=[0, 1, 2, 3, 4, 5], default=0)
    aparser.add_argument(
        "-c", "--csv", help="The output csv file for results.", type=str, default="benchmarks/constraints_results.csv")
    aparser.add_argument(
        "-e", "--explain", help="Explain where the benchmarks are stored.", action="store_true")
    aparser.add_argument(
        "-y", "--copy", help="Copy figures to $SYND_LOCAL_COPY/figures and $SYND_LOCAL_COPY/tables.", action="store_true"
    )

    args = aparser.parse_args()

    if args.explain:
        explain()
        exit()

    input_file = args.input
    output_file = args.csv
    table_no = args.table
    tex_out = args.output

    if (table_no is None) or table_no < 0 or table_no > 5:
        print("Please provide a table number between 0 and 5.")
        exit()

    if not input_file:
        input_file = select_last_known_experimental_data(table_no)
        print(f"Input file selected: {input_file}")

    if table_no == 1:
        filenames = make_table_1(input_file, output_file)

    # TABLE 2
    if table_no == 2:
        filenames = make_table_2(input_file, output_file)

        if args.copy:
            LOCAL_COPY = os.getenv('FMSD22_LOCAL_COPY')
            if LOCAL_COPY is not None:
                shutil.copyfile(filenames['cactus'], os.path.join(
                    LOCAL_COPY, "figures/cactus_t2.pdf"))
                shutil.copyfile(filenames['scatter'], os.path.join(
                    LOCAL_COPY, "figures/scatter_t2.pdf"))
                shutil.copyfile(filenames['scatter_no_timeouts'], os.path.join(
                    LOCAL_COPY, "figures/no_timeouts_scatter_t2.pdf"))

    # SET 3
    if table_no == 3:
        filenames = make_table_3(input_file, output_file)
        if args.copy:
            LOCAL_COPY = os.getenv('FMSD22_LOCAL_COPY')
            if LOCAL_COPY is not None:
                shutil.copyfile(filenames['cactus'], os.path.join(
                    LOCAL_COPY, "figures/cactus_t3.pdf"))
                shutil.copyfile(filenames['barchart'], os.path.join(
                    LOCAL_COPY, "figures/barchart_t3.pdf"))
                shutil.copyfile(filenames['verif'], os.path.join(
                    LOCAL_COPY, "figures/verif_t3.pdf"))

    # TABLE 5
    if table_no == 5 or table_no == 0 and input_file:
        filenames = make_table_5(input_file, output_file)

        if args.copy:
            LOCAL_COPY = os.getenv('SYND_LOCAL_COPY')
            if LOCAL_COPY is not None:
                shutil.copyfile(filenames['table'], os.path.join(
                    LOCAL_COPY, "tables/table.tex"))
                shutil.copyfile(filenames['table2'], os.path.join(
                    LOCAL_COPY, "tables/table_unrealizable.tex"))
                shutil.copyfile(filenames['quantile'], os.path.join(
                    LOCAL_COPY, "figures/quantile.pdf"))
                shutil.copyfile(filenames['quantile_unrealizable'], os.path.join(
                    LOCAL_COPY, "figures/quantile_unrealizable.pdf"))
                shutil.copyfile(filenames['scatter'], os.path.join(
                    LOCAL_COPY, "figures/scatter.pdf"))
                shutil.copyfile(filenames['scatter_no_timeouts'], os.path.join(
                    LOCAL_COPY, "figures/no_timeouts_scatter.pdf"))
