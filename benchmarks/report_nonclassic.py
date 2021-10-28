#!/usr/bin/env python3
import sys
import datetime
import argparse

timeout_time = 600.0


algorithms = ["partbnd", "acegis", "ccegis"]
versions = ["all", "ini", "st", "d", "off"]
fields = ["synt", "verif", "#i", "last"]

show_benchmarks = [
    # All positive
    ["constraint/all_positive", [
        ["list_mps",     [" Elements > 0    ",
                          " mps   ", "no", "no",  "mps", 0]]
    ]],
    # Alist
    ["constraint/alist", [
        ["count_eq2",     ["              ",
                           " # elt. = (v2) ", "yes", "yes",  "# =", 0]],
        ["count_eq",      ["  Association ",
                           " # elt. =      ", "no", "no", " # = (v2)", 0]],
        ["sums",          ["   List       ",  "sums", "no", "no", "sums ", 0]],
        ["most_frequent", ["              ",
                           "most frequent", "no", "no",  "most freq.", 0]],
    ]],
    # Balanced tree
    ["constraint/balanced_tree", [
        ["node_count",  [" Balanced ", "node count", "yes", "no",  "node cnt.", 0]],
        ["height",      ["  Tree    ", "height", "yes", "no", "height", 0]],
        ["height_v2",   ["          ",
                         "height (v2)  ", "yes", "yes", "height v2.", 0]],
    ]],
    # BST
    ["constraint/bst", [
        ["contains",           ["          ",
                                "contains elt. ", "no", "no", "contains", 0]],
        ["count_lt",           ["  Binary  ",
                                "# elts. < ", "yes", "yes", "count <", 0]],
        ["most_frequent_v1",   ["Search",
                                " most frequent ", "no", "no", "most freq. ", 0]],
        ["from_list_contains", ["  Tree    ",
                                "of list ∋", "no", "no", "contains (list)", 0]],
        ["from_list_max",      ["          ",
                                "of list max ", "yes", "yes", "max (list)", 0]],
        ["sum_gt_by_key",      ["          ",
                                "sum if key >   ", "yes", "yes", "sumkey", 0]],
    ]],
    # Combining traversals
    ["combine", [
        ["mts_and_mps_nosum", ["Combine", "mts+mps", "no", "no", "m(t+p)s", 1]]]],
    # Constant list
    ["constraint/constantlist", [
        ["index_of",  [" Constant ", "index of elt. ", "no", "no", "idx", 0]],
        ["contains",  ["  List    ", "contains elt  ", "no", "no", "contains", 0]],
    ]],
    # Empty right subtree
    ["constraint/empty_right_subtree", [
        ["contains",  [" Empty Subtree", " contains elt", "yes", "no", "contains", 0]],
    ]],
    # Even list
    ["constraint/evenlist",
     [
         ["parity_of_first", ["           ",
                              "parity of 1st", "no", "no", "parity 1st", 0]],
         ["parity_of_last",  ["  List of  ",
                              "parity of last", "no", "no", "parity last", 0]],
         ["first_odd",       ["   even    ",
                              "first odd elt.", "no", "no", "first odd", 0]],
         ["parity_of_sum",   ["  numbers  ",
                              "parity of sum ", "no", "no", "parity sum", 0]]
     ]],
    # Even tree
    ["constraint/even_tree",
     [
         ["parity_of_max", ["  Tree of Even ",
                            "parity of max ", "no", "no", "parity max", 0]],
         ["sum_of_parities", ["  Numbers  ",
                              "parity of sum", "yes", "yes", "parity sum", 0]]
     ]],
    # Indexed list
    ["indexed_list", [
        ["position_polynomial_no_index",    [" Indexed  ",
                                             "value-pos mult.",   "no", "no", "val*pos", 1]],
        ["search_no_index",                 ["    List  ",
                                             "search index",      "no", "no", "search i", 1]],
        ["sum_elts_lt_pos_no_len",          ["          ",
                                             "sum elts w. pos <", "no", "no", "sum pos", 1]]
    ]],
    ["list", [
        ["atoi_no_fac",                     [
            "    ", "atoi", "no", "no", "atoi", 1]],
        ["is_sorted_no_last",               [
            " List ", "is sorted", "no", "no", "sorted", 1]],
        ["largest_diff_sorted_list_nohead", [
            " Parallelization ", "largest diff", "no", "no", "l. diff", 1]],
        ["mps_no_sum",                      [
            "      ", "mps", "no", "no", "mps", 1]],
        ["poly_no_fac",                     [
            "      ", "poly", "no", "no", "poly", 1]],
        ["zero_after_one_no",               ["      ", "0 after 1", "no", "no", "0-1", 1]]]],
    # Memo
    ["constraint/memo", [
        ["tree_size",     ["             ", "tree size", "yes", "yes",  "size", 0]],
        ["constant",      ["  Tree       ", "constant", "no", "no", " constant", 0]],
        ["max_contains",  [" Memoizing   ",
                           " contains elt. ", "yes", "yes", "contains", 0]],
        ["count_lt",      [" Information ", "count <", "yes", "yes",  "cnt <", 0]],
        ["max_sum_gt",    ["             ", "sum of elts >", "yes", "yes",  "sum >", 0]],
        ["proper_indexation_sum_lt_pos_v2",
         ["             ", "sum elts > pos", "no", "no", "sum > pos", 0]],
        ["proper_indexation_sum_lt_pos",
         ["             ", "sum elts > pos .v2", "no", "no", "sum > pos v2", 0]]

    ]],
    # Program
    ["constraint/program", [
        ["typecheck",  [" Program AST ", " type check  ", "yes", "no", "type chk", 0]],
    ]],
    # Symmetric tree
    ["constraint/symmetric_tree", [
        ["sum",         [" Symmetric  ", "sum", "yes", "no", "sum", 0]],
        ["height",      ["    Tree    ", "height", "yes", "no", "height", 0]],
        ["min",         ["            ", "min", "yes", "no", "min", 0]]
    ]],
    # Sorted
    ["constraint/sortedlist", [
        ["min",                    ["        ", "min ", "no", "no", "min", 0]],
        ["max",                    [" Sorted ", "max ", "no", "no", "max", 0]],
        ["count_lt",               [" List   ",
                                    "count elt. < ", "no", "no", "cnt <", 0]],
        ["index_of",               ["        ",
                                    "index of elt ", "no", "no", "idx", 0]],
        ["is_intersection_empty",  ["        ", "∩-empty ", "no", "no", "∩-∅", 0]],
        ["largest_diff",           ["        ",
                                    "largest diff ", "no", "no", "ldiff", 0]],
        ["smallest_diff",          ["        ",
                                    "smallest diff ", "no", "no", "sdiff", 0]],
        ["parallel_min",          ["        ",
                                   "parallel min", "yes", "yes", "||min", 0]],
        ["parallel_max",          ["        ",
                                   "parallel max", "yes", "yes", "||max", 0]],
    ]],
    # Sorted and indexed
    ["constraint/sorted_and_indexed", [
        ["count_lt0",                    ["        ",
                                          "count < 0", "yes", "yes", "# < 0", 0]],
        ["count_lt",                    ["        ",
                                         "count < x", "yes", "yes", "# < 0", 0]],
    ]],
    # Tail optimizations
    ["tailopt", [
        ["mps_no_sum",  [" Tail opt. ", " mps  ", "no", "no", "mps", 1]],
    ]],
    # Switching tree traversals
    ["tree", [
        ["gradient",   [" Tree      ", "gradient", "no", "no", "grad", 1]],
        ["mits_nosum", [" Traversal ", "mits", "no", "no", "mits", 1]]]]
]

lift_info = {
    "indexed_list/position_polynomial_no_index.ml": "1",
    "indexed_list/search_no_index.ml": "1",
    "indexed_list/sum_elts_lt_pos_no_len.ml": "1",
    "list/atoi_no_fac.ml": "1",
    "list/is_sorted_no_last.ml": "1",
    "list/largest_diff_sorted_list_nohead.ml": "1",
    "list/mps_no_sum.ml": "1",
    "list/poly_no_fac.ml": "1",
    "list/zero_after_one_no.ml": "1",
    "tailopt/mps_no_sum.ml": "1",
    "combine/mts_and_mps_nosum.ml": "1",
    "tree/gradient.ml": "1",
    "tree/mits_nosum.ml": "1",
}

sp = " "
dash = "-"
kw_class = "... Class"
kw_benchmark = "... Benchmark"
kw_time = "time"
kw_steps = "#st"
kw_tlast = "Tlast"
kw_ver = "ver.%"
kw_toolname = "Synduce"
kw_baseline = "Baseline"
kw_acegis = "Symbolic CEGIS"
kw_ccegis = "Concrete CEGIS"
kw_path = "Path"


def explain():
    print("Benchmarks are stored in the benchmarks/ folder.")
    print(f"{kw_class: <20s} | {kw_benchmark : <25s} | L | {kw_path : <30s}")
    for cat_folder, cat_benchmarks in show_benchmarks:
        print(f"{dash:-<80s}-")
        for bench_file, attributes in cat_benchmarks:
            bench_path = "benchmarks/" + cat_folder + "/" + bench_file + ".pmrs"
            print(
                f" {attributes[0]: <19s} | {attributes[1]: <25s} | {attributes[5]} | {bench_path : <30s}")


def all_timeout(l):
    is_t = True
    for elt in l:
        is_t = is_t and (elt == ["TIMEOUT"])
    return is_t


def floti(f):
    ret = "-"
    try:
        ret = float(f)
    except:
        ret = timeout_time
    return ret


def speedup(a, b):
    if b == "?" or a == "?":
        return "?"
    if b == "-":
        if a != "-":
            return "∞"
        else:
            return "!"
    elif a == "-":
        return "-∞"
    else:
        a = floti(a)
        b = floti(b)
        if b > 0:
            return "%3.1f" % (b / a)
        else:
            return -1


def median(sample):
    n = len(sample)
    index = n // 2
    # Sample with an odd number of observations
    if n % 2:
        return sorted(sample)[index]
    # Sample with an even number of observations
    return sum(sorted(sample)[index - 1:index + 1]) / 2


def mean(sample):
    n = len(sample)
    s = sum(sample)
    return s / n


def bold(s):
    return s
    # return "{\\bf %s}" % s


def format_verif(f):
    if f == "?":
        return "?"
    try:
        f = float(f)
        if f < 0.009:
            return "0."
        else:
            return "%3.1f" % f
    except:
        return "-"


def incr_avg(old_val, new_count, new_val):
    return (old_val + (new_val - old_val) / new_count)


# TABLE 1

caption1 = "Experimental Results. Benchmarks are grouped by categories introduced in Section 6.1.\n #st indicates the number of refinement rounds. \n  Tlast is the elapsed time before the last call to the SyGuS solver in the last refinement\n  step before timeout.\nAll times are in seconds. A '-' indicates timeout (4 min) or memory out (> 4Gb).\nThe N column indicates whether bounded checking was required to classify counterexamples, \n and the B column bounded checking for proving lemmas correct."


def produce_txt_table(tex_output_file, data):
    with open(tex_output_file, 'w') as outp:

        # open table
        outp.write(
            f"{kw_class:.<15s}| {kw_benchmark:.<15s}| N  | B  |  {kw_toolname: <20s}|  {kw_baseline: <20s}\n")
        outp.write(
            f"{sp:-<88s}\n")
        outp.write(
            f"{sp: <15s}| {sp: <15s}| N  | B  | {kw_time: <7s}| {kw_steps: <4s}| {kw_tlast: <6s}| {kw_time: <7s}| {kw_steps: <4s}| {kw_tlast: <6s}\n")

        speedups = []
        for benchmark_class, benchmarks in show_benchmarks:
            if len(benchmarks) > 0:
                outp.write(f"{sp:-<88s}\n")

            for benchmark_file, benchmark_info in benchmarks:
                req_t = "?"
                req_iters = "?"
                req_last = "?"
                nai_t = "?"
                nai_iters = "?"
                nai_last = "?"

                bkey = benchmark_class + "/" + benchmark_file + ".ml"
                acegis_bf = False

                if (bkey, "partbnd") not in data.keys():
                    # print("No data for %s, partbnd" % bkey)
                    pass
                else:
                    b_data = data[bkey, "partbnd"]["all"]
                    if "res" in b_data:
                        req_iters, _, req_time, _ = b_data["res"]
                        req_t = "%3.2f" % float(req_time)
                    else:
                        req_t = "-"
                        req_iters = b_data["max"]
                    if str(b_data["max"]) in b_data:
                        req_last = "%3.2f" % b_data[str(b_data["max"])][1]

                best_t = timeout_time

                if (bkey, "acegis") not in data.keys():
                    # print("No data for %s, acegis" % bkey)
                    pass
                else:
                    b_data = data[bkey, "acegis"]["all"]
                    if "res" in b_data:
                        nai_iters, _, nai_time, _ = b_data["res"]
                        nai_t = "%3.2f" % float(nai_time)
                        if req_t == "-" or float(nai_time) < float(req_t):
                            acegis_bf = True
                    else:
                        nai_t = "-"
                        nai_iters = b_data["max"]
                    if str(b_data["max"]) in b_data:
                        nai_last = "%3.2f" % b_data[str(b_data["max"])][1]

                speedups += [[(benchmark_class, benchmark_info[1]),
                              speedup(req_t, nai_t)]]

                outp.write(
                    f"{benchmark_info[0]:.<15s}| {benchmark_info[1]:.<15s}| {benchmark_info[2]: <3s}| {benchmark_info[3]: <3s}| {str(req_t): <7s}| {str(req_iters): <4s}| {req_last: <6s}| {nai_t: <7s}| {str(nai_iters): <4s}|{nai_last: >7s}\n")
        outp.write(f"{sp:-<88s}\n")
        outp.write("TABLE X: \n")
        outp.write(caption1)
        outp.write("\n")
        outp.close()
        print("============== SUMMARY ================")
        print("Summary of relative improvement of Synduce over baseline.")
        print("improvement = baseline synt. time / Synduce synt. time")
        print("∞ means baseline timed out, but Synduce did not")
        print("-∞ means Synduce timed out, but baseline did not,")
        print("! means both timed out.")
        print("---------------------------------------------")
        print("%10s,%20s : %s" % ("Category", "benchmark name", "improvement"))
        count_timeouts = 0
        count_100x = 0
        count_10x = 0
        speedup_only = []
        count_data_points = 0
        for b, s in speedups:
            if s != "?":
                print("%20s,%20s : %s" % (b[0], b[1], s))
                count_data_points += 1
            try:
                s = float(s)
                if s < 0:
                    count_timeouts += 1
                else:
                    speedup_only += [s]
                if s > 50.1:
                    count_100x += 1
                if s > 5.1:
                    count_10x += 1
            except:
                if s != "?":
                    count_timeouts += 1

        print("---------------------------------------------")
        print("Data for %i benchmarks." % count_data_points)
        print("Num timeouts (baseline or Synduce): %i" % count_timeouts)
        print("Not counting when baseline times out, count improvements of")
        print("> 100x : %i" % count_100x)
        print("> 10x  : %i" % count_10x)
        print("Average speedup: %3.4f" % mean(speedup_only))
        print("Median  speedup: %3.4f" % median(speedup_only))
        print("=============================================")


# TABLE 2 =====================================================================
caption2 = "Extended Experimental Results. Three algorithms are compared: the selective bounding\nCEGIS in Synduce, symbolic CEGIS and concrete CEGIS. Benchmarks are grouped by categories\nintroduced in Section 6.1 of the paper.\n#st indicates the number of refinement rounds.\nver.% indicates the percentage of total time spent verifying solutions.\nT_last is the elapsed time before the last call to the SyGuS solver in the last refinement\nstep before timeout (`.` indicates that there was no previous round).\nAll times are in seconds. A '-' indicates timeout (10 min).\n"


def table2(tex_output_file, data):
    txt_out = open(tex_output_file, 'w')
    txt_out.write(
        f"{kw_benchmark: <18s}| {kw_toolname: <21s}|| {kw_acegis: <21s}|| {kw_ccegis: <21s}\n")
    txt_out.write(
        f"{sp : <18s}| {kw_time: <7s}| {kw_ver: <6s}| {kw_steps: <4s}|| {kw_time: <7s}| {kw_ver: <6s}| {kw_steps: <4s}|| {kw_time: <7s}| {kw_ver: <6s}| {kw_steps: <4s}\n")

    ver = "all"
    for benchmark_class, benchmarks in show_benchmarks:
        if len(benchmarks) > 0:
            txt_out.write(f"{sp:-<88s}\n")
        for benchmark_file, benchmark_info in benchmarks:
            bkey = benchmark_class + "/" + benchmark_file + ".pmrs"
            csvline = []
            for algo in algorithms:
                if (bkey, algo) in data.keys():
                    bdata = data[bkey, algo]
                    times = ["?", "?", "?", "?"]
                    if ver in bdata.keys():
                        times = times_of_bdata(bdata[ver])
                    csvline += times
                else:
                    times = ["?", "?", "?", "?"]
                    csvline += times
            # Make fastest bold
            t_Synduce = floti(csvline[0])
            t_acegis = floti(csvline[4])
            t_ccegis = floti(csvline[8])
            if t_Synduce <= t_acegis and t_Synduce <= t_ccegis:
                csvline[0] = "%s" % csvline[0]
            if t_acegis < t_Synduce and t_acegis < t_ccegis:
                csvline[4] = "%s" % csvline[4]
            if t_ccegis < t_Synduce and t_ccegis < t_acegis:
                csvline[8] = "%s" % csvline[8]
            # Put percentage in italic
            csvline[1] = format_verif(csvline[1])
            csvline[5] = format_verif(csvline[5])
            csvline[9] = format_verif(csvline[9])
            # Remove T_last
            del csvline[3]
            del csvline[6]
            del csvline[9]

            txt_out.write(
                f"{benchmark_info[1]:.<18s}| {csvline[0]: <7s}| {csvline[1]: <6s}| {csvline[2]: <4s}|| {csvline[3]: <7s}| {csvline[4]: <6s}| {csvline[5]: <4s}|| {csvline[6]: <7s}| {csvline[7]: <6s}| {csvline[8]: <4s}\n")

    txt_out.write(f"{sp:-<88s}\n")
    txt_out.write("TABLE 2:\n")
    txt_out.write(caption2)
    txt_out.close()


caption3 = "Optimization Evaluation Results:\nWe evaluate Synduce and symbolic CEGIS with some optimizations turned off:\n  -ini indicates that optimized initialization is off,\n  -sys indicates tuple components and independent subsystems is turned off, and\n  -stx that syntactic definitions are turned off.\n  on (and off) indicate that all optimizations are turned on (resp. off).\nBenchmarks are grouped by categories introduced in Section 6.1 of the paper.\n\# indicates the number of refinement rounds for the given algorithm, and\n\#i indicate the number of refinement rounds without the optimized initialization.\nAll times are in seconds. A '-' indicates timeout (10 min).\n"


def table3(tex_output_file, data):
    txt_out = open(tex_output_file, 'w')
    txt_out.write(
        f"{kw_benchmark: <18s}|  {kw_toolname: <44s} ||  {kw_acegis: <21s}\n")
    kw_on = "on"
    kw_ini = "-ini"
    kw_sys = "-sys"
    kw_stx = "-stx"
    kw_off = "off"
    kw_step = "#"
    kw_stepi = "#i"
    txt_out.write(f"{sp: <18s}| {kw_step: <2s}| {kw_on: <6s}| {kw_stepi: <2s}| {kw_ini: <6s}| {kw_sys : <6s}| {kw_stx: <6s}| {kw_off: <6s}|| {kw_step: <2s}| {kw_on: <6s}| {kw_sys : <6s}| {kw_stx: <6s}| {kw_off: <7s}\n")
    ver = "all"
    for benchmark_class, benchmarks in show_benchmarks:
        if len(benchmarks) > 0:
            txt_out.write(f"{sp:-<103s}\n")

        for benchmark_file, benchmark_info in benchmarks:
            bkey = benchmark_class + "/" + benchmark_file + ".pmrs"
            # Collect Data for partbnd
            algo = "partbnd"
            csvline = []
            if (bkey, algo) in data.keys():
                bdata = data[bkey, algo]
                for version in versions:
                    times = ["?", "?", "?", "?"]
                    if version in bdata.keys():
                        times = times_of_bdata(bdata[version])
                    csvline += times
            else:
                for version in versions:
                    times = ["?", "?", "?", "?"]
                    csvline += times

            req_csvline = [csvline[2], csvline[0], csvline[6], csvline[4],
                           csvline[8], csvline[12], csvline[16]]
            amin = floti(req_csvline[1])
            indexmin = 1
            for i in range(1, len(req_csvline)):
                if floti(req_csvline[i]) < amin and i != 2:
                    indexmin = i
            req_csvline[indexmin] = bold(req_csvline[indexmin])
            # Collect data for ACEGIS
            algo = "acegis"
            csvline = []
            if (bkey, algo) in data.keys():
                bdata = data[bkey, algo]
                for version in versions:
                    times = ["?", "?", "?", "?"]
                    if version in bdata.keys():
                        times = times_of_bdata(bdata[version])
                    csvline += times
            else:
                for version in versions:
                    times = ["?", "?", "?", "?"]
                    csvline += times

            cegis_csvline = [csvline[2], csvline[0],
                             csvline[8], csvline[12], csvline[16]]
            amin = floti(cegis_csvline[1])
            indexmin = 1
            for i in range(1, len(cegis_csvline)):
                if floti(cegis_csvline[i]) < amin:
                    indexmin = i
            # cegis_csvline[indexmin] = bold(cegis_csvline[indexmin])

            # txt_out.write("%s & %s & %s \\\\ \n" % (
            #    benchmark_info[1], "& ".join(req_csvline), "& ".join(cegis_csvline)))
            txt_out.write(f"{benchmark_info[1]:.<18s}|")
            txt_out.write(
                f" {req_csvline[0]: <2s}| {req_csvline[1]: <6s}| {req_csvline[2]: <2s}| {req_csvline[3]: <6s}|")
            txt_out.write(
                f" {req_csvline[4]: <6s}| {req_csvline[5]: <6s}| {req_csvline[6]: <6s}||")
            txt_out.write(
                f" {cegis_csvline[0]: <2s}| {cegis_csvline[1]: <6s}| {cegis_csvline[2]: <6s}| {cegis_csvline[3]: <6s}| {cegis_csvline[4] : <6s}")

            txt_out.write("\n")
    txt_out.write(f"{sp:-<103s}\n")
    txt_out.write("TABLE 3:\n")
    txt_out.write(caption3)
    txt_out.close()


def means(v):
    n = len(v)
    tot_time = 0.0
    tot_iterations = 0
    for p in v:
        if p == ["TIMEOUT"]:
            pass
        else:
            iterations, time = p
            tot_iterations = tot_iterations + iterations
            tot_time = tot_time + time
    return (int(tot_iterations / n), float(tot_time / n))


def times_of_bdata(b_data):
    s_t = "-"
    v_t = "-"
    tot_iters = "0"
    s_last = "?"

    if "res" in b_data:
        tot_iters, v_time, s_time, _ = b_data["res"]
        tot_iters = "%i" % int(tot_iters)
        s_t = "%3.2f" % float(s_time)
        v_t = "%3.1f" % (100.0 * (float(v_time) / float(s_time)))
    else:
        s_t = "-"
        tot_iters = "%i" % int(b_data["max"])

    if str(b_data["max"]) in b_data:
        s_last = float(b_data[str(b_data["max"])][1])
        if "res" not in b_data:
            v_t = float(b_data[str(b_data["max"])][0])
            # Compare to total time including time waiting for solution before timeout
            v_t = "%3.1f" % (100 * v_t / 600.0)

        if int(tot_iters) > 1:
            s_last = "%3.2f" % s_last
        else:
            s_last = "."

    return [s_t, v_t, tot_iters, s_last]


def build_line(data, benchmark_class, benchmark_file):
    bkey = (benchmark_class + "/" + benchmark_file + ".ml").strip()
    csvline = [benchmark_class, benchmark_file]
    for algo in algorithms:
        if (bkey, algo) in data.keys():
            bdata = data[bkey, algo]
            for ver in versions:
                times = ["?", "?", "?", "?"]
                if ver in bdata.keys():
                    times = times_of_bdata(bdata[ver])
                csvline += times
        else:
            for ver in versions:
                times = ["?", "?", "?", "?"]
                csvline += times
    return csvline


def csv_table(data, csvfile):
    with open(csvfile, 'w') as csvout:
        csvout.write("Category,File")
        for algor in algorithms:
            for opt in versions:
                csvout.write("," + algor + "+" + opt)
                for field in fields[1:]:
                    csvout.write("," + field)
        csvout.write("\n")
        for benchmark_class, benchmarks in show_benchmarks:
            for benchmark_file, _ in benchmarks:
                csvline = build_line(data, benchmark_class, benchmark_file)
                csvout.write(",".join(csvline) + '\n')


def raw_to_csv(input_file):
    benchmark_data = {}
    with open(input_file) as raw:
        lines = raw.readlines()
        num_lines = len(lines)
        benchfile, method = "none", "none"
        for i in range(num_lines):
            infos = ""
            s = lines[i]
            if s.startswith("B:"):
                inst = s.strip("B:").strip("/").split(",")
                benchfile, method = inst[0], inst[1].strip(" ").split("+")
                method_base = method[0]
                method_opt = "all"
                if len(method) > 1 and method[1] != "":
                    method_opt = method[1].strip()
                key = benchfile, method_base, method_opt
                if key not in benchmark_data:
                    benchmark_data[key] = {}
                if "max" not in benchmark_data[key]:
                    benchmark_data[key]["max"] = 0
            elif s == "success":
                continue
            else:
                infos = lines[i].strip().split(",")
                # A line with 5 numbers if for an intermediate synthesis step.
                if len(infos) == 5:
                    step, verif_time, time, tnum, unum = infos[0], float(
                        infos[1]), float(infos[2]), int(infos[3]), int(infos[4])
                    benchmark_data[key][step] = verif_time, time, tnum, unum
                    benchmark_data[key]["max"] = max(
                        benchmark_data[key]["max"], int(step))
                # A line with 3 numbers means synthesis finished.
                if len(infos) == 3:
                    try:
                        steps = int(infos[0])
                    # Res = #of refinement steps, verif. time, total time.
                        if "res" in benchmark_data[key]:
                            old_res = benchmark_data[key]["res"]
                            old_count = old_res[3]
                            new_steps = max(old_res[0], steps)
                            new_verif = incr_avg(
                                old_res[1], old_count, float((infos[1])))
                            new_time = incr_avg(
                                old_res[2], old_count, float(infos[2]))
                            new_count = old_count + 1
                            benchmark_data[key]["res"] = new_steps, new_verif, new_time, new_count
                        else:
                            benchmark_data[key]["res"] = steps, float(
                                infos[1]), float(infos[2]), 1
                    except:
                        continue

    bd = {}
    for k, v in benchmark_data.items():
        file, method, flags = k
        key = file, method
        if key in bd.keys():
            bd[key][flags] = v
        else:
            bd[key] = {}
            bd[key][flags] = v
    return bd


def benchsort(x):
    return x[0] + x[1] + x[2]


if __name__ == "__main__":
    aparser = argparse.ArgumentParser()
    aparser.add_argument(
        "-i", "--input", help="The input file produced by running test.py", type=str, default="benchmarks/constraints_bench.txt")
    aparser.add_argument(
        "-o", "--output", help="The output text file for the table.", type=str, default="benchmarks/table.txt")
    aparser.add_argument(
        "-t", "--table", help="Table number that the script must generate.", type=int, choices=[1, 2, 3], default=1)
    aparser.add_argument(
        "-c", "--csv", help="The output csv file for results.", type=str, default="benchmarks/constraints_results.csv")
    aparser.add_argument(
        "-e", "--explain", help="Explain where the benchmarks are stored.", action="store_true")

    args = aparser.parse_args()

    if args.explain:
        explain()
        exit()

    input_file = args.input
    output_file = args.csv
    table_no = args.table
    tex_out = args.output

    if table_no > 0 and table_no < 4:
        if len(sys.argv) < 5:
            print(
                "Please provide output .tex file after table number.")

    thedict = raw_to_csv(input_file)

    csv_table(thedict, output_file)

    if table_no == 0:
        explain()
        exit()

    if table_no == 1:
        tex_out = sys.argv[4]
        produce_txt_table(tex_out, thedict)

    if table_no == 2:
        tex_out = sys.argv[4]
        table2(tex_out, thedict)

    if table_no == 3:
        tex_out = sys.argv[4]
        table3(tex_out, thedict)
