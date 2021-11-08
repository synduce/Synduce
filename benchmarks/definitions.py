import sys

# Timeout for all experiments.
timeout_value = 200
timestamp_definition = "%d-%m-%y-%H:%M:%S"
experimental_setup = "a machine with and AMD® Ryzen 7 3700x 8-core processor and 32GB Ram running Ubuntu 20.04"
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
kw_segis = "Symbolic CEGIS"
kw_cegis = "Concrete CEGIS"
kw_path = "Path"

if sys.platform.startswith('linux'):
    timeout = ("timeout %i" %
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
    ["list/last.pmrs", ""],
    ["constraints/sortedlist/count_lt.ml", ""],
    ["constraints/bst/count_lt.ml", "-NB"],
    ["constraints/ensures/mps_no_ensures.ml", "-B"],
    ["list/largest_diff_sorted_list_nohead.ml", ""],
    ["list/poly_no_fac.ml", ""],
    ["unrealizable/po_sorted.ml", ""]
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


base_benchmark_set = [
    # Combine
    ["combine/mts.pmrs", ""],
    ["combine/mts_and_mps.pmrs", ""],
    ["combine/mss_with_sum.ml", ""],
    # Compressed list
    ["compressed_list/sum.ml", ""],
    # Indexed list
    ["indexed_list/search.ml", ""],
    ["indexed_list/position_polynomial.ml", ""],
    ["indexed_list/sum_elts_lt_pos.ml", ""],
    # Misc
    ["misc/count_between.ml", ""],
    ["misc/composed_unkwns.ml", ""],
    ["misc/simple_nnf.ml", ""],
    ["misc/unknowns_are_ids.ml", ""],
    # List
    ["list/alist_sum.ml", ""],
    ["list/atoi.ml", ""],
    ["list/bal.ml", ""],
    ["list/hamming.pmrs", ""],
    ["list/last.pmrs", ""],
    ["list/lenhom.pmrs", ""],
    ["list/line_of_sight.ml", ""],
    ["list/maxcount.pmrs", ""],
    ["list/maxhom.pmrs", ""],
    ["list/mincount.pmrs", ""],
    ["list/minhom.pmrs", ""],
    ["list/mpshom.pmrs", ""],
    ["list/mtshom.pmrs", ""],
    ["list/mts_and_mps_hom.pmrs", ""],
    ["list/msshom.pmrs", ""],
    ["list/prodhom.pmrs", ""],
    ["list/polyhom.pmrs", ""],
    ["list/search.pmrs", ""],
    ["list/sndmax.ml", ""],
    ["list/sndmin.ml", ""],
    ["list/sumgt.ml", ""],
    ["list/sumhom.pmrs", ""],
    ["list/sumodds.pmrs", ""],
    ["list/sumevens.pmrs", ""],
    ["list/zero_after_one.ml", ""],
    ["list/zeros_ones.ml", ""],
    # List to tree
    ["list/issorted.pmrs", "-t"],
    ["list_to_tree/search.pmrs", ""],
    ["list_to_tree/search_v2.pmrs", ""],
    ["list_to_tree/search_v3.pmrs", ""],
    ["list_to_tree/mls.pmrs", ""],
    # Numbers
    ["numbers/int_nat_toint.ml", ""],
    ["numbers/int_nat_twosum.ml", ""],
    # Ptrees
    ["ptree/maxheads.pmrs", ""],
    ["ptree/maxlast.pmrs", ""],
    ["ptree/maxsum.pmrs", ""],
    ["ptree/mul.pmrs", ""],
    ["ptree/sum.pmrs", ""],
    # Sorting lists
    ["sort_list/min.ml", ""],
    ["sort_list/max.ml", ""],
    ["sort_list/sumgtz.ml", ""],
    # Tail optimization
    ["tailopt/mts.pmrs", ""],
    ["tailopt/mps.pmrs", ""],
    ["tailopt/sum.pmrs", ""],
    # Terms
    ["terms/height.ml", ""],
    # Trees
    ["tree/maxPathWeight.pmrs", ""],
    ["tree/maxtree.pmrs", ""],
    ["tree/maxtree2.pmrs", ""],
    ["tree/min.pmrs", ""],
    ["tree/minmax.pmrs", ""],
    ["tree/mips.pmrs", ""],
    ["tree/mits.pmrs", ""],
    ["tree/mpps.pmrs", "--no-gropt"],
    ["tree/poly.pmrs", ""],
    ["tree/poly2.ml", ""],
    ["tree/sorted.pmrs", "-t"],
    ["tree/sorted_2.ml", ""],
    ["tree/sumtree.pmrs", ""],
    # Tree paths
    ["treepaths/height.pmrs", ""],
    ["treepaths/leftmostodd.pmrs", "-b 6"],
    ["treepaths/maxPathWeight.pmrs", ""],
    ["treepaths/maxPathWeight2.pmrs", ""],
    ["treepaths/mips.pmrs", ""],
    ["treepaths/sum.pmrs", ""],
    # Split lists
    ["unimodal_lists/sum.ml", ""],
    ["unimodal_lists/search_no_optim.ml", ""],
    # Zippers
    ["zippers/list_sum.ml", ""],
    ["zippers/list_sum_basic.ml", ""],
]

constraint_benchmarks = [
    # Ensures
    ["constraints/ensures/mps_no_ensures.ml", "-B"],
    ["constraints/ensures/mts_no_ensures.ml", ""],
    ["constraints/ensures/mss_no_ensures.ml", "-B"],
    # alist
    ["constraints/alist/count_eq2.ml", "-NB"],
    ["constraints/alist/count_eq.ml", ""],
    ["constraints/alist/sums.ml", ""],
    ["constraints/alist/most_frequent.ml", ""],
    # all positive
    ["constraints/all_positive/list_mps.ml", ""],
    ["constraints/all_positive/sndmin.ml", "-NB"],
    ["constraints/all_positive/sndmax.ml", "-NB"],
    # balanced_tree
    ["constraints/balanced_tree/node_count.ml", "-N"],
    ["constraints/balanced_tree/height.ml", "-N"],
    ["constraints/balanced_tree/height_v2.ml", "-NB"],
    # bst
    ["constraints/bst/contains.ml", ""],
    ["constraints/bst/count_lt.ml", "-NB"],
    ["constraints/bst/count_between.ml", "-NB --no-gropt"],
    ["constraints/bst/most_frequent_v1.ml", ""],
    ["constraints/bst/from_list_contains.ml", ""],
    ["constraints/bst/from_list_max.ml", "-NB -n 50"],
    ["constraints/bst/sum_gt_by_key.ml", "-NB -n 50"],
    # constantlist
    ["constraints/constantlist/index_of.ml", ""],
    ["constraints/constantlist/contains.ml", ""],
    # empty_right
    ["constraints/empty_right_subtree/contains.ml", "-N"],
    # evenlist
    ["constraints/evenlist/parity_of_first.ml", ""],
    ["constraints/evenlist/parity_of_last.ml", ""],
    ["constraints/evenlist/first_odd.ml", ""],
    ["constraints/evenlist/parity_of_sum.ml", ""],
    # even_tree
    ["constraints/even_tree/sum_of_parities.ml", "-NB"],
    ["constraints/even_tree/parity_of_max.ml", ""],
    # memo
    ["constraints/memo/tree_size.ml", "-NB"],
    ["constraints/memo/constant.ml", ""],
    ["constraints/memo/mts_memo_sum.ml", ""],
    ["constraints/memo/max_contains.ml", "-NB"],
    ["constraints/memo/count_lt.ml", "-NB -n 50"],
    ["constraints/memo/max_sum_gt.ml", "-NB"],
    ["constraints/memo/proper_indexation_sum_lt_pos_v2.ml", ""],
    ["constraints/memo/proper_indexation_sum_lt_pos.ml", ""],
    # Size constraint
    ["constraints/size/obfuscated_length.ml", ""],
    ["constraints/size/obfuscated_length_3.ml", ""],
    # program
    # ["constraints/program/typecheck.ml", ""],
    # sortedlist
    ["constraints/sortedlist/min.ml", ""],
    ["constraints/sortedlist/max.ml", ""],
    ["constraints/sortedlist/count_lt.ml", ""],
    ["constraints/sortedlist/index_of.ml", ""],
    ["constraints/sortedlist/is_intersection_empty.ml", ""],
    ["constraints/sortedlist/largest_diff.ml", ""],
    ["constraints/sortedlist/smallest_diff.ml", ""],
    ["constraints/sortedlist/sndmind.ml", ""],
    ["constraints/sortedlist/sndmax_empty_base_case.ml", "-NB"],
    ["constraints/sortedlist/sndmax_empty_base_case_v2.ml", "-NB"],
    ["constraints/sortedlist/sndmax_len2_base_case.ml", "-NB"],
    ["constraints/sortedlist/parallel_min.ml", "-NB"],
    ["constraints/sortedlist/parallel_max.ml", "-NB"],
    ["constraints/sortedlist/parallel_max2.ml", "-NB"],
    # Sorted and indexed
    ["constraints/sorted_and_indexed/count_lt0.ml", "-NB -n 30"],
    ["constraints/sorted_and_indexed/count_lt.ml", "-NB -n 30"],
    # symmetric tree
    ["constraints/symmetric_tree/sum.ml", "-N"],
    ["constraints/symmetric_tree/height.ml", "-N"],
    ["constraints/symmetric_tree/min.ml", "-N"],
    # unimodal
    # ["constraints/unimodal_list/max_logn.ml", "-B"],
    ["constraints/unimodal_list/max_logn_v2.ml", ""],
    # ["constraints/unimodal_list/max_in_um_pos.ml", ""],
]

lifting_benchmarks = [
    # Indexed list
    ["indexed_list/position_polynomial_no_index.ml", ""],
    ["indexed_list/search_no_index.ml", ""],
    ["indexed_list/sum_elts_lt_pos_no_len.ml", "--no-assumptions"],
    # Automatic parallelization
    ["list/atoi_no_fac.ml", "--no-gropt"],
    ["list/is_sorted_no_last.ml", ""],
    ["list/largest_diff_sorted_list_nohead.ml", ""],
    ["list/mps_no_sum.ml", ""],
    ["list/poly_no_fac.ml", ""],
    ["list/zero_after_one_no.ml", ""],
    # Tail optimizations
    ["tailopt/mps_no_sum.ml", ""],
    # Combining traversals
    ["combine/mts_and_mps_nosum.ml", ""],
    # Sorting list makes things easier
    ["sort_list/sndmax.ml", ""],
    ["sort_list/sndmin.ml", ""],
    # Switching tree traversals
    ["tree/gradient.ml", ""],
    ["tree/mits_nosum.ml", ""],
    # Unimodal lists (might also be due to representation)
    ["unimodal_lists/prod_needs_aux.ml", ""]
]

unrealizable_benchmarks = [
    ["unrealizable/po_sorted.ml", ""],
    ["unrealizable/balanced_tree_logn_sum.ml", ""],
    ["unrealizable/twosum.ml", ""],
    ["unrealizable/minmax.ml", ""],
    ["unrealizable/count_between.ml", ""]
]

benchmark_set = constraint_benchmarks + base_benchmark_set + lifting_benchmarks

# Extra extra benchmarks (takes extra time..)
extra_benchmarks = [
    ["list/bal.ml", ""],
    ["list/lpeak.ml", ""],

]


def summarize():
    num_constraint = len(constraint_benchmarks)
    num_base = len(base_benchmark_set)
    num_lifting = len(lifting_benchmarks)
    num_extrs = len(extra_benchmarks)
    total = num_constraint + num_base + num_lifting + num_extrs
    print("%i benchmarks in total:" % total)
    print("\t- %i basic benchmarks (run with --base)." % num_base)
    print("\t\tincluding %i in kick-the-tires set (run with --kick-the-tires)." %
          len(kick_the_tires_set))
    print("\t- %i benchmarks with requires constraints (run with --constraint-benchmarks)." % num_constraint)
    print("\t- %i benchmarks with lifting (run with --lifting-benchmarks)." % num_lifting)
    print("\t- %i extras benchmarks." % num_extrs)


algorithms = ["se2gis", "segis", "cegis"]
versions = ["all", "ini", "st", "d", "off"]
fields = ["synt", "verif", "#i", "last"]

show_benchmarks = [
    # All positive
    ["constraints/all_positive", [
        ["list_mps",     [" Elements > 0    ",
                          " mps   ", "no", "no",  "mps", 0]]
    ]],
    # Alist
    ["constraints/alist", [
        ["count_eq2",     ["              ",
                           "num elt. = (v2) ", "yes", "yes",  "num=", 0]],
        ["count_eq",      ["  Association ",
                           "num elt. =      ", "no", "no", "num=(v2)", 0]],
        ["sums",          ["   List       ",  "sums", "no", "no", "sums ", 0]],
        ["most_frequent", ["              ",
                           "most frequent", "no", "no",  "most freq.", 0]],
    ]],
    # Balanced tree
    ["constraints/balanced_tree", [
        ["node_count",  [" Balanced ", "node count", "yes", "no",  "node cnt.", 0]],
        ["height",      ["  Tree    ", "height", "yes", "no", "height", 0]],
        ["height_v2",   ["          ",
                         "height (v2)  ", "yes", "yes", "height v2.", 0]],
    ]],
    # BST
    ["constraints/bst", [
        ["contains",           ["          ",
                                "contains elt", "no", "no", "contains", 0]],
        ["count_lt",           ["  Binary  ",
                                "count smaller elts ", "yes", "yes", "count <", 0]],
        ["most_frequent_v1",   ["Search",
                                " most frequent ", "no", "no", "most freq. ", 0]],
        ["from_list_contains", ["  Tree    ",
                                "list-contains", "no", "no", "contains (list)", 0]],
        ["from_list_max",      ["          ",
                                "of list max ", "yes", "yes", "max (list)", 0]],
        ["sum_gt_by_key",      ["          ",
                                "sum if key larger", "yes", "yes", "sumkey", 0]],
    ]],
    # Combining traversals
    ["combine", [
        ["mts_and_mps_nosum", ["Combine", "mts+mps", "no", "no", "m(t+p)s", 1]]]],
    # Constant list
    ["constraints/constantlist", [
        ["index_of",  [" Constant ", "index of elt. ", "no", "no", "idx", 0]],
        ["contains",  ["  List    ", "contains elt  ", "no", "no", "contains", 0]],
    ]],
    # Empty right subtree
    ["constraints/empty_right_subtree", [
        ["contains",  [" Empty Subtree", " contains elt", "yes", "no", "contains", 0]],
    ]],
    # Even list
    ["constraints/evenlist",
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
    ["constraints/even_tree",
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
                                             "sum smaller pos.", "no", "no", "sum pos", 1]]
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
    ["constraints/memo", [
        ["tree_size",     ["             ", "tree size", "yes", "yes",  "size", 0]],
        ["constant",      ["  Tree       ", "constant", "no", "no", " constant", 0]],
        ["max_contains",  [" Memoizing   ",
                           " contains elt. ", "yes", "yes", "contains", 0]],
        ["count_lt",      [" Information ", "count smaller", "yes", "yes",  "cnt <", 0]],
        ["max_sum_gt",    ["             ",
                           "sum of elts larger", "yes", "yes",  "sum >", 0]],
        ["proper_indexation_sum_lt_pos_v2",
         ["             ", "sum elts larger pos", "no", "no", "sum > pos", 0]],
        ["proper_indexation_sum_lt_pos",
         ["             ", "sum elts larger pos .v2", "no", "no", "sum > pos v2", 0]]

    ]],
    # Program
    # ["constraints/program", [
    #     ["typecheck",  [" Program AST ", " type check  ", "yes", "no", "type chk", 0]],
    # ]],
    # Symmetric tree
    ["constraints/symmetric_tree", [
        ["sum",         [" Symmetric  ", "sum", "yes", "no", "sum", 0]],
        ["height",      ["    Tree    ", "height", "yes", "no", "height", 0]],
        ["min",         ["            ", "min", "yes", "no", "min", 0]]
    ]],
    # Sorted
    ["constraints/sortedlist", [
        ["min",                    ["        ", "min ", "no", "no", "min", 0]],
        ["max",                    [" Sorted ", "max ", "no", "no", "max", 0]],
        ["count_lt",               [" List   ",
                                    "count elt. smaller ", "no", "no", "cnt <", 0]],
        ["index_of",               ["        ",
                                    "index of elt ", "no", "no", "idx", 0]],
        ["is_intersection_empty",  ["        ", "inter-empty ", "no", "no", "∩-∅", 0]],
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
    ["constraints/sorted_and_indexed", [
        ["count_lt0",                    ["        ",
                                          "count smaller 0", "yes", "yes", "# < 0", 0]],
        ["count_lt",                    ["        ",
                                         "count smaller", "yes", "yes", "# < 0", 0]],
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


def explain():
    print("Benchmarks are stored in the benchmarks/ folder.")
    print(f"{kw_class: <20s} | {kw_benchmark : <25s} | L | {kw_path : <30s}")
    for cat_folder, cat_benchmarks in show_benchmarks:
        print(f"{dash:-<80s}-")
        for bench_file, attributes in cat_benchmarks:
            bench_path = "benchmarks/" + cat_folder + "/" + bench_file + ".pmrs"
            print(
                f" {attributes[0]: <19s} | {attributes[1]: <25s} | {attributes[5]} | {bench_path : <30s}")


def floti(f):
    ret = "-"
    try:
        ret = float(f)
    except:
        ret = float(timeout_value)
    return ret


timeout_names = ["N/A", "-", "timeout", "TIMEOUT"]


def speedup(a, b):
    if b in timeout_names and a in timeout_names:
        return "?"
    if b in timeout_names:
        if a not in timeout_names:
            return "∞"
        else:
            return "!"
    elif a in timeout_names:
        return "-∞"
    else:
        a = floti(a)
        b = floti(b)
        if b > 0:
            return "%3.1f" % (b / a)
        else:
            return -1
