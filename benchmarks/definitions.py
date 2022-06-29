from configurations_set_search import incomplete_benchmarks
import sys

# Timeout for all experiments.
global timeout_value
timeout_value = 1200
timestamp_definition = "%d-%m-%y-%H:%M:%S"
experimental_setup = "a machine with an AMD® Ryzen 7 3700x 8-core processor and 32GB Ram running Ubuntu 20.04"
experimental_setup_2 = "a laptop with an Intel Core i7-8750H 6-core processor and 32GB Ram running Ubuntu 21.04"
experimental_setup_3 = "a machine with an Intel(R) Xeon(R) CPU E5-2620 16-core processor @ 2.10GHz and 32GB Ram running Ubuntu 21.04"
sp = " "
dash = "-"
kw_class = "... Class"
kw_benchmark = "... Benchmark"
kw_time = "time"
kw_steps = "#st"
kw_tlast = "Tlast"
kw_ver = "ver.%"
kw_toolname = "Synduce"
kw_tool_main_algo = "S-CEGIS${\\bf +PB}$"
kw_tool_main_algo_v2 = "SE²GIS"
kw_baseline = "Baseline"
kw_segis = "Symbolic CEGIS"
kw_segis_short = "S-CEGIS"
kw_cegis = "Concrete CEGIS"
kw_cegis_short = "C-CEGIS"
kw_segis_uc = "Symbolic CEGIS with UC"
kw_segis_uc_short = "SEGIS+UC"
kw_path = "Path"
plot_fontsize = 13

if sys.platform.startswith('linux'):
    timeout = ("timeout %i" %
               (timeout_value))
elif sys.platform.startswith('darwin'):
    timeout = ("timelimit -t%i" % timeout_value)
else:
    print("Platform %s not supported" % sys.platform)
    exit(-1)

kick_the_tires_set = [
    ["list/sum.ml", ""],
    ["ptree/sum.ml", ""],
    ["tree/sum.ml", ""],
    ["tailopt/sum.ml", ""],
    ["treepaths/sum.ml", ""],
    # output reading fails without the option
    ["treepaths/height.ml", "--se2gis"],
    ["treepaths/maxPathWeight.ml", ""],
    ["ptree/mul.ml", ""],
    ["tree/max.ml", ""],
    ["tree/min.pmrs", ""],
    ["tree/maxtree2.pmrs", ""],
    ["list/sumodds.ml", ""],
    ["list/prod.ml", ""],
    ["list/poly.ml", ""],
    ["list/hamming.ml", ""],
    ["list/sumevens.ml", ""],
    ["list/len.ml", ""],
    ["list/last.pmrs", ""],
    ["constraints/sortedlist/count_lt.ml", ""],
    ["constraints/bst/count_lt.ml", "-NB"],
    ["constraints/ensures/mps_no_ensures.ml", "-B"],
    ["list/largest_diff_sorted_list_nohead.ml", "--se2gis"],
    ["list/poly_no_fac.ml", "--se2gis"],
    ["unrealizable/po_sorted.ml", "", False]
]

reduced_benchmark_set_table2 = [
    ["list/sum.ml", ""],
    ["ptree/sum.ml", ""],
    ["tree/sum.ml", ""],
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
    ["tree/sorted.pmrs", "--no-detupling"],
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
    # Expressions
    ["expressions/norm_arith.ml", ""],
    ["expressions/max_subexpr_sum.ml", ""],
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
    ["list/block_ones_extremas.ml", ""],
    ["list/hamming.pmrs", ""],
    ["list/last.pmrs", ""],
    ["list/lenhom.pmrs", ""],
    ["list/line_of_sight.ml", ""],
    ["list/maxcount.pmrs", ""],
    ["list/max_block_ones.ml", ""],
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
    ["list/slsp.ml", ""],
    ["list/sndmax.ml", ""],
    ["list/sndmin.ml", ""],
    ["list/sumgt.ml", ""],
    ["list/sumhom.pmrs", ""],
    ["list/sumodds.pmrs", ""],
    ["list/sumevens.pmrs", ""],
    ["list/zero_after_one.ml", ""],
    ["list/zeros_ones.ml", ""],
    # List to tree
    ["list/issorted.pmrs", "--no-detupling"],
    ["list_to_tree/search.pmrs", ""],
    ["list_to_tree/search_v2.pmrs", ""],
    ["list_to_tree/search_v3.pmrs", ""],
    ["list_to_tree/mls.pmrs", ""],
    # Nested lists
    ["nested_lists/mtss.ml", ""],
    ["nested_lists/pyramid_intervals.ml", ""],
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
    ["tree/sorted.pmrs", "--no-detupling"],
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
    ["constraints/ensures/mtss_noe.ml", ""],
    ["constraints/ensures/mtss_full_noe.ml", ""],
    ["constraints/ensures/mpss_noe.ml", ""],
    ["constraints/ensures/mpss_full_noe.ml", ""],
    ["constraints/ensures/maxsegstrip_noe.ml", ""],
    ["constraints/ensures/maxsegstrip_full_noe.ml", ""],
    ["constraints/ensures/nested_minmax_noe.ml", ""],
    ["constraints/ensures/bal_2.ml", ""],
    # alist
    ["constraints/alist/count_eq2.ml", "-NB --se2gis"],
    ["constraints/alist/count_eq.ml", ""],
    ["constraints/alist/most_frequent.ml", ""],
    ["constraints/alist/most_frequent_v2.ml", ""],
    ["constraints/alist/sums.ml", ""],
    # all positive
    ["constraints/all_positive/list_mps.ml", ""],
    ["constraints/all_positive/sndmin.ml", "-NB"],
    ["constraints/all_positive/sndmax.ml", "-NB"],
    # balanced_tree
    ["constraints/balanced_tree/height.ml", "-N"],
    ["constraints/balanced_tree/height_v2.ml", "-NB"],
    ["constraints/balanced_tree/node_count.ml", "-N"],
    # bst
    ["constraints/bst/check_mem_included.ml", ""],
    ["constraints/bst/check_included_comb.ml", ""],
    ["constraints/bst/check_no_subtree_sum_gt2.ml", ""],
    ["constraints/bst/contains.ml", ""],
    ["constraints/bst/contains_bool_2.ml", ""],
    ["constraints/bst/contains_v2.ml", ""],
    ["constraints/bst/contains_v3.ml", ""],
    ["constraints/bst/contains_no_ensures.ml", ""],
    ["constraints/bst/count_lt.ml", ""],
    ["constraints/bst/count_between.ml", ""],
    ["constraints/bst/from_list_contains.ml", ""],
    ["constraints/bst/from_list_max.ml", "-NB -n 50"],
    ["constraints/bst/minmax_separate.ml", ""],
    ["constraints/bst/minmax_sim.ml", ""],
    ["constraints/bst/most_frequent_v1.ml", ""],
    ["constraints/bst/sum_gt_by_key.ml", ""],
    ["constraints/bst/sum_between.ml", ""],
    # constantlist
    ["constraints/constantlist/contains.ml", ""],
    ["constraints/constantlist/index_of.ml", ""],
    # empty_right
    ["constraints/empty_right_subtree/contains.ml", "-N"],
    ["constraints/empty_right_subtree/sum.ml", ""],
    # evenlist
    ["constraints/evenlist/first_odd.ml", ""],
    ["constraints/evenlist/parity_of_first.ml", ""],
    ["constraints/evenlist/parity_of_last.ml", ""],
    ["constraints/evenlist/parity_of_sum.ml", ""],
    # even_tree
    ["constraints/even_tree/sum_of_parities.ml", "-NB"],
    ["constraints/even_tree/parity_of_max.ml", ""],
    # memo
    ["constraints/memo/constant.ml", ""],
    ["constraints/memo/count_lt.ml", "-NB -n 50"],
    ["constraints/memo/max_contains.ml", "-NB"],
    ["constraints/memo/max_sum_gt.ml", "-NB"],
    ["constraints/memo/minmax.ml", ""],
    ["constraints/memo/mts_memo_sum.ml", ""],
    ["constraints/memo/proper_indexation_sum_lt_pos_v2.ml", ""],
    ["constraints/memo/proper_indexation_sum_lt_pos.ml", ""],
    ["constraints/memo/tree_size.ml", "-NB"],
    # Size constraint
    ["constraints/size/obfuscated_length.ml", ""],
    ["constraints/size/obfuscated_length_3.ml", ""],
    # program
    # ["constraints/program/typecheck.ml", ""],
    ["constraints/program/memo_psum.ml", ""],
    # sortedlist
    ["constraints/sortedlist/count_lt.ml", ""],
    ["constraints/sortedlist/exists_equal_elems.ml", ""],
    ["constraints/sortedlist/exists_equal_elems_v2.ml", ""],
    ["constraints/sortedlist/failing_slspplus.ml", ""],
    ["constraints/sortedlist/index_of.ml", ""],
    ["constraints/sortedlist/interval_intersection.ml", ""],
    ["constraints/sortedlist/is_intersection_empty.ml", ""],
    ["constraints/sortedlist/largest_diff.ml", ""],
    ["constraints/sortedlist/largest_diff_poslist.ml", ""],
    ["constraints/sortedlist/largest_even.ml", ""],
    ["constraints/sortedlist/largest_even_pos.ml", ""],
    ["constraints/sortedlist/largest_even_pos_v2.ml", "--se2gis"],
    ["constraints/sortedlist/max.ml", ""],
    ["constraints/sortedlist/max_point_sum.ml", ""],
    ["constraints/sortedlist/min.ml", ""],
    ["constraints/sortedlist/mss.ml", ""],
    ["constraints/sortedlist/nestedl_pyramid.ml", ""],
    ["constraints/sortedlist/parallel_min.ml", "-NB"],
    ["constraints/sortedlist/parallel_max.ml", "-NB"],
    ["constraints/sortedlist/parallel_max2.ml", "-NB"],
    ["constraints/sortedlist/slsp.ml", ""],
    ["constraints/sortedlist/smallest_diff.ml", ""],
    ["constraints/sortedlist/sndmind.ml", ""],
    ["constraints/sortedlist/sndmax_empty_base_case.ml", "-NB"],
    ["constraints/sortedlist/sndmax_empty_base_case_v2.ml", "-NB"],
    ["constraints/sortedlist/sndmax_len2_base_case.ml", "-NB"],
    # Sorted and indexed
    ["constraints/sorted_and_indexed/count_lt0.ml", "-NB -n 30"],
    ["constraints/sorted_and_indexed/count_lt.ml", "-NB -n 30"],
    # symmetric tree
    ["constraints/symmetric_tree/height.ml", "-N"],
    ["constraints/symmetric_tree/min.ml", "-N"],
    ["constraints/symmetric_tree/sum.ml", "-N"],
    # unimodal
    ["constraints/unimodal_list/max_logn_v2.ml", ""],
    ["constraints/unimodal_list/max_in_um_pos.ml", ""],
    ["constraints/unimodal_list/max_logn.ml", "-B"],
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
    ["unrealizable/approximately_pareto.ml", "", False],
    ["unrealizable/balanced_tree_logn_sum.ml", "", False],
    ["unrealizable/common_elt.ml", "", False],
    ["unrealizable/count_between.ml", "", False],
    ["unrealizable/count_between_wrong_comparison.ml", "", False],
    ["unrealizable/count_between_swap_ec_no_gr_gl.ml", "", False],
    ["unrealizable/count_between_ec_no_gr_gl.ml", "",  False],
    ["unrealizable/count_between_ec_no_gr.ml", "", False],
    ["unrealizable/contains.ml", "", False],
    ["unrealizable/contains2.ml", "", False],
    ["unrealizable/eval_psum.ml", "", False],
    ["unrealizable/interval_intersection.ml", "", False],
    ["unrealizable/largest_even_pos.ml", "", False],
    ["unrealizable/minmax.ml", "", False],
    ["unrealizable/minmax_mistake.ml", "", False],
    ["unrealizable/magic_nesting.ml", "", False],
    ["unrealizable/max_low_table_sum.ml", "", False],
    ["unrealizable/nested_min_sum_max_mts.ml", "", False],
    ["unrealizable/lpeak_noimf.ml", "", False],
    ["unrealizable/memo_max.ml", "", False],
    ["unrealizable/most_frequent_noinv.ml", "", False],
    ["unrealizable/mps_in_partitioned_list.ml", "", False],
    ["unrealizable/nested_min_max_mixed.ml", "", False],
    ["unrealizable/pyramid_maxsort.ml", "", False],
    ["unrealizable/po_sorted.ml", "", False],
    ["unrealizable/simple_parity.ml", "", False],
    ["unrealizable/swap_game.ml", "", False],
    ["unrealizable/swap_game_2.ml", "", False],
    ["unrealizable/twosum.ml", "", False],
    ["indexed_list/position_polynomial_no_index.ml", " --no-lifting", False],
    ["indexed_list/search_no_index.ml", " --no-lifting", False],
    ["indexed_list/sum_elts_lt_pos_no_len.ml",
        "--no-assumptions --no-lifting", False],
    ["list/atoi_no_fac.ml", "--no-gropt --no-lifting", False],
    ["list/is_sorted_no_last.ml", " --no-lifting", False],
    ["list/largest_diff_sorted_list_nohead.ml", " --no-lifting", False],
    ["list/mps_no_sum.ml", " --no-lifting", False],
    ["list/poly_no_fac.ml", " --no-lifting", False],
    ["list/zero_after_one_no.ml", " --no-lifting", False],
    ["tailopt/mps_no_sum.ml", " --no-lifting", False],
    ["combine/mts_and_mps_nosum.ml", " --no-lifting", False],
    ["sort_list/sndmax.ml", " --no-lifting", False],
    ["sort_list/sndmin.ml", " --no-lifting", False],
    ["tree/gradient.ml", " --no-lifting", False],
    ["tree/mits_nosum.ml", " --no-lifting", False],
    ["unimodal_lists/prod_needs_aux.ml",  "--no-lifting", False],
]

incomplete_benchmarks = incomplete_benchmarks

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
    num_unr = len(unrealizable_benchmarks)
    total = num_constraint + num_base + num_lifting + num_extrs
    print("%i benchmarks in total:" % total)
    print("\t• %i basic benchmarks (run with --base)." % num_base)
    print("\t\tincluding %i in kick-the-tires set (run with --kick-the-tires or -b small)." %
          len(kick_the_tires_set))
    print("\t• %i benchmarks with requires constraints (run with -b constraint)." %
          num_constraint)
    print("\t• %i unrealizable benchmarks (run with -b unr)." % num_unr)
    print("\t• %i benchmarks with lifting (run with -b lifting)." % num_lifting)
    print("\t• %i extras benchmarks." % num_extrs)
    print("\t🎉 that means %i benchmarks in T5, %i in T6." % (num_unr + num_constraint,  (num_unr + num_constraint + num_lifting))
          )


algorithms = ["se2gis", "segis", "cegis"]
versions = ["all", "ini", "st", "d", "off"]
fields = ["synt", "verif", "#i", "last"]

show_benchmarks = {
    # Ensures only
    "constraints/ensures": {
        "mps_no_ensures": ["", "mps", "list->concat-list", True],
        "mts_no_ensures": ["", "mts", "list->concat-list", True],
        "mss_no_ensures": ["Inferring", "mss", "list->concat-list", True],
        "mtss_noe": ["Postconditions", "max top strip", "nested list->concat-list", True],
        "mtss_full_noe": ["", "max top strip (no hint)", "nested list->concat-list", True],
        "mpss_noe": ["", "max bottom strip", "nested list->concat-nested list", True],
        "mpss_full_noe": ["", "max bottom strip (no hint)", "nested list->concat-list", True],
        "maxsegstrip_noe": ["", "max segment strip", "nested list->concat-list", True],
        "maxsegstrip_full_noe": ["", "max seg. strip (no hint)", "nested list->concat-list", True],
        "nested_minmax_noe": ["", "min-max nested", "nested list->concat-list", True],
        "bal_2": ["", "balanced parens", "list->concat-list", True],
    },
    # All positive
    "constraints/all_positive": {
        "list_mps":     ["All", "mps", "list->concat list", True],
        "sndmin":     ["Elements", "second min", "list->concat list", True],
        "sndmax":     ["Positive", "second max", "list->concat list", True],
    },
    # Alist
    "constraints/alist": {
        "count_eq2":     ["              ", "count mems (v2) ", "list->list", True],
        "count_eq":      ["  Association ", "count mems", "list->list", True],
        "sums":          ["   List       ",  "sum for matching keys", "list->list", True],
        "most_frequent":  ["              ",
                           "most frequent (alist)", "list->list", True],
        "most_frequent_v2":  ["              ",
                              "most frequent (unique keys)", "list->alist", True],
    },
    # Balanced tree
    "constraints/balanced_tree": {
        "node_count": [" Balanced ", "node count", "tree->tree", True],
        "height":        ["  Tree    ", "height", "tree->tree", True],
        "height_v2":   ["          ", "height (v2)", "tree->tree", True],
    },
    # BST
    "constraints/bst": {
        "check_mem_included":  [" ", "check bounds inclusion", "tree->tree", True],
        "check_included_comb":  [" ", "single-pass inclusion", "tree->tree", True],
        "contains":            [" ", "integer contains", "tree->tree", True],
        "check_no_subtree_sum_gt2": [" ", "no subtree sum gt.2", "tree->tree", True],
        "contains_bool_2":            [" ", "boolean contains", "tree->tree", True],
        "count_lt":                ["      ", "count elts. <", "tree->tree", True],
        "contains_v2":            [" Binary ", "int contains w. const case", "tree->tree", True],
        "contains_v3":            [" Search ", "contains rev.", "tree->tree", True],
        "count_between":         [" Tree ", "count between", "tree->tree", True],
        "from_list_contains":    [" ", "int. contains (list)", "list->tree", True],
        "from_list_max":    [" ", "max elt. (list)", "list->tree", True],
        "minmax_separate":    [" ", "minmax (2 traversals)", "tree->tree", True],
        "minmax_sim":    [" ", "minmax (1 traversal)", "tree->tree", True],
        "most_frequent_v1":    [" ", "most frequent", "tree->tree", True],
        "sum_gt_by_key":       [" ", "sum if key larger", "list->tree", True],
        "sum_between": [" ", "sum between", "list->tree", True],
    },
    # Constant list
    "constraints/constantlist": {
        "index_of":  [" Constant ", "index of elt. ", "list->list", True],
        "contains":  ["  List    ", "contains elt  ", "list->list", True],
    },
    # Empty right subtree
    "constraints/empty_right_subtree": {
        "contains":  [" Empty", "contains", "tree->tree", True],
        "sum":  [" Subtree", "sum", "tree->tree",  True],
    },
    # Even list
    "constraints/evenlist":
    {
        "parity_of_first": ["Elements", "parity of 1st", "list->list", True],
        "parity_of_last":  [" are even", "parity of last", "list->list", True],
        "first_odd":        [" numbers  ", "first odd elt.", "list->list", True],
        "parity_of_sum":   ["    ", "parity of sum ", "list->list", True],
    },
    # Even tree
    "constraints/even_tree":
    {
        "parity_of_max": ["  Tree of Even ", "parity of max ", "tree->tree", True],
        "sum_of_parities":  ["  Numbers  ", "parity of sum", "tree->tree", True],
    },


    # Memo
    "constraints/memo": {
        "constant":       ["       ", "has constant", "tree->tree", True],
        "count_lt":       ["       ", "count elts lt", "tree->tree", True],
        "max_contains":       [" Memoizing  ", "contains", "tree->tree", True],
        "max_sum_gt":       [" Information ", "sum elts gt. key", "tree->tree", True],
        "minmax":       ["       ", "minmax", "tree->tree", True],
        "proper_indexation_sum_lt_pos":       ["       ", "sum lt. pos", "list->list", True],
        "proper_indexation_sum_lt_pos_v2":       ["       ", "sum lt. pos (v2)", "list->list", True],
        "tree_size":       ["       ", "size", "tree->tree", True],
    },
    "constraints/size": {
        "obfuscated_length": [" ", "obfuscated length", "list->list", True],
        "obfuscated_length_3": [" ", "obfuscated length (v2)", "list->list", True],
    },
    # Symmetric tree
    "constraints/symmetric_tree": {
        "sum":        [" Symmetric  ", "sum", "tree->tree", True],
        "height":      ["    Tree    ", "height", "tree->tree", True],
        "min":         ["            ", "min", "yes", "tree->tree", True],
    },
    # Sorted
    "constraints/sortedlist": {
        "min":                    ["        ", "min ", "list->list", True],
        "max":                    [" Sorted ", "max ", "list->list", True],
        "count_lt":               [" List   ",
                                   "count elt. smaller ", "list->list", True],
        "index_of":               ["        ", "index of elt ", "list->list", True],
        "is_intersection_empty":  ["        ", "intersection-empty ", "list->list", True],
        "exists_equal_elems":     ["   ", "exists duplicates", "list->list", True],
        "exists_equal_elems_v2":  ["   ", "exists duplicates (v2)", "list->list", True],
        "largest_diff":           ["        ", "largest diff ", "list->list", True],
        "smallest_diff":           ["        ", "smallest diff ", "list->list", True],
        "interval_intersection": ["         ", "interval intersection ", "list->list", True],
        "largest_diff_poslist":   ["        ", "largest diff (pos elts) ", "list->list", True],
        "largest_even":           ["        ", "largest even", "list->list", True],
        "largest_even_pos":       ["        ", "largest even positive", "list->list", True],
        "largest_even_pos_v2":    ["        ", "largest even positive (v2)", "list->list", True],
        "smallest_diff":          ["        ", "smallest diff ",  "list->list", True],
        "parallel_min":          ["        ", "parallel min",  "list->concat list", True],
        "parallel_max":          ["        ", "parallel max", "list->concat list", True],
        "max_point_sum":          ["        ", "par. max of point sum", "list->concat list", True],
        "parallel_max2":          ["        ", "parallel max (v2)", "list->concat list", True],
        "mss":                   ["        ", "mss", "list->concat list", True],
        "slsp":                   ["        ", "sum of longest positive suffix", "list->concat list", True],
        "failing_slspplus":       ["        ", "sum if all positive", "nested list->concat list", True],
        "nestedl_pyramid":       ["        ", "pyramid range", "nested list->nested list", True],
        "sndmind":          ["        ", "second smallest", "list->list", True],
        "sndmax_empty_base_case": ["        ", "second largest", "list->list", True],
        "sndmax_empty_base_case_v2": ["        ", "second smallest (v2)", "list->list", True],
        "sndmax_len2_base_case": ["        ", "second smallest, len2 base case", "list->list", True],
    },
    # Sorted and indexed
    "constraints/sorted_and_indexed": {
        "count_lt0": [" Sorted and ", "count smaller 0", "list->list", True],
        "count_lt":  [" Indexed     ", "count smaller x", "list->list", True],
    },
    # unimodal
    "constraints/unimodal_list": {
        "max_logn_v2": ["Unimodal  ", "max", "list->ulist", True],
        "max_logn": ["List ", "max (v2)", "list->ulist", True],
        "max_in_um_pos": ["    ", "max with pos", "list->ulist", True],
    }
}

unrealizable_show_set = {
    # Indexed list
    "indexed_list": {
        "position_polynomial_no_index":    [" Indexed  ", "value-pos mult.", "list->list", False],
        "search_no_index":                 ["    List  ", "search index", "list->list", False],
        "sum_elts_lt_pos_no_len":          ["", "sum smaller pos.", "list->list", False]
    },
    "list": {
        "atoi_no_fac":                    ["    ", "atoi", "list->concat list", False],
        "is_sorted_no_last":               [" List ", "is sorted", "list->concat list", False],
        "largest_diff_sorted_list_nohead": [" Parallelization ", "largest diff", "list->concat list", False],
        "mps_no_sum":                       ["      ", "mps", "list->concat list", False],
        "poly_no_fac":                     ["      ", "poly", "list->concat list", False],
        "zero_after_one_no":               ["      ", "0 after 1", "list->concat list", False],
    },
    "tailopt": {
        "mps_no_sum":  [" Tail opt. ", "mps (no sum)", "list->concat list", False],
    },
    # Switching tree traversals
    "tree": {
        "gradient":   [" Tree      ", "gradient", "tree->tree", False],
        "mits_nosum": [" Traversal ", "mits", "tree->tree", False],
    },
    "unimodal_lists": {
        "prod_needs_aux": [" Unimodal L.", "product", "list->ulist", False]
    },
    "combine": {
        "mts_and_mps_nosum":  ["Combine Traversals", "Mts+mps (no sum)", "list->concat list", False],
    },
    "unrealizable": {
        "po_sorted": ["          ", "Partial order sorted", "tree->tree", False],
        "balanced_tree_logn_sum": ["          ", "sum ", "tree->tree", False],
        "twosum": ["          ", "two-sum", "list->list", False],
        "minmax": ["          ", "minmax", "list->list", False],
        "most_frequent_noinv": ["  ", "most freq. no invariant", "list->alist", False],
        "count_between": ["          ", "count between", "tree->tree", False],
        "count_between": ["          ", "count between (swap calls)", "tree->tree", False],
        "count_between_swap_ec_no_gr_gl": ["          ", "count between (try 1 intro)", "tree->tree", False],
        "count_between_ec_no_gr_gl": ["          ", "count between (try 2 intro)", "tree->tree", False],
        "count_between_ec_no_gr": ["          ", "count between (try 3 intro)", "tree->tree", False],
        "count_between_wrong_comparison": ["          ", "count between v2", "tree->tree", False],
        "contains": ["          ", "contains", "tree->tree", False],
        "contains2": ["          ", "contains (v2)", "tree->tree", False],
        "simple_parity": ["          ", "parity", "tree->tree", False],
        "largest_even_pos": ["          ", "largest even positive", "list->tree", False],
        "minmax_mistake": ["          ", "minmax (v2)", "list->list", False],
        "swap_game": ["          ", "swapping components", "list->concat list", False],
        "swap_game": ["          ", "swapping, missing call", "list->concat list", False],
        "magic_nesting": ["          ", "forced unknown nesting", "list->list", False],
        "approximately_pareto": ["          ", "pareto approx.", "list->alist", False],
        "eval_psum": ["          ", "partial sum", "program->program", False],
        "common_elt": ["          ", "common elt.", "list*list->list*list", False],
        "interval_intersection": ["          ", "interval intersection", "list->list", False],
        "max_low_table_sum": ["          ", "max bottom strip", "nested list->concat list", False],
        "mps_in_partitioned_list": ["          ", "max b. strip, bad sorting", "nested list->concat list", False],
        "nested_min_sum_max_mts": ["          ", "min max mts", "nested list->concat list", False],
        "nested_min_max_mixed": ["          ", "min max mixed", "nested list->concat list", False],
        "pyramid_maxsort": ["          ", "pyramid sort", "nested list->list", False],
        "lpeak_noimf":     ["          ", "largest peak", "list->concat list", False],
        "memo_max":       ["          ", "height memoizing max", "tree->tree", False],
    }

}

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
    for cat_folder, cat_benchmarks in show_benchmarks.items():
        print(f"{dash:-<80s}-")
        for bench_file, attributes in cat_benchmarks.items():
            bench_path = "benchmarks/" + cat_folder + "/" + bench_file + ".ml"
            print(
                f" {attributes[0]: <19s} | {attributes[1]: <25s} | {bench_path : <30s}")
    print(f"{dash:-<80s}-")
    print(f"{dash:-<80s}-")
    kw_unr = "Unrealizable"
    print(f"{kw_unr: <20s} | {kw_benchmark : <25s} | L | {kw_path : <30s}")
    for cat_folder, cat_benchmarks in unrealizable_show_set.items():
        print(f"{dash:-<80s}-")
        for bench_file, attributes in cat_benchmarks.items():
            bench_path = "benchmarks/" + cat_folder + "/" + bench_file + ".ml"
            print(
                f" {sp: <19s} | {attributes[1]: <25s} | {bench_path : <30s}")


def floti(f):
    ret = "-"
    try:
        ret = float(f)
    except:
        ret = float(timeout_value)
    return ret


def booli(s):
    if s == "True" or s == "#t" or s == "true":
        return True
    else:
        return False


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
