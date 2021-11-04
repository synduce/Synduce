import sys

# Timeout for all experiments.
timeout_value = 700

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

constraint_benchmarks = [
    # Ensures
    ["constraints/ensures/mps_no_ensures.ml", "-B"],
    ["constraints/ensures/mts_no_ensures.ml", ""],
    ["constraints/ensures/mss_no_ensures.ml", "-B"],

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
    # constantlist
    ["constraints/constantlist/index_of.ml", ""],
    ["constraints/constantlist/contains.ml", ""],
    # all positive
    ["constraints/all_positive/list_mps.ml", ""],
    ["constraints/all_positive/sndmin.ml", "-NB"],
    # ["constraints/all_positive/sndmax.ml", "-NB"], Unsafe for now, sometimes takes 10min
    # evenlist
    ["constraints/evenlist/parity_of_first.ml", ""],
    ["constraints/evenlist/parity_of_last.ml", ""],
    ["constraints/evenlist/first_odd.ml", ""],
    ["constraints/evenlist/parity_of_sum.ml", ""],
    # bst
    ["constraints/bst/contains.ml", ""],
    ["constraints/bst/count_lt.ml", "-NB"],
    ["constraints/bst/count_between.ml", "-NB --no-gropt"],
    ["constraints/bst/most_frequent_v1.ml", ""],
    ["constraints/bst/from_list_contains.ml", ""],
    ["constraints/bst/from_list_max.ml", "-NB -n 100"],
    ["constraints/bst/sum_gt_by_key.ml", "-NB -n 100"],
    # balanced_tree
    ["constraints/balanced_tree/node_count.ml", "-N"],
    ["constraints/balanced_tree/height.ml", "-N"],
    # ["constraints/balanced_tree/height_v2.ml", "-NB"],
    # symmetric tree
    ["constraints/symmetric_tree/sum.ml", "-N"],
    ["constraints/symmetric_tree/height.ml", "-N"],
    ["constraints/symmetric_tree/min.ml", "-N"],
    # memo
    ["constraints/memo/tree_size.ml", "-NB"],
    ["constraints/memo/constant.ml", ""],
    ["constraints/memo/mts_memo_sum.ml", ""],
    ["constraints/memo/max_contains.ml", "-NB"],
    ["constraints/memo/count_lt.ml", "-NB -n 50"],
    ["constraints/memo/max_sum_gt.ml", "-NB"],
    ["constraints/memo/proper_indexation_sum_lt_pos_v2.ml", ""],
    ["constraints/memo/proper_indexation_sum_lt_pos.ml", ""],
    # empty_right
    ["constraints/empty_right_subtree/contains.ml", "-N"],
    # alist
    ["constraints/alist/count_eq2.ml", "-NB"],
    ["constraints/alist/count_eq.ml", ""],
    ["constraints/alist/sums.ml", ""],
    ["constraints/alist/most_frequent.ml", ""],
    # even_tree
    ["constraints/even_tree/sum_of_parities.ml", "-NB"],
    ["constraints/even_tree/parity_of_max.ml", ""],
    # program
    #["constraints/program/typecheck.ml", ""],
    # unimodal
    ["constraints/unimodal_list/max_logn.ml", "-B"],
    ["constraints/unimodal_list/max_logn_v2.ml", "-kNB"],
    ["constraints/unimodal_list/max_in_um_pos.ml", ""],
    # Size constraint
    ["constraints/size/obfuscated_length.ml", ""]
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
    ["list/line_of_sight.pmrs", ""],
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
    ["unimodal_lists/sm.ml", ""],
    ["unimodal_lists/search_no_optim.ml", ""],
    # Zippers
    ["zippers/list_sum.ml", ""],
    ["zippers/list_sum_basic.ml", ""],
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
    # Switching tree traversals
    ["tree/gradient.ml", ""],
    ["tree/mits_nosum.ml", ""],
    # Unimodal lists (might also be due to representation)
    ["unimodal_lists/prod_needs_aux.ml", ""]
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
