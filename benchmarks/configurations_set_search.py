
incomplete_benchmarks_set0 = [
    ["incomplete/sortedlist/count_lt_p1.ml", "", True, 1],
    ["incomplete/list/sum_p1.ml", "", True, 1],
    ["incomplete/list/sorted_min.ml", "", True, 1],
    ["incomplete/list/swap_game.ml", "", False, 1],
    ["incomplete/common_elt.ml", "", False, 1],
    ["incomplete/bst/count_lt_partial_2.ml", "", True, 1],
    ["constraints/bst/contains.ml", "-NB", True, 1],
    ["constraints/bst/contains_bool_2.ml", "", True, 1],
    ["constraints/bst/contains_no_ensures.ml", "", True, 1],
    ["constraints/bst/from_list_contains.ml", "", True, 1],
    ["constraints/bst/from_list_max.ml", "-NB -n 50", True, 1],
    ["constraints/bst/most_frequent_v1.ml", "", True, 1],
    ["list/sum.ml", "", True, 1],
    ["list/mps.ml", "-NB", True, 1],
    ["list/mss.ml", "-NB", True, 1],
    ["list/mts.ml", "-NB", True, 1],
    ["list/alist_sum.ml", "", True, 1],
    ["list/atoi.ml", "", True, 1],
    ["list/bal.ml", "", True, 1],
    ["list/block_ones_extremas.ml", "-n 20", True, 1],
    ["list/hamming.ml", "-NB", True, 1],
    ["list/last.ml", "", True, 1],
    ["list/len.ml", "-n 1000", True, 1],
    ["list/line_of_sight.ml", "", True, 1],
    ["list/maxcount.ml", "", True, 1],
    ["list/max_block_ones.ml", "", True, 1],
    ["list/max.ml", "", True, 1],
    ["list/min.ml", "", True, 1],
    ["list/mincount.ml", "", True, 1],
    ["list/mts_and_mps_hom.pmrs", "", True, 1],
    ["list/poly.ml", "", True, 1],
    ["list/prod.ml", "", True, 1],
    ["list/search.pmrs", "", True, 1],
    ["list/slsp.ml", "", True, 1],
    ["list/sndmax.ml", "", True, 1],
    ["list/sndmin.ml", "", True, 1],
    ["list/sumgt.ml", "", True, 1],
    ["list/sumodds.ml", "", True, 1],
    ["list/sumevens.ml", "", True, 1],
    ["list/zero_after_one.ml", "", True, 1],
    ["list/zeros_ones.ml", "", True, 1],
    ["constraints/sortedlist/slsp.ml", True, 1],
    ["tree/maxPathWeight.pmrs", "", True, 1],
    ["tree/max.ml", "", True, 1],
    ["tree/max2.pmrs", "", True, 1],
    ["tree/min.ml", "", True, 1],
    ["tree/minmax.ml", "", True, 1],
    ["tree/mips.ml", "", True, 1],
    ["tree/mits.ml", "", True, 1],
    ["tree/mpps.ml", "--no-gropt", True, 1],
    ["tree/poly.ml", "", True, 1],
    ["tree/poly2.ml", "", True, 1],
    ["tree/sorted.ml", "--no-detupling", True, 1],
    ["tree/sorted_2.ml", "", True, 1],
    ["tree/sum.ml", "", True, 1],
]

incomplete_benchmarks_set1 = [
    ["constraints/bst/check_mem_included.ml", "", True, 1],
    ["constraints/bst/check_included_comb.ml", "", True, 1],
    ["constraints/bst/check_no_subtree_sum_gt2.ml", "", True, 1],
    ["constraints/bst/sum_gt_by_key.ml", "", True, 1],
    ["constraints/bst/sum_between.ml", "", True, 1],
    ["incomplete/bst/count_between.ml", "-n 20", True, 1],
    ["constraints/sortedlist/index_of.ml", "", True, 1],
    ["constraints/sortedlist/exists_equal_elems.ml", "", True, 1]
]

incomplete_benchmarks = incomplete_benchmarks_set0 + incomplete_benchmarks_set1
