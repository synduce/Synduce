import sys
import datetime

timeout_time = 600.0


caption = "Experimental Results.  Benchmarks are grouped by categories introduced in Section \\ref{sec:cstudies}. \# steps indicates the number of refinement rounds. $T_{last}$ is the elapsed time before the last call to the SyGuS solver in the last refinement step before timeout. All times are in seconds. The best time is highlighted in bold font.  A '-' indicates timeout ($>$ 10 min). The ``Inv'' column indicates if codomain constraints were required. Experiments are run on a laptop with 16G memory and an i7-8750H 6-core CPU at 2.20GHz running Ubuntu 19.10."

caption2 = "Extended Experimental Results.  Three algorithm are compared: the selective bounding CEGIS in {\\tool}, symbolic CEGIS and concrete CEGIS. \
            Benchmarks are grouped by categories introduced in Section \\ref{sec:cstudies}. \# indicates the number of refinement rounds. \
            ver.\% indicates the percentage of total time spent verifying solutions.\
            $T_{last}$ is the elapsed time before the last call to the SyGuS solver in the last refinement step before timeout (`.` indicates that there was no previous round). \
            All times are in seconds. A '-' indicates timeout($>$ 10 min).\
             Experiments are run on a laptop with 16G memory and an i7-8750H 6-core CPU at 2.20GHz running Ubuntu 19.10."

caption_optimizations = "Optimization Evaluation Results: We evaluate {\\tool} and symbolic CEGIS with some optimizations turned off. \
            {\\tt - ini} indicates that optimized initialized is off, {\\tt - sys} indicates tuple components and independent subsystems is turned off, and {\\tt stx} that syntactic definitions are turned off.\
            {\\tt on} (and {\\tt off}) indicate that all optimizations are turned on (resp. off).\
            Benchmarks are grouped by categories introduced in Section \\ref{sec:cstudies}. \
            \# indicates the number of refinement rounds for the given algorithm, and \#i indicate the number of refinement rounds without the optimized initialization.\
            All times are in seconds. A '-' indicates timeout($>$ 10 min).\
             Experiments are run on a laptop with 16G memory and an i7-8750H 6-core CPU at 2.20GHz running Ubuntu 19.10."


algorithms = ["se2gis", "segis", "cegis"]
versions = ["all", "ini", "st", "d", "off"]
fields = ["synt", "verif", "#i", "last"]

show_benchmarks = [
    ["tree", [
        ["sumtree", ["", "sum", "no", "sum"]],
        ["maxtree", ["", "max", "no", "max"]],
        ["maxtree2", ["", "max 2", "no", "max(2)"]],
        ["min", ["Changing", "min", "no", "min"]],
        ["minmax", ["Tree", "min-max", "no", "minmax"]],
        ["maxPathWeight", ["Traversals", "max weighted path", "no", "max w. path"]],
        ["sorted", ["", "sorted in-order", "no", "sorted"]],
        ["poly", ["", "pre-order poly.", "no", "poly"]],
        ["mips", ["", "{\\tt mips}", "yes", "{\\tt mips}"]],
        ["mits", ["", "in-order mts", "yes", "mits"]],
        ["mpps", ["", "post-order mps", "yes", "mpps"]]
    ]],
    ["treepaths", [
        ["sum", ["", "sum", "no", "sum"]],
        ["height", ["From ", "height", "no", "height"]],
        ["maxPathWeight", ["Tree to", "max weighted path", "no", "max w. path"]],
        ["maxPathWeight2", [
            "Path", "max w. path (hom)", "no", "max w. path"]],
        ["leftmostodd", ["", "leftmost odd", "no", "leftmost o."]],
        ["mips", ["", "{\\tt mips}", "yes", "{\\tt mips}"]]
    ]],
    ["tailopt",
     [
         ["sum", ["Enforcing", "sum", "no", "sum"]],
         ["mts", ["Tail", "mts", "no", "mts"]],
         ["mps", ["Recursion", "mps", "no", "mps"]]
     ]],
    ["combine",
     [
         ["mts", ["Combining", "mts + sum", "no", "mts"]],
         ["mts_and_mps", ["Traversals", "sum + mts + mps", "yes", "max. sums"]]
     ]],
    ["ptree", [
        ["sum", ["", "sum", "no", "sum"]],
        ["mul", ["Tree", "product", "no", "mul"]],
        ["maxheads", ["Flattening", "max of heads", "no", "max heads"]],
        ["maxlast", ["", "max of lasts", "no", "max lasts"]],
        ["maxsum", ["", "max sibling sum", "no", "max sib. sum"]]
    ]],

    ["list", [
        ["sumhom", ["", "sum", "no", "sum"]],
        ["sumevens", ["", "sum of even elts.", "no", "sum evens"]],
        ["lenhom", ["", "length", "no", "len"]],
        ["last", ["", "last", "no", "last"]],
        ["prodhom", ["Parallelizing", "product", "no", "product"]],
        ["polyhom", ["Functions", "polynomial", "no", "polynomial"]],
        ["hamming", ["on", "hamming", "no", "hamming"]],
        ["minhom", ["Lists", "min", "no", "minhom"]],
        ["issorted", ["", "is sorted", "no", "sorted"]],
        ["search", ["", "linear search", "no", "search"]],
        ["line_of_sight", ["", "line of sight", "no", "l. of sight"]],
        ["mtshom", ["", "mts", "yes", "mts"]],
        ["mpshom", ["", "mps", "yes", "mps"]],
        ["mts_and_mps_hom", ["", "mts and mps combined", "yes", "mts + mps"]],
        ["msshom", ["", "mss", "yes", "mss"]],
        ["maxcount", ["", "count max elements", "no", "max cnt."]],
    ]]
]

extra_benchmarks = [
    ["tree_to_list", [
        ["search",    ["List", "search", "no", "search"]],
        ["search_v2", ["to", "search (v2)", "no", "search"]],
        ["search_v3", ["Tree", "search (v3)", "no", "search"]],
        # ["mls", ["", "max left sum", "yes", "mls"]],
        # ["mps",       ["", "mps", "no", "mps"]]
    ]]
]


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
    if b == "-":
        return - 1
    else:
        a = floti(a)
        b = floti(b)
        if b > 0:
            return b / a
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
    return "{\\bf %s}" % s


def format_verif(f):
    if f == "?":
        return "?"
    f = float(f)
    if f < 0.009:
        return "$\\sim$\\textit{0.}"
    else:
        return "\\textit{%3.1f}" % f


def incr_avg(old_val, new_count, new_val):
    return (old_val + (new_val - old_val) / new_count)


def produce_tex_table(tex_output_file, data):
    with open(tex_output_file, 'w') as tex:
        tex.write("%s ====================================\n" % '%')
        tex.write(
            "%s This table has been automatically produced by the tool on %s.\n" %
            ('%', str(datetime.datetime.now())))
        tex.write("%s ====================================\n" % '%')
        # open table
        tex.write("\\begin{table}\n")
        tex.write("\t\caption{%s}\label{table:experiments}\n" % caption)
        tex.write("\t{\n")
        tex.write("\t\t\\begin{tabular}[h]{|c|c|c|c|c|c||c|c|c|}\n")
        tex.write("\t\t\t\\hline\n")
        tex.write(
            "\t\t\t \multirow{2}{*}{Class} &\
                 \multirow{2}{*}{Benchmark} & \
                  \multirow{2}{*}{Inv.} & \
                   \multicolumn{3}{c||}{\\tool} & \
                    \multicolumn{3}{c|}{Baseline Method}\\\\ \n")
        tex.write("\t\t\t\\cline{4-9}\n")
        tex.write(
            "\t\t\t &   & & time & \# steps & $T_{last}$ & time & \# steps & $T_{last}$\\\\ \n")
        speedups = []
        for benchmark_class, benchmarks in show_benchmarks:
            if len(benchmarks) > 0:
                tex.write("\t\t\t\\hline\n")
            for benchmark_file, benchmark_info in benchmarks:
                req_t = "?"
                req_iters = "?"
                req_last = "?"
                nai_t = "?"
                nai_iters = "?"
                nai_last = "?"

                bkey = benchmark_class + "/" + benchmark_file + ".pmrs"
                segis_bf = False

                # Pick best optimized version for se2gis
                a_data = data[bkey, "se2gis"]
                versions = ["all", "st", "d", "off"]
                best_v = "all"

                if (bkey, "se2gis") not in data.keys():
                    print("No data for %s, se2gis" % bkey)
                else:
                    b_data = data[bkey, "se2gis"][best_v]
                    if "res" in b_data:
                        req_iters, _, req_time, _ = b_data["res"]
                        req_t = "%3.2f" % float(req_time)
                    else:
                        req_t = "-"
                        req_iters = b_data["max"]
                    if str(b_data["max"]) in b_data:
                        req_last = "%3.2f" % b_data[str(b_data["max"])][1]

                # Pick best optimized version for segis
                a_data = data[bkey, "segis"]
                versions = ["all", "st", "d", "off"]
                best_v = "all"
                best_t = timeout_time
                for version in versions:
                    if version in a_data.keys():
                        b_data = a_data[version]
                        if "res" in b_data:
                            _, _, this_time, _ = b_data["res"]
                            this_t = float(this_time)
                            if this_t < best_t:
                                best_t = this_t
                                best_v = version

                if (bkey, "segis") not in data.keys():
                    print("No data for %s, segis" % bkey)

                else:
                    b_data = data[bkey, "segis"][best_v]
                    if "res" in b_data:
                        nai_iters, _, nai_time, _ = b_data["res"]
                        nai_t = "%3.2f" % float(nai_time)
                        if req_t == "-" or float(nai_time) < float(req_t):
                            segis_bf = True
                    else:
                        nai_t = "-"
                        nai_iters = b_data["max"]
                    if str(b_data["max"]) in b_data:
                        nai_last = "%3.2f" % b_data[str(b_data["max"])][1]

                speedups += [[(benchmark_class, benchmark_info[1]),
                              speedup(req_t, nai_t)]]

                if segis_bf:
                    nai_t = "{\\bfseries %s}" % nai_t
                else:
                    req_t = "{\\bfseries %s}" % req_t

                tex.write("\t\t\t%s & %s & %s & %s & %s & %s & %s & %s & %s\\\\ \n" % (
                    benchmark_info[0], benchmark_info[1], benchmark_info[2],
                    req_t, req_iters, req_last, nai_t, nai_iters, nai_last))

            # close table
        tex.write("\t\t\t\\hline\n")
        tex.write("\t\t\end{tabular}\n")
        tex.write("\t}\n")
        tex.write("\end{table}\n")
        tex.close()
        print("Num benchmarks: %i" % len(speedups))
        print("==================================")
        count_timeouts = 0
        count_100x = 0
        count_10x = 0
        speedup_only = []
        for b, s in speedups:
            print("Speedup %10s,%20s: %4.1f" % (b[0], b[1], s))
            if s < 0:
                count_timeouts += 1
            else:
                speedup_only += [s]
            if s > 50.1:
                count_100x += 1
            if s > 5.1:
                count_10x += 1
        print("==================================")
        print("Num timeouts: %i" % count_timeouts)
        print("Num > 100x: %i" % count_100x)
        print("Num > 10x: %i" % count_10x)
        print("Average: %3.4f" % mean(speedup_only))
        print("Median: %3.4f" % median(speedup_only))


def produce_full_tex_table(tex_output_file, data):
    tex = open(tex_output_file, 'w')
    tex.write("%s ====================================\n" % '%')
    tex.write(
        "%s This table has been automatically produced by the tool on %s.\n" %
        ('%', str(datetime.datetime.now())))
    tex.write("%s ====================================\n" % '%')
    # open table
    tex.write("\\begin{table}\n")
    tex.write("\t\caption{%s}\label{table:experiments_full}\n" % caption2)
    tex.write("\t{\n")
    tex.write("\t\t\\begin{tabular}[h]{|c| c|c|c || c|c|c || c|c|c|}\n")
    tex.write("\t\t\t\\hline\n")
    tex.write(
        "\t\t\t \multirow{2}{*}{Benchmark} & \
                \multicolumn{3}{c||}{\\tool} & \
                \multicolumn{3}{c||}{Symbolic CEGIS} & \
                \multicolumn{3}{c|}{Concrete CEGIS}\\\\ \n")
    tex.write("\t\t\t\\cline{2-10}\n")
    tex.write(
        "\t\t\t  &\
        time & ver. \%  & \# &\
        time & ver. \%  & \# &\
        time & ver. \%  & \#\\\\ \n")

    ver = "all"
    for benchmark_class, benchmarks in show_benchmarks + extra_benchmarks:
        if len(benchmarks) > 0:
            tex.write("\t\t\t\\hline\n")
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
            t_segis = floti(csvline[4])
            t_cegis = floti(csvline[8])
            if t_Synduce <= t_segis and t_Synduce <= t_cegis:
                csvline[0] = "{\\bf %s}" % csvline[0]
            if t_segis < t_Synduce and t_segis < t_cegis:
                csvline[4] = "{\\bf %s}" % csvline[4]
            if t_cegis < t_Synduce and t_cegis < t_segis:
                csvline[8] = "{\\bf %s}" % csvline[8]
            # Put percentage in italic
            csvline[1] = format_verif(csvline[1])
            csvline[5] = format_verif(csvline[5])
            csvline[9] = format_verif(csvline[9])
            # Remove T_last
            del csvline[3]
            del csvline[6]
            del csvline[9]

            tex.write("%s & %s \\\\ \n" % (
                benchmark_info[1], "& ".join(csvline)))

    tex.write("\t\t\t\\hline\n")
    tex.write("\t\t\end{tabular}\n")
    tex.write("\t}\n")
    tex.write("\end{table}\n")
    tex.close()


def produce_versions_tex_table(tex_output_file, data):
    tex = open(tex_output_file, 'w')
    tex.write("%s ====================================\n" % '%')
    tex.write(
        "%s This table has been automatically produced by the tool on %s.\n" %
        ('%', str(datetime.datetime.now())))
    tex.write("%s ====================================\n" % '%')
    # open table
    tex.write("\\begin{table}\n")
    tex.write(
        "\t\caption{%s}\label{table:experiment_optimizations}\n" % caption_optimizations)
    tex.write("\t{\n")
    tex.write(
        "\t\t \\hspace{-3em}\
        \\begin{tabular}[h]{|c| c| c|c|c|c|c|c || c| c|c|c|c| }\n")
    tex.write("\t\t\t\\hline\n")
    tex.write(
        "\t\t\t \multirow{2}{*}{Benchmark} & \
                \multicolumn{7}{c||}{\\tool} & \
                \multicolumn{5}{c|}{Symbolic CEGIS} \\\\ \n")
    tex.write("\t\t\t\\cline{2-13}\n")
    tex.write(
        "\t\t\t  &\
         \# & {\\tt on} & \#i & {\\tt -ini} & {\\tt -sys} & {\\tt -stx} & {\\tt off} &\
         \# & {\\tt on} & {\\tt -sys} & {\\tt -stx} & {\\tt off} \\\\ \n")

    ver = "all"
    for benchmark_class, benchmarks in show_benchmarks + extra_benchmarks:
        if len(benchmarks) > 0:
            tex.write("\t\t\t\\hline\n")
        for benchmark_file, benchmark_info in benchmarks:
            bkey = benchmark_class + "/" + benchmark_file + ".pmrs"
            # Collect Data for se2gis
            algo = "se2gis"
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
            algo = "segis"
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
            cegis_csvline[indexmin] = bold(cegis_csvline[indexmin])

            tex.write("%s & %s & %s \\\\ \n" % (
                benchmark_info[1], "& ".join(req_csvline), "& ".join(cegis_csvline)))

    tex.write("\t\t\t\\hline\n")
    tex.write("\t\t\end{tabular}\n")
    tex.write("\t}\n")
    tex.write("\end{table}\n")
    tex.close()


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
    bkey = benchmark_class + "/" + benchmark_file + ".pmrs"
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
                inst = s.strip().strip("B:").split(",")
                benchfile, method = inst[0], inst[1].strip(" ").split("+")
                method_base = method[0]
                method_opt = "all"
                if len(method) > 1 and method[1] != "":
                    method_opt = method[1]
                key = benchfile, method_base, method_opt
                if key not in benchmark_data:
                    benchmark_data[key] = {}
                if "max" not in benchmark_data[key]:
                    benchmark_data[key]["max"] = 0
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
                    # Res = #of refinement steps, verif. time, total time.
                    if "res" in benchmark_data[key]:
                        old_res = benchmark_data[key]["res"]
                        old_count = old_res[3]
                        new_steps = max(old_res[0], int(infos[0]))
                        new_verif = incr_avg(
                            old_res[1], old_count, float((infos[1])))
                        new_time = incr_avg(
                            old_res[2], old_count, float(infos[2]))
                        new_count = old_count + 1
                        benchmark_data[key]["res"] = new_steps, new_verif, new_time, new_count
                    else:
                        benchmark_data[key]["res"] = int(
                            infos[0]), float(infos[1]), float(infos[2]), 1

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
    # input_file = "benchmarks/bench.txt"
    # output_file = "benchmarks/results.csv"

    if len(sys.argv) < 4:
        print(
            "Usage: python3 report.py INPUT_FILE OUTPUT_CSV TABLE# [TEX_OUTPUT]\n\n\
                    INPUT_FILE   the input file produce by running test.py\n\
                    OUTPUT_FILE  the csv to store temporary results\n\
                    TABLE#       1-3 to produce Table 1, 2 or 3.\n\
                    TEX_OUTPUT   If 0 < TABLE# <= 3, provide .tex output file.\n\
            ")
        exit()

    input_file = sys.argv[1]
    output_file = sys.argv[2]
    table_no = int(sys.argv[3])

    if table_no > 0 and table_no < 4:
        if len(sys.argv) < 5:
            print(
                "Please provide output .tex file after table number.")

    thedict = raw_to_csv(input_file)

    csv_table(thedict, output_file)

    if table_no == 1:
        tex_out = sys.argv[4]
        produce_tex_table(tex_out, thedict)

    if table_no == 2:
        tex_out = sys.argv[4]
        produce_versions_tex_table(tex_out, thedict)

    if table_no == 3:
        tex_out = sys.argv[4]
        produce_full_tex_table(tex_out, thedict)
