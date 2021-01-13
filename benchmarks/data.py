import sys
import datetime

caption = "Benchmarks. A '-' indicates that synthesis timed out ($>$ 10min)."


def all_timeout(l):
    is_t = True
    for elt in l:
        is_t = is_t and (elt == ["TIMEOUT"])
    return is_t


def produce_tex_table(tex_output_file, data):
    show_benchmarks = [
        ["tailopt",
         [
             ["sum", ["Tail", "sum", "no"]],
             ["mts", ["Recursive", "mts", "no"]],
             ["mps", ["", "mps", "no"]]
         ]],
        ["combine",
         [
             ["mts", ["Combine", "mts", "no"]],
             ["mts_and_mps", ["", "mts + mps", "no"]]
         ]],
        ["zippers", [
            ["sum", ["", "sum", "no"]],
            ["height", ["Tree to", "height", "no"]],
            ["maxPathWeight", ["Zipper", "max weighted path", "no"]],
            ["maxPathWeight2", ["", "max w. path (hom)", "no"]],
            ["leftmostodd", ["", "leftmost odd", "no"]]
        ]],
        ["ptree", [
            ["sum", ["", "sum", "no"]],
            ["mul", ["Flat", "product", "no"]],
            ["maxheads", ["Tree", "max of heads", "no"]],
            ["maxlast", ["", "max of lasts", "no"]],
            ["maxsum", ["", "max sibling sum", "no"]]
        ]],
        ["tree", [
            ["sumtree", ["", "sum", "no"]],
            ["maxtree", ["", "max", "no"]],
            ["maxtree2", ["Change", "max 2", "no"]],
            ["min", ["Tree", "min", "no"]],
            ["minmax", ["Tree", "minmax", "no"]],
            ["maxPathWeight", ["Traversal", "max weighted path", "no"]],
            ["sorted", ["", "sorted in-order", "no"]],
            ["mips", ["", "in-order mps", "yes"]],
            ["mits", ["", "in-order mts", "yes"]],
            ["mpps", ["", "post-order mps", "yes"]]
        ]],
        ["list", [
            ["sumhom", ["", "sum", "no"]],
            ["lenhom", ["", "length", "no"]],
            ["prodhom", ["Parallelize", "product", "no"]],
            ["polyhom", ["List", "polynomial", "no"]],
            ["hamming", ["Functions", "hamming", "no"]],
            ["maxhom", ["", "max", "no"]],
            ["minhom", ["", "min", "no"]],
            ["issorted", ["", "is sorted", "no"]],
            ["search", ["", "lin. search", "no"]],
            ["line_of_sight", ["", "line of sight", "no"]],
            ["mtshom", ["", "mts", "yes"]],
            ["mpshom", ["", "mps", "yes"]],
            ["mts_and_mps_hom", ["", "mts + mps", "yes"]],
            ["msshom", ["", "mss", "yes"]]
        ]]

    ]
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
        tex.write("\t\t\\begin{tabular}[h]{|c|c|c|c|c|c|c|c|}\n")
        tex.write("\t\t\t\\hline\n")
        tex.write(
            "\t\t\t &   & Inv. & \\tool & \# steps & Naive & \# steps & $T_{last}$\\\\ \n")
        for benchmark_class, benchmarks in show_benchmarks:
            if len(benchmarks) > 0:
                tex.write("\t\t\t\\hline\n")
            for benchmark_file, benchmark_info in benchmarks:
                req_t = "?"
                req_iters = "?"
                nai_t = "?"
                nai_iters = "?"
                nai_last = "?"

                bkey = benchmark_class + "/" + benchmark_file + ".pmrs"

                if (bkey, "requation") not in data.keys():
                    print("No data for %s, requation" % bkey)
                else:
                    b_data = data[bkey, "requation"]
                    if "res" in b_data:
                        req_iters, req_time = b_data["res"]
                        req_t = "%3.2f" % float(req_time)
                    else:
                        req_t = "-"
                        req_iters = b_data["max"]

                if (bkey, "naive") not in data.keys():
                    print("No data for %s, naive" % bkey)

                else:
                    b_data = data[bkey, "naive"]
                    if "res" in b_data:
                        nai_iters, nai_time = b_data["res"]
                        nai_t = "%3.2f" % float(nai_time)
                    else:
                        nai_t = "-"
                        nai_iters = b_data["max"]
                    if str(b_data["max"]) in b_data:
                        nai_last = "%3.2f" % b_data[str(b_data["max"])][0]

                tex.write("\t\t\t%s & %s & %s & %s & %s & %s & %s & %s\\\\ \n" % (
                    benchmark_info[0], benchmark_info[1], benchmark_info[2],
                    req_t, req_iters, nai_t, nai_iters, nai_last))

            # close table
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


def raw_to_csv():
    benchmark_data = {}
    with open("benchmarks/bench.txt") as raw:
        lines = raw.readlines()
        num_lines = len(lines)
        benchfile, method = "none", "none"
        for i in range(num_lines):
            infos = ""
            s = lines[i]
            if s.startswith("B:"):
                inst = s.strip().strip("B:").split(",")
                benchfile, method = inst[0], inst[1].strip(" ")
                benchmark_data[benchfile, method] = {}
                benchmark_data[benchfile, method]["max"] = 0
            else:
                infos = lines[i].strip().split(",")

                if len(infos) == 4:
                    step, time, tnum, unum = infos[0], float(
                        infos[1]), int(infos[2]), int(infos[3])
                    benchmark_data[benchfile, method][step] = time, tnum, unum
                    benchmark_data[benchfile, method]["max"] = max(
                        benchmark_data[benchfile, method]["max"], int(step))

                if len(infos) == 2:
                    benchmark_data[benchfile, method]["res"] = int(
                        infos[0]), float(infos[1])

    return benchmark_data


def benchsort(x):
    return x[0] + x[1] + x[2]


if __name__ == "__main__":
    thedict = raw_to_csv()
    results = []
    for k, v in thedict.items():
        print(k, v)
        k0 = k[0].split("/")
        subfolder = k0[0].strip()
        benchmark = k0[1].split(".")[0].strip()
        if "res" in v.keys():
            iters = v["res"][0]
            time = v["res"][1]
            results.append([subfolder, benchmark, k[1], iters, time])

    results.sort(key=benchsort)

    with open("benchmarks/results.csv", 'w') as outf:
        for a, b, c, d, e in results:
            outf.write("%s,%s,%s,%s,%s\n" % (a, b, c, d, e))

    if len(sys.argv) > 1:
        tex_out = sys.argv[1]
        produce_tex_table(tex_out, thedict)
