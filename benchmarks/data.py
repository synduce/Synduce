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
            ["mintree", ["Tree", "min", "no"]],
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
        tex.write("\t\t\\begin{tabular}[h]{|c|c|c|c|c|c|}\n")
        tex.write("\t\t\t\\hline\n")
        tex.write(
            "\t\t\t &   & Inv. & \\# steps & \\tool  & Naive \\\\ \n")
        for benchmark_class, benchmarks in show_benchmarks:
            if len(benchmarks) > 0:
                tex.write("\t\t\t\\hline\n")
            for benchmark_file, benchmark_info in benchmarks:
                req_iters = 0
                req_time = 0.0
                nai_iters = 0
                nai_time = 0.0

                if (benchmark_class, benchmark_file, "requation") not in data.keys():
                    print("No data for %s, %s, requation" %
                          (benchmark_class, benchmark_file))
                else:
                    req_iters, req_time = data[benchmark_class,
                                               benchmark_file, "requation"]

                if (benchmark_class, benchmark_file, "naive") not in data.keys():
                    print("No data for %s, %s, naive" %
                          (benchmark_class, benchmark_file))

                else:
                    nai_iters, nai_time = data[benchmark_class,
                                               benchmark_file, "naive"]

                if req_time < 0:
                    req_t = "-"
                    req_iters = "-"
                elif req_time == 0.0:
                    req_t = "?"
                    req_iters = "?"
                else:
                    req_t = "%3.2f" % req_time
                    req_iters = "%i" % req_iters

                if nai_time < 0:
                    nai_t = "-"
                    nai_iters = "-"
                elif nai_time == 0.0:
                    nai_t = "?"
                    nai_iters = "?"
                else:
                    nai_t = "%3.2f" % nai_time
                    nai_iters = "%i" % nai_iters

                tex.write("\t\t\t%s & %s & %s & %s & %s & %s\\\\ \n" % (
                    benchmark_info[0], benchmark_info[1], benchmark_info[2],
                    req_iters, req_t, nai_t))

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
        i = 0
        while i < num_lines:
            s = lines[i]
            if s.startswith("B:"):
                inst = lines[i].strip().strip("B:").split(",")
                if len(inst) == 2:
                    i = i + 1
                    if i < num_lines:
                        result = lines[i].strip().split(",")
                        benchfile, method = inst[0], inst[1].strip(" ")
                        if len(result) == 2:
                            try:
                                iterations, time = int(
                                    result[0]), float(result[1])
                                benchmark_data.setdefault(
                                    (benchfile, method), []).append([iterations, time])
                            except:
                                benchmark_data.setdefault(
                                    (benchfile, method), []).append(["TIMEOUT"])
                                i = i - 1

                        else:  # this is a TIMEOUT
                            benchmark_data.setdefault(
                                (benchfile, method), []).append(["TIMEOUT"])
                    else:
                        benchmark_data.setdefault(
                            (benchfile, method), []).append(["TIMEOUT"])
            i = i + 1

    averaged = {}
    for k, v in benchmark_data.items():
        if all_timeout(v):
            averaged[k] = -1, -1
        else:
            averaged[k] = means(v)
    return averaged


def benchsort(x):
    return x[0] + x[1] + x[2]


if __name__ == "__main__":
    thedict = raw_to_csv()
    results = []
    for k, v in thedict.items():
        k0 = k[0].split("/")
        subfolder = k0[0].strip()
        benchmark = k0[1].split(".")[0].strip()
        results.append([subfolder, benchmark, k[1], v[0], v[1]])

    results.sort(key=benchsort)

    with open("benchmarks/results.csv", 'w') as outf:
        for a, b, c, d, e in results:
            outf.write("%s,%s,%s,%s,%s\n" % (a, b, c, d, e))

    if len(sys.argv) > 1:
        tex_out = sys.argv[1]
        data = {}
        for a, b, c, d, e in results:
            data[a, b, c] = d, e

        produce_tex_table(tex_out, data)
