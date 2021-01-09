import sys


def all_timeout(l):
    is_t = True
    for elt in l:
        is_t = is_t and (elt == ["TIMEOUT"])
    return is_t


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
                    result = lines[i].strip().split(",")
                    benchfile, method = inst[0], inst[1].strip(" ")
                    if len(result) == 2:
                        try:
                            iterations, time = int(result[0]), float(result[1])
                            benchmark_data.setdefault(
                                (benchfile, method), []).append([iterations, time])
                        except:
                            benchmark_data.setdefault(
                                (benchfile, method), []).append(["TIMEOUT"])
                            i = i - 1

                    else:  # this is a TIMEOUT
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

    with open("benchmarks/bench.csv", 'w') as outf:
        for a, b, c, d, e in results:
            outf.write("%s,%s,%s,%s,%s\n" % (a, b, c, d, e))
