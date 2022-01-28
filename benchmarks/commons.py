from definitions import *


def caption(exp_setup):
    return "Experimental Results for Realizable Benchmarks.  Benchmarks are grouped by categories introduced in Section \\ref{sec:evaluation}. All times are in seconds. The best time is highlighted in bold font.  A '-' indicates timeout ($>$ 400s). The ``I'' column indicates whether intermediate lemmas were proven by induction. Steps is a sequence of '$\\bullet$' (refinement) and '$\\circ$' (coarsening). Experiments are run on %s." % exp_setup


def caption_unrealizable(exp_setup):
    return "Experimental Results for Unrealizable Benchmarks. All synthesis times are in seconds. The best time is highlighted in bold font.  A '-' indicates timeout ($>$ 400s). The ``I'' column indicates whether intermediate lemmas were proven by induction. Steps is a sequence of '$\\bullet$' (refinement) and '$\\circ$' (coarsening). Experiments are run on %s." % exp_setup


def timefix(x):
    if x in timeout_names:
        return "-"
    else:
        return x


def roundfix(s):
    if s == "None":
        s = "-"
    else:
        s = s[:-1]
    s = s.replace("^", "l")
    s = s.replace("+", "\\bullet")
    s = s.replace(".", "\\circ")
    return s


def unrealizable(s):
    return s.endswith("∅")


def roundcount(s):

    if s.startswith("f"):
        return s[1:]

    if s == "None":
        s = "-"
    else:
        s = s[:-1]
    return str(len(s))


def empty_exp_data(info, realizable=True):
    #        category   benchmark NB time  ref. time  ref.
    if realizable:
        res = f"{info[0]}&{info[1]}& ? & ?   & ?  & ?  & ? & ? & ? \\\\ %chktex 26\n"
    else:
        res = f"{info[1]}& ? & ?   & ?  & ?  & ? & ? & ?  \\\\ %chktex 26\n"
    return res


def with_exp_data(info, data, data2, data3, realizable=True):
    #    se2gis_result = {
    #                 "time": info[2],
    #                 "delta": info[3],
    #                 "rounds": info[4],
    #                 "N": info[5],
    #                 "B": info[6],
    #                 "verif": info[7],
    #             }
    if data['B'] == "✓":
        by_induction = "y"
    else:
        by_induction = "n"

    rounds = f"${roundfix(data['rounds'])}$"
    rounds2 = roundcount(data2['rounds'])
    if data3 is not None:
        rounds3 = roundcount(data2['rounds'])
        time3 = timefix(data3['time'])
    else:
        rounds3 = "?"
        time3 = "?"

    time1 = timefix(data['time'])
    time2 = timefix(data2['time'])

    if floti(time1) < floti(time2) and floti(time1) < floti(time3):
        time1 = "{\\bf" + time1 + "}"
    elif floti(time2) < floti(time3):
        time2 = "{\\bf" + time2 + "}"
    elif time3 not in timeout_names:
        time3 = "{\\bf" + time3 + "}"

    if realizable:
        res = f"{info[0]} & {info[1]} & { by_induction } & {time1} & {rounds} & {time2}  & {rounds2} & {time3} & {rounds3} \\\\ \n"
    else:
        res = f"{info[1]} & { by_induction } & {time1} & {rounds} & {time2}  & {rounds2} \\\\ \n"
    return res


def parse_alg(info, i):
    return {
        "time": info[i+2],
        "delta": info[i+3],
        "rounds": info[i+4],
        "N": info[i+5],
        "B": info[i+6],
        "verif": info[i+7],
    }


def parse_alg_v2(info, i):
    return {
        "time": info[i + 2],
        "last_elapsed": info[i+3],
        "delta": info[i+4],
        "rounds": info[i+5],
        "N": info[i+6],
        "B": info[i+7],
        "verif": info[i+8],
    }


def parse_block(info, i):
    fields = [
        ("time", True),
        ("last_elapsed", True),
        ("delta", True),
        ("rounds", False),
        ("N", False),
        ("B", False),
        ("verif", True)]

    try:
        block = {}
        m = info[i].split(":")
        if len(m) != 2:
            return None
        algo = m[0]  # name of the algorithm
        option = m[1]  # name of the option (ablation study)
        j = 1  # Number of cells read
        while j <= 7:
            if len(info) > i + j:
                if fields[j-1][1]:
                    block[fields[j-1][0]] = floti(info[i + j])
                else:
                    block[fields[j-1][0]] = info[i + j]
            else:
                return j, algo, option, block
            j += 1
        return j, algo, option, block

    except:
        return 0, None, None, None


def repair(info):
    def look_and_insert(i):
        if info[i].startswith("N/A") and info[i].strip() != "N/A":
            splits = info[i].split(" ")
            if len(splits) > 1:
                last_step_time = splits[1].strip()
                info[i] = "N/A"
                info.insert(i+1, last_step_time)
                return True
            else:
                return False
        return False

    if(len(info)) != 22 or len(info) != 25:
        if look_and_insert(10):
            look_and_insert(18)
        else:
            look_and_insert(17)

    return info
