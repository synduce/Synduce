import sys
import subprocess
import json
#
from definitions import *
from parsing import MultiDataObj


def run_one(progress, bench_id, command, algo, optim, filename, extra_opt, expect_many):
    print(f"{progress : >11s}  {bench_id} 🏃", end="\r")
    sys.stdout.flush()
    process = subprocess.Popen(
        command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    print(command, end="\n")
    info = None
    last_refinement_string = ""
    last_verif_time = 0.0
    last_elapsed = 0.0
    last_info = None
    # Poll process for new output until finished
    while True:
        try:
            line = process.stdout.readline()
            data = json.loads(line)
            info = MultiDataObj(data)
            last_verif_time = info.verif_elapsed
            last_elapsed = info.elapsed
            last_info = info
        except Exception as e:
            info = MultiDataObj({})
            info.is_successful = False
            info.verif_elapsed = last_verif_time
            info.elapsed = last_elapsed
            break

        if process.poll() is not None or info.is_successful:
            break
        print(
            f"{progress : >11s}.. benchmarks/{filename} {extra_opt} {algo[1]} {optim[1]} 🏃 at step {info.major_step_count(0)}:{info.minor_step_count(0)}", end="\r")

    if info is not None and last_info is None:
        last_info = info
    return last_info


def success_msg(progress, info, elapsed, bench_name, num_runs, delta, timing, verif_elapsed):
    current_algo = info.algo(0)
    if info.is_proved_by_induction(0):
        p_by_induction = "✓"
    else:
        p_by_induction = "~"

    if info.is_classified_by_induction(0):
        c_by_induction = "✓"
    else:
        c_by_induction = "~"

    refinement_rounds = info.get_refinement_summary(0)
    if "." in refinement_rounds:
        induction_info = f"B:{c_by_induction},B':{p_by_induction}"
    else:
        induction_info = "        "

    msg = f"{progress : >11s} ✅ {current_algo: <6s} : {bench_name : <33s} ×{num_runs} runs, {str(timing): <30s} {induction_info} | R: {refinement_rounds} {sp : <20s} "
    print(msg)

    return f"{elapsed: 4.3f}, N/A, {delta : .0f},{refinement_rounds},{c_by_induction},{p_by_induction},{verif_elapsed : 4.3f}"


def run_n(progress, bench_id, command, filename, realizable=True, expect_many=False, algo="se2gis",
          optim="", extra_opt="", errors=[], num_runs=1, csv_output=None):
    total_elapsed = 0.0
    total_last_step_elapsed = 0.0
    verif_elapsed = 0.0
    max_e = 0
    min_e = timeout_value
    estim = 0
    delta = 0
    sp = " "
    bench_parts = bench_id.split(".")[0].split("/")
    bench_name = bench_parts[-1]
    info = MultiDataObj({})
    info.is_successful = False
    print(
        f"{progress : >11s} {bench_name: <25s}", end="\r")

    # Run the tests num_run times
    for i in range(num_runs):
        try:
            info = run_one(progress, bench_id, command,
                           algo, optim, filename, extra_opt, expect_many)
        except:
            info = MultiDataObj({})
            info.is_successful = False

        if (info is not None and
            info.is_successful and
            ((realizable and not info.is_unrealizable) or
                (not realizable and info.is_unrealizable))):
            total_elapsed += info.elapsed
            verif_elapsed += info.verif_elapsed
            max_e = max(info.elapsed, max_e)
            min_e = min(info.elapsed, min_e)
            estim = float(estim * i + info.elapsed) / float(i + 1)

            msg = f"[estimate: {estim: 4.3f} s] ({i}/{num_runs} runs){sp : <30s}"
            print(msg, end="\r")
            sys.stdout.flush()
        else:
            break

    elapsed = total_elapsed / num_runs
    verif_elapsed = verif_elapsed / num_runs
    delta = 1000 * max(abs(max_e - elapsed), abs(min_e-elapsed))
    sp = " "
    csvline = "?,?,?,?,?,?"

    if info is not None and info.is_successful and ((realizable and not info.is_unrealizable) or (not realizable and info.is_unrealizable)):
        delta_str = f"{delta : .0f}ms"
        if (float(delta) / (1000.0 * elapsed)) > 0.05:
            delta_str = f"{delta_str} !"
        else:
            delta_str = f"{delta_str}  "
        timing = f"average: {elapsed: 4.3f}s ±{delta_str}"
        csvline = success_msg(progress, info, elapsed,
                              bench_name, num_runs, delta, timing, verif_elapsed)
    else:
        errors += [bench_id]
        print(f"{progress: >11s} ❌ {bench_id : <90s}")
        if info is not None:
            csvline = f"N/A,N/A,N/A,f{info.major_step_count},N/A,N/A,{info.verif_elapsed : 4.3f}"
        else:
            csvline = f"N/A,N/A,N/A,N/A,N/A,N/A,N/A"

    sys.stdout.flush()

    return errors,  elapsed, csvline


# def run_multi_configuration(progress, bench_id, command, filename, )
