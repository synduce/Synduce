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


class BenchmarkResult(object):
    def __init__(self, info, progress, elapsed):
        self.info = info
        self.progress = progress
        self.elapsed = elapsed
        self.benchmark_name = ""
        self.delta = 0.0
        self.num_runs = 0
        self.verif_elapsed = 0.0

    def timing(self):
        return f"average: {self.elapsed: 4.3f}s ±{self.delta_str()}"

    def delta_str(self):
        delta_str = f"{self.delta : .0f}ms"
        if (float(self.delta) / (1000.0 * self.elapsed)) > 0.05:
            delta_str = f"{delta_str} !"
        else:
            delta_str = f"{delta_str}  "
        return delta_str

    def success_msg(self):
        current_algo = self.info.algo(0)
        if self.info.is_proved_by_induction(0):
            p_by_induction = "✓"
        else:
            p_by_induction = "~"

        if self.info.is_classified_by_induction(0):
            c_by_induction = "✓"
        else:
            c_by_induction = "~"

        refinement_rounds = self.info.get_refinement_summary(0)
        if "." in refinement_rounds:
            induction_info = f"B:{c_by_induction},B':{p_by_induction}"
        else:
            induction_info = "        "

        msg = f"{self.progress : >11s} ✅"
        msg += f"{current_algo: <6s}: {self.benchmark_name: <33s} ×{self.num_runs} runs,"
        msg += f" {self.timing(): <30s} {induction_info} | R: {refinement_rounds} {sp : <15s} "
        print(msg)

        csv = f"{self.elapsed: 4.3f},"
        csv += "N/A,"
        csv += f"{self.delta : .0f},"
        csv += f"{refinement_rounds},"
        csv += f"{c_by_induction},"
        csv += f"{p_by_induction},"
        csv += f"{self.verif_elapsed : 4.3f}"
        return csv

    def success_msg_multi(self):
        current_algo = self.info.algo(0)
        num_solved_confs = self.info.count_solved_configurations()
        num_total_confs = self.info.count_total_configurations()
        num_solutions = self.info.count_solutions()
        num_unrealizable = self.info.count_unrealizable_configurations()

        msg = f"{self.progress : >11s} ✅"
        msg += f"{current_algo: <6s}: {self.benchmark_name: <33s} ×{self.num_runs} runs,"
        msg += f" {self.timing(): <30s}"
        cnt = f"{num_solved_confs}/{num_total_confs} S:{num_solutions}, U:{num_unrealizable}"
        msg += f" {cnt : <10s}"
        print(msg)

        csv = f"{self.elapsed: 4.3f},"
        csv += "N/A,"
        csv += f"{self.delta : .0f},"
        csv += f"{num_solved_confs},"
        csv += f"{num_total_confs},"
        csv += f"{num_solutions},"
        csv += f"{num_unrealizable},"
        csv += f"{self.verif_elapsed : 4.3f}"
        return csv


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

    result = BenchmarkResult(info, progress, total_elapsed / num_runs)
    result.verif_elapsed = verif_elapsed / num_runs
    delta = 1000 * max(abs(max_e - result.elapsed), abs(min_e-result.elapsed))
    sp = " "
    csvline = "?,?,?,?,?,?"

    if info is not None and info.is_successful and ((realizable and not info.is_unrealizable) or (not realizable and info.is_unrealizable)):
        result.delta = delta
        result.timing
        if expect_many:
            csvline = result.success_msg_multi()
        else:
            csvline = result.success_msg()
    else:
        errors += [bench_id]
        print(f"{progress: >11s} ❌ {bench_id : <90s}")
        if info is not None:
            csvline = f"N/A,N/A,N/A,f{info.major_step_count},N/A,N/A,{info.verif_elapsed : 4.3f}"
        else:
            csvline = f"N/A,N/A,N/A,N/A,N/A,N/A,N/A"

    sys.stdout.flush()

    return errors, result.elapsed, csvline


# def run_multi_configuration(progress, bench_id, command, filename, )
