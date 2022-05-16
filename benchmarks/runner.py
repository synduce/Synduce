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
    last_verif_time = 0.0
    last_elapsed = 0.0
    last_info = None
    # Intermediate solutions counts
    count_solns = 0
    count_unr = 0
    count_fails = 0
    interm_elapsed = 0.0
    interm_verif = 0.0
    # Poll process for new output until finished
    while True:
        try:
            line = process.stdout.readline()
        except:
            break
        try:
            data = json.loads(line)
        except Exception as e:
            break

        try:

            info = MultiDataObj(data)

            if info.is_intermediate:
                interm_elapsed += info.elapsed
                interm_verif += info.verif_elapsed
                if info.failed:
                    count_fails += 1
                elif info.is_unrealizable:
                    count_unr += 1
                else:
                    count_solns += 1
            else:
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
        if info.is_progress:
            print(
                f"{progress : >11s}.. benchmarks/{filename} {extra_opt} {algo[1]} {optim[1]} 🏃 at step {info.major_step_count(0)}:{info.minor_step_count(0)}", end="\r")
        if info.is_intermediate:
            print(
                f"{progress : >11s}... benchmarks/{filename} {extra_opt} {algo[1]} {optim[1]} 🏃 [{count_solns}/{count_unr}/{count_fails}]", end='\r')

    if info is not None and last_info is None:
        last_info = info

    # Save intermediate solution counts in info
    if last_info.is_intermediate:
        last_info.is_successful = True
        last_info.elapsed = interm_elapsed
        last_info.verif_elapsed = interm_verif
        last_info.set_counts(count_solns, count_unr, count_fails)

    return last_info


class MultiCsvLine(object):
    def __init__(self, elapsed, delta, complete, solved, total, solutions, unrealizable, verif, ch, oh, ph):
        self.elapsed = elapsed
        self.delta = delta
        self.complete = complete
        self.solved = solved
        self.total = total
        self.solutions = solutions
        self.unrealizable = unrealizable
        self.verif_time = verif
        self.cache_hits = ch
        self.orig_conf_hit = oh
        self.predicate_hit = ph

    def from_string(line):
        parts = line.split(",")
        if len(parts) < 11:
            return None
        elapsed = floti(parts[0])
        delta = floti(parts[1])
        verif = floti(parts[2])
        complete = booli(parts[3])
        solved = int(parts[4])
        total = int(parts[5])
        solutions = int(parts[6])
        unr = int(parts[7])
        ch = int(parts[8])
        oh = booli(parts[9])
        ph = int(parts[10])
        return MultiCsvLine(elapsed, delta, complete, solved, total, solutions, unr, verif, ch, oh, ph)

    def get_csv_string(self):
        l = [self.elapsed, self.delta, self.verif_time]
        l += [self.complete, self.solved, self.total]
        l += [self.solutions, self.unrealizable]
        l += [self.cache_hits, self.orig_conf_hit, self.predicate_hit]
        return ",".join([str(x) for x in l])


class SingleCsvLine(object):
    def __init__(self, elapsed, delta, refinement_rounds,
                 c_by_induction, p_by_induction, verif_elapsed):
        self.elapsed = elapsed
        self.delta = delta
        self.refinement_rounds = refinement_rounds
        self.c_by_induction = c_by_induction
        self.p_by_induction = p_by_induction
        self.verif_elapsed = verif_elapsed

    def get_csv_string(self):
        l = [self.elapsed, "N/A", self.delta]
        l += [self.refinement_rounds, self.p_by_induction, self.c_by_induction]
        l += [self.verif_elapsed]
        return ",".join([str(x) for x in l])


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
        return f"avg: {self.elapsed: 4.3f}s ±{self.delta_str()}"

    def delta_str(self):
        delta_str = f"{self.delta : .0f}ms"
        if self.elapsed > 0 and (float(self.delta) / (1000.0 * self.elapsed)) > 0.05:
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

        msg = f"{self.progress : >8s} ✅"
        msg += f"{current_algo: <6s}: {self.benchmark_name: <33s} ×{self.num_runs} runs,"
        msg += f" {self.timing(): <30s} {induction_info} | R: {refinement_rounds} {sp : <15s} "
        print(msg)

        csv_obj = SingleCsvLine(self.elapsed, self.delta,
                                refinement_rounds, c_by_induction, p_by_induction,
                                self.verif_elapsed)
        return csv_obj.get_csv_string()

    def success_msg_multi(self):
        current_algo = self.info.algo(0)
        num_solved_confs = self.info.count_solved_configurations()
        num_total_confs = self.info.count_total_configurations()
        num_solutions = self.info.count_solutions()
        num_unrealizable = self.info.count_unrealizable_configurations()
        num_failures = self.info.count_failures()

        msg = f"{self.progress : >6s}"
        if self.info.is_intermediate:
            msg += " ❔ "
        else:
            msg += " ✅ "
        msg += f"{current_algo: <8s}: {self.benchmark_name: <33s} ×{self.num_runs},"
        msg += f" {self.timing(): <25s}"
        ratio = f"{num_solved_confs}/{num_total_confs}"
        cnt = f"{ratio: <10s}"
        nums = f"[{num_solutions},{num_unrealizable},{num_failures}]"
        cnt += f"{nums : <10s}"
        cnt += f"CH:{self.info.unr_cache_hits} S:{self.info.orig_conf_hit} P:{self.info.foreign_lemma_uses}"
        msg += f" {cnt : <24s}"
        print(msg)

        csv_obj = MultiCsvLine(self.elapsed, self.delta,
                               not self.info.is_intermediate,
                               num_solved_confs, num_total_confs,
                               num_solutions, num_unrealizable,
                               self.verif_elapsed,
                               self.info.unr_cache_hits,
                               self.info.orig_conf_hit,
                               self.info.foreign_lemma_uses
                               )
        return csv_obj.get_csv_string()


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
        f"{progress : >8s} {bench_name: <25s}", end="\r")

    # Run the tests num_run times
    for i in range(num_runs):
        try:
            info = run_one(progress, bench_id, command,
                           algo, optim, filename, extra_opt, expect_many)
        except:
            info = MultiDataObj({})
            info.is_successful = False

        matches_expected_realizability = ((realizable and not info.is_unrealizable) or
                                          (not realizable and info.is_unrealizable)) if info is not None else False

        if (matches_expected_realizability or expect_many) and info.is_successful:
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
    if info.is_intermediate:
        print
    result = BenchmarkResult(info, progress, total_elapsed / num_runs)
    result.verif_elapsed = verif_elapsed / num_runs
    result.benchmark_name = bench_name
    sp = " "
    csvline = "?,?,?,?,?,?"

    result_realizable = (not info.is_unrealizable) or (
        info.count_solutions() > 0)

    matches_expected_realizability = ((realizable and result_realizable) or
                                      (not realizable and info.is_unrealizable)) if info is not None else False

    if matches_expected_realizability and info.is_successful:
        if num_runs > 1:
            result.delta = 1000 * \
                max(abs(max_e - result.elapsed), abs(min_e-result.elapsed))

        if expect_many:
            csvline = result.success_msg_multi()
        else:
            csvline = result.success_msg()
    else:
        errors += [bench_id]
        if expect_many:
            print(f"{progress: >11s} ❌ {bench_id : <90s}")
            if info is not None:
                num_solutions = info.count_solutions()
                num_unrealizable = info.count_unrealizable_configurations()
                csv_obj = MultiCsvLine(
                    info.elapsed, 0, False, info.count_solved_configurations(),
                    info.total_configurations, num_solutions, num_unrealizable,
                    info.verif_elapsed, info.unr_cache_hits, info.orig_conf_hit,
                    info.foreign_lemma_uses)
                csvline = csv_obj.get_csv_string()
            else:
                csv_obj = MultiCsvLine(
                    "N/A", 0, False, 0, 0, 0, 0, "N/A", 0, False, 0)
                csvline = csv_obj.get_csv_string()
        else:
            print(f"{progress: >11s} ❌ {bench_id : <90s}")
            if info is not None:
                csv_obj = SingleCsvLine(
                    "N/A", "N/A", f"f{info.major_step_count}", "N/A", "N/A", info.verif_elapsed)
                csvline = csv_obj.get_csv_string()
            else:
                csv_obj = SingleCsvLine(
                    "N/A", "N/A", "N/A", "N/A", "N/A", "N/A")
                csvline = csv_obj.get_csv_string()

    sys.stdout.flush()

    return errors, result.elapsed, csvline
