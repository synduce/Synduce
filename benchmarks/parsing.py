import json


def get_minor_step_count(data: dict):
    if "progress" in data:
        steps = list(data["progress"].values())
        if len(steps) <= 0:
            return 0
        return max(len(steps[:-1]) - 2, 0)
    refinement_steps = data.get("refinement-steps")
    if refinement_steps is not None and not refinement_steps == "unknown":
        steps = list(refinement_steps.values())
        if len(steps) <= 0:
            return -1
        return max(len(steps[:-1]) - 2, 0)
    else:
        return 0


def get_major_step_count(data):
    if "progress" in data:
        return len(data["progress"].values())
    refinement_steps = data.get("refinement-steps")
    if refinement_steps is not None and not refinement_steps == "unknown":
        return len(refinement_steps.values())
    else:
        return 0


def is_successful(data: dict):
    ret = False
    refinement_steps = data.get("refinement-steps")
    if refinement_steps is not None and not refinement_steps == "unknown":
        final_step = list(refinement_steps.values())[-1]
        success_info = final_step.get("success")
        if success_info:
            ret = bool(success_info.get("verified"))
    unrealizable = data.get("unrealizable")
    if unrealizable is not None:
        ret = ret or unrealizable
    return ret


def is_unrealizable(data: dict):
    unrealizable = data.get("unrealizable")
    return bool(unrealizable)


def all_proved_by_induction(data: dict):
    cex_classif_flag = True
    lemma_proof_flag = True
    refinement_steps = data.get("refinement-steps")
    if refinement_steps is not None and not refinement_steps == "unknown":
        for step in refinement_steps.values():
            failures = step.get("failures")
            if failures:
                for failure in failures.values():
                    classif_with = failure.get('cex_classification_with')
                    if classif_with:
                        if classif_with.startswith("bounded"):
                            cex_classif_flag = False

                    proved_by = failure.get('proved_by')
                    if proved_by:
                        if proved_by.startswith("bounded"):
                            lemma_proof_flag = False

    return cex_classif_flag, lemma_proof_flag


def get_last_step_elapsed(data: dict):
    last_step_time = 0.0
    progress_data = data.get("progress")
    if progress_data:
        for step in progress_data.values():
            start_time = step.get("start_info").get("start_time")
            if start_time:
                last_step_time = float(start_time)

    else:
        refinement_steps = data.get("refinement-steps")
        if refinement_steps is not None and not refinement_steps == "unknown":
            for step in refinement_steps.values():
                start_time = step.get("start_info").get("start_time")
                if start_time:
                    last_step_time = float(start_time)

    return last_step_time


def get_verif_elapsed(data: dict):
    verif_elapsed = data.get("verif-elapsed")
    return verif_elapsed


class DataObj:
    def __init__(self, data):
        self.data = data
        self.algo = data.get("algorithm")
        self.minor_step_count = get_minor_step_count(data)
        self.major_step_count = get_major_step_count(data)
        self.elapsed = data.get("total-elapsed")
        self.last_elapsed = get_last_step_elapsed(data)
        self.verif_elapsed = get_verif_elapsed(data)
        self.is_successful = is_successful(data)
        self.is_unrealizable = is_unrealizable(data)
        cex_classif_flag, lemma_proof_flag = all_proved_by_induction(data)
        self.proved_by_induction = lemma_proof_flag
        self.classified_by_induction = cex_classif_flag

    def __str__(self,):
        return f"<{self.algo}>[{self.elapsed}][{self.is_successful}(R:{self.is_unrealizable})]"

    def get_refinement_summary(self, set_sizes=False) -> str:
        rsteps = self.data.get("refinement-steps")
        if not rsteps or rsteps == "unknown":
            return None
        str_summary = ""
        for step in rsteps.values():
            verified = step.get("success") and bool(
                step["success"].get("verified"))
            t = int(step["start_info"]["count_in_t"])
            u = int(step["start_info"]["count_in_u"])
            if set_sizes:
                str_summary += f"<{t}:{u}>"
            else:
                str_summary += "+"
            failures = step.get("failures")
            if failures:
                for failure_step in failures.values():
                    if bool(failure_step.get("lifted")):
                        str_summary += "^"
                    else:
                        str_summary += "."
            if verified:
                str_summary += "✓"

        if is_unrealizable(self.data):
            str_summary += "∅"

        return str_summary


class MultiDataObj():
    def __init__(self, data):
        algo = data.get("algorithm")
        self.is_successful = False
        self.is_progress = False
        self.is_intermediate = False
        self.is_unrealizable = True
        self.data_objs = []
        self.verif_elapsed = 0
        self.elapsed = 0
        self.total_configurations = -1
        self.unr_cache_hits = 0
        self.orig_conf_hit = False
        self.foreign_lemma_uses = 0
        # If algo is specified, then this is a single entry
        if algo:
            final_dat = DataObj(data)
            if final_dat is not None:
                self.data_objs = [final_dat]
            self.is_progress = False
            self.is_intermediate = False
            self.is_unrealizable = True  # compute later

        # This is a progress message
        elif "progress" in data:
            progress_data = DataObj(data)
            if progress_data is not None:
                self.data_objs = [progress_data]
            self.is_progress = True
            self.is_unrealizable = True  # compute later

        # This message contains an intermediate result
        elif "intermediate-result" in data:
            self.is_intermediate = True
            self.total_configurations = data.get("total-configurations")
            self.unr_cache_hits = data.get("unr-cache-hits")
            self.orig_conf_hit = data.get("orig-conf-hit")
            self.foreign_lemma_uses = data.get("foreign-lemma-uses")
            res = data.get("intermediate-result")
            if "failure" in res:
                self.failed = True
                self.is_unrealizable = False
            else:
                self.failed = False
                self.is_unrealizable = res.get("unrealizable")
            self.elapsed = res.get("total-elapsed")
            self.verif_elapsed = res.get("verif-elapsed")

        # Otherwise, it should be a set of responses:
        else:
            for problem_id, problem_data in data.items():
                if problem_id.startswith("problem"):
                    data_obj = DataObj(problem_data)
                    if data_obj is not None:
                        self.data_objs.append(data_obj)
                else:
                    pass

            total_configurations = data.get('total-configurations')
            self.unr_cache_hits = data.get("unr-cache-hits")
            self.orig_conf_hit = data.get("orig-conf-hit")
            self.foreign_lemma_uses = data.get("foreign-lemma-uses")
            if total_configurations is not None:
                self.total_configurations = total_configurations

        for dat in self.data_objs:
            if dat.elapsed is not None and dat.elapsed > 0:
                self.elapsed += dat.elapsed
            if dat.verif_elapsed is not None and dat.verif_elapsed > 0:
                self.verif_elapsed += dat.verif_elapsed
            self.is_unrealizable = self.is_unrealizable and dat.is_unrealizable
            if dat.is_successful:
                self.is_successful = True

        self.counts = (0, 0, 0)
        if not self.is_intermediate:
            self.counts = (self.count_solved_configurations(),
                           self.count_unrealizable_configurations(),
                           0)

    def __str__(self,):
        if self.is_progress or self.is_successful:
            return f"({self.is_progress}/{self.is_successful})(R:{self.is_unrealizable})" + " ".join([str(x) for x in self.data_objs])
        else:
            return f"(Intermediate step {self.total_configurations} configurations.)"

    def set_counts(self, solns, unr, fails):
        self.counts = (solns, unr, fails)

    def count_total_configurations(self):
        return self.total_configurations

    def count_solved_configurations(self):
        if self.is_intermediate:
            return self.counts[0] + self.counts[1] + self.counts[2]
        return len(self.data_objs)

    def count_unrealizable_configurations(self):
        if self.is_intermediate:
            return self.counts[1]
        count = 0
        for dat in self.data_objs:
            count += 1 if dat.is_unrealizable and dat.is_successful else 0
        return count

    def count_failures(self):
        if self.is_intermediate:
            return self.counts[2]
        return 0

    def count_solutions(self):
        if self.is_intermediate:
            return self.counts[0]
        count = 0
        for dat in self.data_objs:
            if not dat.is_unrealizable and dat.is_successful:
                count += 1
        return count

    def get_refinement_summary(self, i: int) -> str:
        if len(self.data_objs) <= i:
            return "?"
        dat = self.data_objs[i]
        return dat.get_refinement_summary()

    def is_proved_by_induction(self, i: int) -> bool:
        if len(self.data_objs) <= i:
            return False
        dat = self.data_objs[i]
        return dat.proved_by_induction

    def major_step_count(self, i: int) -> int:
        if len(self.data_objs) <= i:
            return False
        dat = self.data_objs[i]
        return dat.major_step_count

    def minor_step_count(self, i: int) -> int:
        if len(self.data_objs) <= i:
            return False
        dat = self.data_objs[i]
        return dat.minor_step_count

    def algo(self, i: int) -> str:
        if len(self.data_objs) <= i:
            return "None"
        dat = self.data_objs[i]
        return dat.algo

    def is_classified_by_induction(self, i: int) -> bool:
        if len(self.data_objs) <= i:
            return False
        dat = self.data_objs[i]
        return dat.classified_by_induction
