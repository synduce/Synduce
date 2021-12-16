import json


def get_minor_step_count(data: dict):
    if "progress" in data:
        steps = list(data["progress"].values())
        if len(steps) <= 0:
            return 0
        return max(len(steps[:-1]) - 2, 0)

    if "refinement-steps" in data:
        steps = list(data["refinement-steps"].values())
        if len(steps) <= 0:
            return -1
        return max(len(steps[:-1]) - 2, 0)
    else:
        return 0


def get_major_step_count(data):
    if "progress" in data:
        return len(data["progress"].values())
    if "refinement-steps" in data:
        return len(data["refinement-steps"].values())
    else:
        return 0


def is_successful(data: dict):
    ret = False
    if "refinement-steps" in data:
        final_step = list(data["refinement-steps"].values())[-1]
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
    if refinement_steps:
        for step in refinement_steps.values():
            for failure in step["failures"].values():

                classif_with = failure.get('cex_classification_with')
                if classif_with:
                    if classif_with.startswith("bounded"):
                        cex_classif_flag = False

                proved_by = failure.get('proved_by')
                if proved_by:
                    if proved_by.startswith("bounded"):
                        lemma_proof_flag = False

    return cex_classif_flag, lemma_proof_flag


class DataObj:
    def __init__(self, data):
        self.data = data
        self.algo = data.get("algorithm")
        self.minor_step_count = get_minor_step_count(data)
        self.major_step_count = get_major_step_count(data)
        self.elapsed = data.get("total_elapsed")
        self.verif_elapsed = data.get("verif_elapsed")
        self.is_successful = is_successful(data)
        self.is_unrealizable = is_unrealizable(data)
        cex_classif_flag, lemma_proof_flag = all_proved_by_induction(data)
        self.proved_by_induction = lemma_proof_flag
        self.classified_by_induction = cex_classif_flag

    def __str__(self,):
        return f"<{self.algo}>[{self.elapsed}][{self.is_successful}(R:{self.is_unrealizable})]"

    def get_refinement_summary(self, set_sizes=False) -> str:
        rsteps = self.data.get("refinement-steps")
        if not rsteps:
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
            for failure_step in step.get("failures").values():
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
        # If algo is specified, then this is a single entry
        if algo:
            final_dat = DataObj(data)
            if final_dat is not None:
                self.data_objs = [final_dat]
            self.is_progress = False
        # This is a progress message
        elif "progress" in data:
            progress_data = DataObj(data)
            if progress_data is not None:
                self.data_objs = [progress_data]
            self.is_progress = True
        # Otherwise, it should be a set of responses:
        else:
            self.data_objs = []
            self.is_progress = False
            for problem_id, problem_data in data.items():
                data_obj = DataObj(problem_data)
                if data_obj is not None:
                    self.data_objs.append(data_obj)

        self.is_successful = False
        self.is_unrealizable = True
        self.verif_elapsed = 0
        self.elapsed = 0
        for dat in self.data_objs:
            if dat.elapsed is not None and dat.elapsed > 0:
                self.elapsed += dat.elapsed
            if dat.verif_elapsed is not None and dat.verif_elapsed > 0:
                self.verif_elapsed += dat.verif_elapsed
            self.is_unrealizable = self.is_unrealizable and dat.is_unrealizable
            if dat.is_successful:
                self.is_successful = True

    def __str__(self,):
        return f"({self.is_progress}/{self.is_successful})(R:{self.is_unrealizable})" + " ".join([str(x) for x in self.data_objs])

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
