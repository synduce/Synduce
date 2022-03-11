import json
from timeout_v import timeout_value


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


def check_missing_unrealizable(data):
    refinement_steps = data.get("refinement-steps")
    b = False
    if refinement_steps:
        for step in refinement_steps.values():
            for failure in step["failures"].values():
                um = failure.get('unrealizability_methods')
                if um:
                    # CHeck whether unrealizability with smt-proof worked
                    b = b or (not ('smp' in um))


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
        self.missing_unrealizable_smt_checks = check_missing_unrealizable(data)

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
