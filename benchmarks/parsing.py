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
    return ret


class DataObj:
    def __init__(self, data):
        self.data = data
        self.minor_step_count = get_minor_step_count(data)
        self.major_step_count = get_major_step_count(data)
        self.elapsed = data.get("total_elapsed")
        self.is_successful = is_successful(data)

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

        return str_summary
