from typing import Union

class CostPerformance(object):
    def __init__(self):
        pass

    def execute() -> Union[bool, str]:
        print("heur cost perf execute")
        return "cost perf execution"


def identity() -> str:
    return "Node cost / Peak Perf."