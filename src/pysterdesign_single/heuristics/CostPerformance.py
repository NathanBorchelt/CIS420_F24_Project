from typing import Union



def execute() -> Union[bool, str]:
    print("heur cost perf execute")
    return "cost perf execution"


def identity() -> str:
    return "Node cost / Peak Perf."