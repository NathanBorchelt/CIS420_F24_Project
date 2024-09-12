from typing import Union

def execute() -> Union[bool, str]:
    print("Liebert APM")
    return "Liebert APM execution"


def identity() -> str:
    return "Liebert APM (up to 45 kW)"

def url() -> str:
    return "http://localhost:8000/cgi-bin/ups/ups_sizer.py?task=design"