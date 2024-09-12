from typing import Union



def execute() -> Union[bool, str]:
    print("FatTree Nonblocking module")
    return "FatTree Nonblocking module execution"


def identity() -> str:
    return "Fat-Tree, non-blocking"

def url() -> str:
    return "http://localhost:8000/cgi-bin/network/network?task=design&network_vendor=mellanox&network_topology=fat-tree"