from typing import Union



def execute() -> Union[bool, str]:
    print("FatTree 3:1 module")
    return "FatTree 3:1 module execution"


def identity() -> str:
    return "Fat-Tree, 3:1 blocking"

def url() -> str:
    return "http://localhost:8000/cgi-bin/network/network?task=design&network_vendor=mellanox&network_topology=fat-tree&max_network_blocking_factor=3"