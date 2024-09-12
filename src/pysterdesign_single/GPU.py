from typing import Dict


class GraphicUnit(object):

    def __init__(self , name : str, clock : Dict[str, int], fp64 : Dict[str, int], fp32 : Dict[str, int], fp16 : Dict[str, int], memory : int, tdp : int, maxGpus : int):

        self.name = name
        self.clock = clock
        self.fp64 = fp64
        self.fp32 = fp32
        self.fp16 = fp16
        self.memory = memory
        self.tdp = tdp
        self.maxGpus = maxGpus

    def __str__(self):
        return "{nme} with {mem}GB producing {fp}TFLOPs".format(nme=self.name, mem=self.memory, fp=self.fp64["base"])