import CPU, Memory

class Blade(object):

    def __init__(self,bladeName : str, acceptedCPUs: str, maxCpus : int, dimmsPerCPU : int, maxDimmCapacity: int, dimmGeneration : int, maxTransferRate : int):

        self.cpuQuantity = maxCpus
        self.bladeName = bladeName
        self.acceptedCPUs = acceptedCPUs
        self.dimmsPerCPU = dimmsPerCPU
        self.maxDimmCapacity = maxDimmCapacity
        self.dimmGeneration = dimmGeneration
        self.maxTransferRate = maxTransferRate
        self.maxScore = maxDimmCapacity * maxTransferRate
        self.dimm = Memory.RAM("DDR",0,0,0,0, maxDimmCapacity)
        self.heat = 0
        self.price = 0

    def setCPU(self, cpu : CPU.CompUnit) -> bool:
        if ("-".join((cpu.brand, cpu.subBrand, cpu.codeName)) in self.acceptedCPUs):
            self.cpu = cpu

            if (cpuLimit := max(cpu.cpuConfigs)) < self.cpuQuantity:
                self.cpuQuantity = cpuLimit

            self.heat = self.cpuQuantity * cpu.tdp

            for memSpec in cpu.memorySpecs:
                if (memSpec["slots"] < self.dimmsPerCPU):
                    self.dimmsPerCPU = memSpec["slots"]
                if (memSpec["speed"] < self.maxTransferRate):
                    self.maxTransferRate = memSpec["speed"]

                maxMemoryCapCPU = memSpec["max_memory"].split('b')
                if (newMaxDimmSize := ((int(maxMemoryCapCPU[0])*(1024**int(maxMemoryCapCPU[1])))/(1024**3))) < self.maxDimmCapacity:
                    self.maxDimmCapacity = newMaxDimmSize
                self.maxScore = self.maxDimmCapacity * self.maxTransferRate
            self.heat += 11 * self.maxDimmCapacity
            return True
        return False

    def fastestMemory(self, ram : Memory.RAM) -> None:
        if ram.generation == self.dimmGeneration:

            if (ram.score > self.dimm.score) and (ram.score <= self.maxScore):
                self.dimm = ram

    def calcPrice(self):
        self.price = self.cpu.price
        self.price += self.maxDimmCapacity * self.dimm.price

    def getHeat(self):
        return self.heat


    def getPrice(self):
        return self.price

    def __str__(self):
        return"{bldNme}\nCPU: {cpuCnt}x {cpuDesc}\nMemory: {dimmCnt} DIMMs filled with {mem} per CPU".format(bldNme=self.bladeName, cpuCnt=self.cpuQuantity, cpuDesc=self.cpu, dimmCnt=self.dimmsPerCPU, mem=self.dimm)
