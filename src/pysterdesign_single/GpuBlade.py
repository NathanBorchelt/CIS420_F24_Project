import Blade, CPU, Memory, GPU

class GpuBlade(Blade.Blade):
    def __init__(self,bladeName : str, acceptedCPUs: str, maxCpus : int, dimmsPerCPU : int, maxDimmCapacity: int, dimmGeneration : int, maxTransferRate : int, acceptedGPUs : list, maxGPUs : int):
        super().__init__(bladeName, acceptedCPUs, maxCpus, dimmsPerCPU, maxDimmCapacity, dimmGeneration, maxTransferRate)
        self.acceptedGPUs = acceptedGPUs
        self.maxGPUs = maxGPUs

    def setCPU(self, cpu : CPU.CompUnit) -> bool:
        return super().setCPU(cpu)

    def fastestMemory(self, ram : Memory.RAM) -> None:
        super().fastestMemory(ram)

    def setGPU(self, gpu : GPU.GraphicUnit) -> bool:
        if gpu.name in self.acceptedGPUs:
            self.gpu = gpu
            self.heat += self.maxGPUs * gpu.tdp
            return True
        return False

    def calcPrice(self) -> None:
        super().calcPrice()
        self.price += self.maxGPUs * self.gpu.price

    def __str__(self):
        return "{superTxt}\nGPU: {gpuCount} x {gpu} each".format(superTxt=super().__str__(), gpuCount=self.maxGPUs, gpu=self.gpu)