#!/usr/local/bin/python3.8
import copy
import RackMount

class Chassis(object):


    def __init__(self, name : str, height : float, chassisCost : int = 0):
        self.name = name
        self.height = height
        self.freeSpace = height
        self.occupiedSpace = []
        self.flops=0
        self.flopEnergyEfficency=0
        self.heat = 0
        self.yearlyElectical = 0
        self.chassisCost = chassisCost
        self.price = 0
        self.totalCost = 0
        self.yearlyFlops = 0



    def calculateFlops(self) -> None:
        #FLOPs = cores * cycles/second * flops/cycle
        self.flops = 0
        for item in self.occupiedSpace:
            try:
                cpu = item.blade.cpu
                self.flops += (item.bladeQuantity * (item.blade.cpuQuantity * (cpu.cores * (cpu.clockSpeed["all_boost"]*(10**9)) * cpu.flopsPerCycle)))
            except:
                pass

    def calculateEfficiency(self) -> None:
        if self.flops == 0:
            self.calculateFlops()
        self.heat = 0
        for item in self.occupiedSpace:
            try:
                self.heat += item.heat
            except:
                pass
        self.flopEnergyEfficency = self.flops/self.heat

    def yearlyCost(self, electricityPricePerKWh : float) -> None:
        self.yearlyElectical = round(((self.heat * 8766) / 1000) * electricityPricePerKWh,2)

    def calculateHardwareCost(self):
        self.price = self.chassisCost
        for item in self.occupiedSpace:
            try:
                self.price += item.calcPrice()
            except:
                continue

    def totalCostOfOwnership(self, lifespan : int = 1,  electricityPricePerKWh : float = .135) -> None:
        if self.price == 0:
            self.calculateHardwareCost()
        self.yearlyCost(electricityPricePerKWh)
        self.totalCost = self.price + (lifespan * self.yearlyElectical)

    def ownershipCostEfficencyYearly(self, electricityPricePerKWh : float = .135):
        if self.flops == 0:
            self.calculateFlops()
        self.yearlyFlops = self.flops * 31536000
        self.yearlyKwh = self.heat * 8766 / 1000
        self.yearlyEfficency = self.yearlyFlops / (self.yearlyKwh * electricityPricePerKWh)

    def addItem(self, item : RackMount.RackMount) -> None:
        itemHeight : int = item.getHeight()
        while(self.freeSpace - itemHeight > itemHeight):
            self.occupiedSpace.append(item)
            self.freeSpace -= itemHeight

        if(self.freeSpace - itemHeight > 0 ):
            self.occupiedSpace.append(item)
            self.freeSpace -= itemHeight

        self.calculateFlops()
        self.calculateEfficiency()

    def __str__(self):
        allItemPrint = ""
        for i in range(len(self.occupiedSpace)):
            allItemPrint += "    Item {num}: {item}\n".format(num=i, item=self.occupiedSpace[i])

        return "{chass}: Capacity: {cap}U\n  Free Space: {frSpc}U\n  Filled with:\n{items}".format(chass=self.name , cap=self.height , frSpc=self.freeSpace , items=allItemPrint)
