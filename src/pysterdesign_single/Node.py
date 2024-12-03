#!/usr/local/bin/python3.8

import RackMount, Blade
from typing import List, Type

class ComputeNode(RackMount.RackMount):

    def __init__(self, name: str, height: float, bladeQuantity: int = 4, validBlades: List[Type[Blade.Blade]]=None):
        super().__init__(name, height)
        self.validBlades = validBlades
        self.containedBlades = list()
        self.bladeQuantity = bladeQuantity
        self.heat = 0

    def addBlade(self, blade: Blade.Blade) -> None:
        self.validBlades.append(blade)

    def fillBlades(self, blade: Blade.Blade) -> None:
        if blade.bladeName in self.validBlades:
            for _ in range(self.bladeQuantity):
                self.containedBlades.append(blade)

    def getHeat(self, blade: Blade.Blade = None) -> float:
        if(blade is None):
            return self.heat
        self.heat = self.calculateHeat()       
    
    def calculateHeat(self):
        self.heat = 0
        for blade in self.containedBlades:
            self.heat += blade.getHeat()


    def getPrice(self, blade: Blade.Blade) -> float:
        return blade.getPrice() * self.bladeQuantity
    
    def getBlade(self, index: int) -> Blade.Blade:
        return self.validBlades[index]
    
    def getBlades(self) -> List[Type[Blade.Blade]]:
        return self.validBlades
    
    def isFilled(self) -> bool:
        return (len(self.containedBlades) == self.bladeQuantity)


    def __str__(self):
        return "{hgt}U \"{nme}\" chassis holding {bldqnt}x {bld}".format(hgt=self.height ,nme=self.name  ,bldqnt=self.bladeQuantity ,bld=self.containedBlades[0])
