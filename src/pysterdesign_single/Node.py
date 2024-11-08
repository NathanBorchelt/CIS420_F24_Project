#!/usr/local/bin/python3.8

import RackMount, Blade
from typing import List, Type

class ComputeNode(RackMount.RackMount):

    def __init__(self, name: str, height: float, bladeQuantity: int = 4, bladesList: List[Type[Blade.Blade]]=None):
        super().__init__(name, height)
        self.bladesList = bladesList
        self.bladeQuantity = bladeQuantity

    def addBlade(self, blade: Blade.Blade):
        self.bladesList.append(blade)

    def getHeat(self, blade: Blade.Blade):
        return blade.getHeat() * self.bladeQuantity

    def getPrice(self, blade: Blade.Blade):
        return blade.getPrice() * self.bladeQuantity
    
    def getBlade(self, index: int) -> Blade.Blade:
        return self.bladesList[index]
    
    def getBlades(self) -> List[Type[Blade.Blade]]:
        return self.bladesList


    def __str__(self):
        return "{hgt}U \"{nme}\" chassis holding {bldqnt}x {bld}".format(hgt=self.height ,nme=self.name  ,bldqnt=self.bladeQuantity ,bld=self.bladesList)
