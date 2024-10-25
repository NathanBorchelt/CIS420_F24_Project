#!/usr/local/bin/python3.8

import RackMount, Blade

class ComputeNode(RackMount.RackMount):

    def __init__(self, name: str, height: float, acceptedBlades: list, bladeQuantity: int = 4, blade: Blade.Blade=None):
        super().__init__(name, height)
        self.blade = blade
        self.bladeQuantity = bladeQuantity
        self.acceptedBlades = acceptedBlades
        self.heat = 0
        self.price = 0

    def get_blade(self):
        return self.blade

    def getBladeQuantity(self):
        return self.bladeQuantity

    def setBladeQuantity(self, value):
        self.bladeQuantity = value

    def getAcceptedBlades(self):
        return self.acceptedBlades

    def setAcceptedBlades(self, value):
        self.acceptedBlades = value

    def getHeat(self):
        return self.heat

    def setHeat(self, value):
        self.heat = value

    def getPrice(self):
        return self.price

    def setPrice(self, value):
        self.price = value
    

    def setBlade(self, blade: Blade.Blade) -> bool:
        if blade.bladeName in self.acceptedBlades:
            self.blade = blade
            self.heat = blade.heat * self.bladeQuantity
            self.price = blade.price * self.bladeQuantity
            return True
        return False

    

    def __str__(self):
        return "{hgt}U \"{nme}\" chassis holding {bldqnt}x {bld}".format(hgt=self.height ,nme=self.name  ,bldqnt=self.bladeQuantity ,bld=self.blade)
