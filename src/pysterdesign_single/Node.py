import RackMount, Blade

class ComputeNode(RackMount.RackMount):

    def __init__(self, name: str, height: float, acceptedBlades: list, bladeQuantity : int, blade: Blade.Blade=None):
        super().__init__(name, height)
        self.blade = blade
        self.bladeQuantity = bladeQuantity
        self.acceptedBlades = acceptedBlades
        self.heat = 0
        self.price = 0

    def setBlade(self, blade: Blade.Blade) -> bool:
        if blade.bladeName in self.acceptedBlades:
            self.blade = blade
            self.heat = blade.heat * self.bladeQuantity
            self.price = blade.price * self.bladeQuantity
            return True
        return False


    def __str__(self):
        return "{hgt}U \"{nme}\" chassis holding {bldqnt}x {bld}".format(hgt=self.height ,nme=self.name  ,bldqnt=self.bladeQuantity ,bld=self.blade)
