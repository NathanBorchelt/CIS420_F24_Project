from typing import Dict, List
from sys import exit as e_exit

sizeBinaryLong = ["", "Kibi", "Mebi", "Gibi", "Tebi", "Pebi", "Exbi", "Zebi", "Yobi"]
sizeBinaryShort = ["", "Ki", "Mi", "Gi", "Ti", "Pi", "Ei", "Zi", "Yi"]
sizeDecimalLong = ["", "Kilo", "Mega", "Giga", "Tera", "Peta", "Exa", "Zetta", "Yotta", "Ronna", "Quetta"]
sizeDecimalShort = ["", "k", "M", "G", "T", "P", "E", "Z", "Y", "R", "Q"]

class CompUnit(object):

    def __init__(self, brand : str, subBrand : str, codeName : str, model : str, cpuConfigs : List[int], price : int, cores : int, cache : Dict[str, int], tdp : int, memorySpecs : List[dict], clockSpeed : Dict[str, float], featureSize : float, flopsPerCycle : int):

        self.brand = brand
        self.subBrand = subBrand
        self.codeName = codeName
        self.model = model
        self.cpuConfigs = cpuConfigs
        self.price = price
        self.cores = cores
        self.cache = cache
        self.tdp = tdp
        self.memorySpecs = memorySpecs
        self.clockSpeed = clockSpeed
        self.featureSize = featureSize
        self.flopsPerCycle = flopsPerCycle

        #print()
        try:
            for level, cacheSize in self.cache.items():
                allData = self.model
                #print(cacheSize)
                cacheData = cacheSize.split('b')
                #print(cacheData)
                cacheData[1] = sizeBinaryLong[int(cacheData[1])]
                self.cache[level] = " ".join(cacheData)
        except Exception as e:
            print("error with the data conversion of the cache for the following chip")
            print(self.brand, self.subBrand, self.codeName, self.model, self.cpuConfigs, self.price, self.cores, self.cache, self.tdp, self.memorySpecs, self.clockSpeed, self.featureSize, self.flopsPerCycle)
            e_exit()
        #print(self.cache)

    def __str__(self) ->  str:
        return "{bnd} {sbnd} {cdnme} {mdl}".format(bnd = self.brand, sbnd = self.subBrand, cdnme = self.codeName, mdl = self.model)