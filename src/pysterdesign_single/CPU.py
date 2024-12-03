#!/usr/local/bin/python3.8

from typing import Dict, List, Type
from sys import exit as e_exit

import Memory

sizeBinaryLong = ["", "Kibi", "Mebi", "Gibi", "Tebi", "Pebi", "Exbi", "Zebi", "Yobi"] #1024^x
sizeBinaryShort = ["", "Ki", "Mi", "Gi", "Ti", "Pi", "Ei", "Zi", "Yi"]
sizeDecimalLong = ["", "Kilo", "Mega", "Giga", "Tera", "Peta", "Exa", "Zetta", "Yotta", "Ronna", "Quetta"]#1000^x
sizeDecimalShort = ["", "k", "M", "G", "T", "P", "E", "Z", "Y", "R", "Q"]

DDR5_DEFAULT =  Memory.RAM('DDR5', 5, 38400, 32, 634,5)

class CompUnit(object):

    def __init__(self, brand : str, codeName : str, model : str, price : int,
                cores : int, cache : Dict[str, str], tdp : int,
                clockSpeed : Dict[str, float], featureSize : float, flopsPerCycle : int, 
                subBrand : str = None, cpuConfigs : List[int] = None, memorySpecs : List[Type[Memory.RAM]] = None, isJSON : bool = True):
        



        if cpuConfigs is None:
            cpuConfigs = [1,2]
        if memorySpecs is None:
            memorySpecs = [DDR5_DEFAULT]
        if subBrand is None:
            subBrand = brand

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
            if isJSON:
                for level, cacheSize in self.cache.items():
                    allData = self.model
                    # print(cacheSize)
                    cacheData = cacheSize.split('b')
                    # print(cacheData)
                    cacheData[1] = sizeBinaryLong[int(cacheData[1])]
                    self.cache[level] = " ".join(cacheData)
        except Exception as e:
            print(e)
            print("error with the data conversion of the cache for the following chip")
            print(self.brand, self.subBrand, self.codeName, self.model, self.cpuConfigs, self.price, self.cores, self.cache, self.tdp, self.memorySpecs, self.clockSpeed, self.featureSize, self.flopsPerCycle)
            e_exit()
        #print(self.cache)

    def getBrand(self) -> str:
        return self.brand
        
    def getCodeName(self) -> str:
        return self.codeName
        
    def getModel(self) -> str:
        return self.model
        
    def getPrice(self) -> int:
        return self.price
        
    def getCores(self) -> int:
        return self.cores
        
    def getCache(self) -> Dict[str, str]:
        return self.cache
        
    def getTdp(self) -> int:
        return self.tdp
        
    def getClockSpeed(self) -> Dict[str, float]:
        return self.clockSpeed
        
    def getFeatureSize(self) -> float:
        return self.featureSize
        
    def getFlopsPerCycle(self) -> int:
        return self.flopsPerCycle
        
    def getSubBrand(self) -> str:
        return self.subBrand
        
    def getCpuConfigs(self) -> List[int]:
        return self.cpuConfigs
        
    def getMemorySpecs(self) -> List[Type[Memory.RAM]]:
        return self.memorySpecs

    def __str__(self) ->  str:
        return "{bnd} {sbnd} {cdnme} {mdl}".format(bnd = self.brand, sbnd = self.subBrand, cdnme = self.codeName, mdl = self.model)