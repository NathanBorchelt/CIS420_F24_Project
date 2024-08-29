#This code will have to be redone to be ppart of the automation of pulling rom the wiki, but works for now to get it up and running.

from Architecture import Architecture
from CpuSpecs import CPU
from shutil import copy
import fileinput


gen4Architecture = Architecture('Sapphire Rapids', 'Sapphire Rapids', 7)

def listStrip(inputList):
	#https://stackoverflow.com/a/3845449
	return [x for x in inputList if x.strip()]


with open('intel_xeon_gen4.csv', 'r') as sapphireIn:
	with open('../clusterdesign/intel_xeon_gen4.xml', 'w') as intelOut:
		intelOut.write('<?xml version="1.0" encoding="UTF-8"?>\n<itemslist xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="items-schema.xsd">\n')
		for cpu in sapphireIn:
			cpuData = cpu.split(',')
			if('Intel' in cpuData[0][1:-1]):
				vendor = cpuData[0][1:-1]
				codeName = gen4Architecture.getCodename()
				model = cpuData[2][1:-1] + ' ' + cpuData[4][1:-1]
				cores = cpuData[5]
				size = gen4Architecture.getSize()
				frequency = cpuData[6]
				flops = 32 #A guess since intel won't tell you and I havent found a GFLOPs calc out there to calculate backwards to the FLOPs/cycle
				cache = cpuData[7]
				tdp = cpuData[8]
				cost = cpuData[9].strip()
				
				newCPU = CPU(vendor, codeName, model, cores, size, frequency, flops, cache, tdp, cost)
				
				intelOut.write(newCPU.__str__()+'\n')
		intelOut.write('</itemslist>')
