#Starts line 732 in ripped file

from CpuSpecs import CPU
import fileinput
from linecache import getline
from re import findall
from shutil import copy		 

def cpuRipping(cpuLineUp, cpus):
	currentArchitecture = 'Sapphire Rapids'
	
	#https://stackoverflow.com/questions/6473283/basic-python-file-io-variables-with-enumerate
	for i, line in enumerate(fileinput.input([cpuLineUp])):
		line = line.strip()
		#print(i)
		
		#this line is kind of specific, may has the potential to be extrapolated to other intel CPUs
		if ('<td><a rel="nofollow" class="external text" href="https://www.intel.com/content/www/us/en/products/sku' in line):
			#print(i, '\t', line)
			fileLineItterator = 0
			cpuData = ''
			while ((cpuDataLine := getline(cpuLineUp, i+fileLineItterator)).rfind('</tr>') == -1):
				cpuData += cpuDataLine.strip()
				fileLineItterator += 1
			level1Extrapolation = cpuData.split('><')[2:]
			if (not ('1S' in level1Extrapolation[8])):
				level2Extrapolation = []
				for itemL1 in level1Extrapolation:
					seperation = itemL1.split('>')
					try:
						level2Extrapolation.append(seperation[1])
					except:
						pass	
				level3Extrapolation = []
				for itemL2 in level2Extrapolation:
					level3Extrapolation.append(itemL2.split('<')[0])
				
				#remove thread count, leaving only core count
				level3Extrapolation[1] = level3Extrapolation[1].split(' ')[0]
				
				#remove MB from cache
				level3Extrapolation[5] = level3Extrapolation[5][0:level3Extrapolation[5].rfind(' MB')]
				
				#remove '$' from price
				level3Extrapolation[-1] = level3Extrapolation[-1][1:]
				
				#get just the cpu number
				level3Extrapolation[0] = level3Extrapolation[0].split(' ')[-1]
				
				finalData = level3Extrapolation
				
				#sample of the list going input
				#['9480', '56', '1.9', '2.6', '3.5', '112.5', '350', '2S', '4800', '4', '12980'
				
				cpuVendor = 'Intel'
				
				cpuCodename = 'Sapphire Rapids'
				
				model = finalData[0]
				if (model[0] == '9'):
					model = 'Max ' + model
				elif (model[0] == '8'):
					model = 'Platinum ' + model
				elif ((model[0] == '6') or (model[0] == '5')):
					model = 'Gold ' + model
				elif (model[0] == '4'):
					model = 'Silver ' + model
				elif (model[0] == '3'):
					model = 'Bronze ' + model
				else:
					model = 'Unknown ' + model
				
				cores = finalData[1]
				
				frequency = finalData[2]
				
				size = 7
				
				flops = 32 #A guess since Intel won't tell you and I havent found a GFLOPs calc out there to calculate backwards to the FLOPs/cycle
				
				cache = finalData[5]
				
				tdp =  finalData[6]
				
				cost = finalData[-1]
				
				cpus.append(CPU(cpuVendor, cpuCodename, model, cores, size, frequency, flops, cache, tdp, cost))
				

def sapphireWriting(cpuList):
	with open('../clusterdesign/intel_xeon_gen4.xml', 'w') as sapphireOut:
		sapphireOut.write('<?xml version="1.0" encoding="UTF-8"?>\n<itemslist xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="items-schema.xsd">\n')
		for cpu in cpuList:
			sapphireOut.write(cpu.__str__()+'\n')
			
		sapphireOut.write('</itemslist>')
		
	copy('../clusterdesign/intel_xeon_gen4.xml', '../saddle/db/intel_xeon_gen4.xml')

def main():
	filesOfCpuLineUps = ["quotes-Sapphire_Rapids.html"]
	for cpuLineUp in filesOfCpuLineUps:
		cpus = []
		cpuRipping(cpuLineUp,cpus)
	
	sapphireWriting(cpus)
		
		








main()

