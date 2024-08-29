#Code works as of June 28, 2023
#for Python Version 3.8.14

from CpuSpecs import CPU
import fileinput
from linecache import getline
from re import findall
from shutil import copy
from Architecture import Architecture
		 
def listStrip(inputList):
                 #https://stackoverflow.com/a/3845449
                return [x for x in inputList if x.strip()]

def cpuData(cpuLineUp, cpus):
	architectures = []
	architectureCPUs = []
	currentArchitecture = ''
	#https://stackoverflow.com/questions/6473283/basic-python-file-io-variables-with-enumerate
	for i, line in enumerate(fileinput.input([cpuLineUp])):	
		line = line.strip()		
		#this line is specific to this wikichips. it can be slightly more generalized if you stop at tc5, but the differences between each chip gen/brand 
		#is stopping me from creating a general solution at this point
		if (line == '<table class="comptable sortable tc4 tc5 tc10 tc11 tc12">'):
			architectureCPUs = []
			#I could itterate though each line, but this is true for all AMD EPYC CPUs, at least Zen/Naples through Zen4/Genoa
			cpuDataLineNum = i+6
			fileLineItterator = 0
			#to keep itterating though until the end of the table. WikiChips has the entire set of uni/dual socket chips on one line
			while ((cpuDataLine := getline(cpuLineUp, cpuDataLineNum + fileLineItterator)).rfind('</table>') == -1):
				if (cpuDataLine.find('<tr><td>') != -1):
					#this line takes care of the issue of it all being on one line, but this code should still work if they ever update it to be more readable
					allLineProcessors = listStrip(cpuDataLine.split('<tr><td>'))
					#I call it garbage data, but in reality it is all the extra info, like the alternative units
					trimmedProcessorDataWithGarbage = []
					for processor in allLineProcessors:
						#removes all the HTML tags, leaving only the data inbetween
						trimmedProcessorDataWithGarbage.append(findall(r'(?!<)[^>]*(?=<)', processor))
					for processorWithGarbage in trimmedProcessorDataWithGarbage:
						#I will admit, this is the most fragile part of the code. I will one day update this to be more dynamic with changing tables, but this work as of June 28, 2023
						#decause it only writes after pulling and processing the data, there will not be data loss due to a bad info rip
						cleanList = []
						cleanList.append(processorWithGarbage[0])
						#The dollar value is always in the second position of the list created from creating the data
						value = processorWithGarbage[1]
						#removes characters not within the list on the right
						value = ''.join( c for c in value if  c not in '$\xa0,' )
						
						#try/except because not every cpu has a price, this the next value in the list is the realease date. this just itterates to the next CPU
						try:
							cleanList.append("{:.2f}".format(float(value)))
						except:
							continue
						#the vlaue in the 6th position is the number of cores in the cpu
						cleanList.append(processorWithGarbage[6])
						#this was a fun bit of code, some of the cpus have their L2 Cache listed, this makes sure that only the first instance when read backwards
						#this loop also grabs the base clock and the left most value of the TDP, which does mean some CPUs in Naples are listed a little hotter compared to what they would run at
						#because they run hotter with lower memory speeds
						firstOccuranceL3 = True
						baseClock = ''
						baseTDP = ''
						for element in reversed(processorWithGarbage):
							if("MiB" in element and firstOccuranceL3):
								cleanList.append(element[0:element.index('MiB')-1])
								firstOccuranceL3 = False
							elif "\xa0W" in element:
								baseTDP = (element[0:element.index('\xa0')])
							elif "\xa0GHz" in element:
								baseClock = (element[0:element.index('\xa0')])
						cleanList.append(baseTDP)
						cleanList.append(baseClock)
						
						#this does create a 2D list, but the other alternative would be a lot of rewriting of my CPU class, which can be done later.
						architectureCPUs.append(cleanList)
							
						
					#output = findall(r'(?!<)[^>]*(?=<)', cpuDataLine)
					#print(trimmedProcessorDataWithGarbage)
				fileLineItterator += 1
			#print(architectures)
			
		#elif(line.strip().find('<title>'):
		#all the tables start out with the "Chipset"-based architecture, so I can filter which one I am reading for without having to do more checks.
		elif (line.find('<tbody>') != -1):
			if(line.rfind('</th></tr>') != -1):
				#this regex sepperates out the chipset, ie "Zen" from "List of Zen-based EPYC Processors"
				#regex is not my speciality, so with what I could, this techincally seperates out " Zen-", so I trim it off
				#while find could be used, I am trying to minimize imports
				currentArchitecture = (findall(r'\s*.Z.*?\-', line)[0])[1:-1].strip()
				workingArchitecture = None
				#print(architectures)
				#print(currentArchitecture)
				for architecture in architectures:
					#print(architecture.getName())
					#this is ran agter the set of code labled "Chip Codenames" so the architectures exist. this just says which one is currently being worked with
					if (architecture.getName() == currentArchitecture):
						workingArchitecture = architecture
						break
				#print(workingArchitecture.getCodename())
				#this code is very specific to the AMD chips, but is fairly universal with a few changes, like to the Vendoline and the number of FLOPs per cycle, and how you decide which one is where
				cpus = []
				for cpuStat in architectureCPUs:
					cpu_vendor = "AMD"
					cpu_codename = workingArchitecture.getCodename()
					#print(cpu_codename)
					cpu_model = cpuStat[0]
					cpu_cores = cpuStat[2]
					cpu_feature_size = workingArchitecture.getSize()[0:workingArchitecture.getSize().index(' ')]
					cpu_frequency = cpuStat[-1]
					if (workingArchitecture.getName() == "Zen"):
						cpu_flops_per_cycle =  8
					else:
						cpu_flops_per_cycle = 16
					cpu_l3_cache = cpuStat[3]
					cpu_tdp = cpuStat[4]
					cpu_cost = cpuStat[1]
					newCpu = CPU(cpu_vendor, cpu_codename, cpu_model, cpu_cores, cpu_feature_size, cpu_frequency, cpu_flops_per_cycle, cpu_l3_cache, cpu_tdp, cpu_cost)
					uniqueCPU = True	
					for cpu in cpus:
						if(cpu.cpu_model == newCpu.cpu_model):
							uniqueCPU = False
					if uniqueCPU:
						cpus.append(CPU(cpu_vendor, cpu_codename, cpu_model, cpu_cores, cpu_feature_size, cpu_frequency, cpu_flops_per_cycle, cpu_l3_cache, cpu_tdp, cpu_cost))

				with open("../clusterdesign/amd-epyc-{arch}.xml".format(arch=workingArchitecture.getCodename()), 'w') as cpuOut:
					cpuOut.write('<?xml version="1.0" encoding="UTF-8"?>\n<itemslist xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="items-schema.xsd">\n')
					for cpu in cpus:
						cpuOut.write(cpu.__str__()+'\n')
					cpuOut.write('</itemslist>')
						
				#this writes it to two places, the db for the clusterdesign gui and for saddle	
				copy("../clusterdesign/amd-epyc-{arch}.xml".format(arch=workingArchitecture.getCodename()),"../saddle/db/amd-epyc-{arch}.xml".format(arch=workingArchitecture.getCodename()))
				
					
				
		#Chip Codenames
		#This works with the stuff above because this happens sooner in the website compared to the CPUs themselves. if that were to change, the individual parts of the wiki have a "See also:" section
		#and that has the code name, so if it needs ot change due to format changes in the page, it is possible to at least get code names simply, but not as simple to get the feature size
		elif(line == '<th> Codename </th>'):
			architecturesRaw = []
			fileLineItterator = 0
			while ((architectureDataLine := getline(cpuLineUp, i + fileLineItterator)).find('</table>') == -1):
				architectureLineItterator = 0
				architectureSpecsList = []
				while((architectureSpecs := getline(cpuLineUp, i + fileLineItterator + architectureLineItterator)).rfind('</td></tr>') == -1):
					if(architectureSpecs.find('<td> <a') != -1):
						trimmedArchitecture = output = findall(r'(?!<)[^>]*(?=<)', architectureSpecs)
						architectureSpecsList += listStrip(trimmedArchitecture)
					architectureLineItterator+=1
				architecturesRaw.append(architectureSpecsList)
				fileLineItterator += 1 + architectureLineItterator
			#due to how the findall above works, it does return some empty list or list where the only object is a space. this goes through and cleans all that iformation up
			architecturesRaw [:]= [y for y in architecturesRaw if listStrip(y)]
			for architecture in architecturesRaw:
				#archObj = Architecture(architecture[1],architecture[0],architecture[3])
				#print(architecture)
				architectures.append(Architecture(architecture[1],architecture[0],architecture[3]))
						
def main():
	filesOfCpuLineUps = ["quotes-epyc.html"]
	for cpuLineUp in filesOfCpuLineUps:
		cpus = []
		cpuData(cpuLineUp,cpus)

	
		
			
main()
