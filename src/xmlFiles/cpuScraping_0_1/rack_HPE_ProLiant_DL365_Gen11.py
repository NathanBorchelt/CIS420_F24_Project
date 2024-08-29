#Code works as of June 28, 2023
#for Python Version 3.8.14

#This creates the different potential configs for the HPE ProLiant DL365 Gen11
import RackUnit
#fileinput can be reimported, with some extra code to help specify which chip goes where
#to read from multiple source files
#import fileinput
#from linecache import getline
from re import findall
from shutil import copy

#Due ot the stats of the blade not needint to be actually processed, strings are fine and a little easier to process

#PLEASE NOTE AS OF JUNE 28th, I HAVE NOT ADDED ACTUAL INTERCONNECTS 
#THERE IS NOT ONE BUILT INTO THE SERVER BUT I PUT FILLER DATA FOR TESTING PURPOSES

#Code designed for HPE ProLiant DL365 Gen11
#https://buy.hpe.com/us/en/compute/rack-servers/proliant-dl300-servers/proliant-dl365-server/hpe-proliant-dl365-gen11/p/1014689131
#
#A rough price for just the rack and redundent power supply, excluding cpus and memories comes out to
#about $5714 that does take osme guessing by comparing listed prices of items and the changes from add-ons
#other stats pulled from https://www.hpe.com/psnow/doc/a50004299enw.html?jumpid=in_pdp-psnow-qs
NODE_COST = "5714"

#weight is based off fully loaded in kg rounded, 18kg, based off wht is deamed the SFF Maximum
NODE_WEIGHT = "18"

#making a guess based of what I remember from seeing the racks, with them being about 6ft tall,
#making me guess that they are 42U, change this variable to change what they actually would be
RACK_HEIGHT = "42" #enclosure size in the xml

#DL365s are 1U tall
NODE_HEIGHT = 1
#node_equipment_size in the xml

#node_model, the name of the Unit
#node_vendor, the vendor
NODE_VENDOR = "HPE"
NODE_MODEL = NODE_VENDOR+" ProLiant DL365 Gen11"
NODE_FORM_FACTOR = NODE_VENDOR+" ProLiant"

#node_free-dimm_count, number of dimm/ram slots
NODE_DIMM_SLOTS = "24"

#I have 0 clue on how to calculate this, so 100W idle draw is a random number
#please change if you know better, or give me some stats
NODE_POWER = 100 

#cpuNumber = ["CPU2_9004_SERIES", "CPU1_9004_SERIES"]
CPU1 = "CPU1_9004_SERIES"
CPU2 = "CPU2_9004_SERIES"

def main():
	with open("../clusterdesign/HPE_DL365_Server.xml",'w') as dl365:
		#baseline text for xml file
		boilerplate = ['<?xml version="1.0" encoding="UTF-8"?>',
			'<itemslist xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="items-schema.xsd">',
			'<item node_model="{mdl}" node_vendor="{vnd}" node_form_factor="{form}" node_cost="+{cost}" node_power="{pwr}" node_weight="{wgt}" node_equipment_size="{n_hgt}" enclosure_size="{r_hgt}" node_free_dimm_count="{fds}">{mdl}</item>'.format(mdl=NODE_MODEL, vnd=NODE_VENDOR, form=NODE_FORM_FACTOR, cost=NODE_COST, pwr=NODE_POWER, wgt=NODE_WEIGHT, n_hgt=NODE_HEIGHT, r_hgt=RACK_HEIGHT, fds=NODE_DIMM_SLOTS),
			'<edge from="Start" to="{}"></edge>'.format(NODE_MODEL),
			'\n'
			]
			
		for text in boilerplate:
			dl365.write(text+'\n')
		
		#This is node is for AMD 9xx4 or 9xx4F, while it can do 9xx4P, those are single CPU chips EPYCs
		with open("../clusterdesign/amd-epyc.xml") as epycChips:
			for processor in epycChips:
				if ("<item " in processor):
					chip = findall(r'(?!<)[^>]*(?=<)', processor)[0]
					chipNumber = chip.split(" ")[2]
					isDualCPU = chipNumber[-1] != 'P'
					isZen4 = (chipNumber[3] == '4') and (chipNumber[0] == '9')
					if(isDualCPU and isZen4):
						dl365.write('<edge from="{mdl}" to="{chip}" to-partition="{cpu}"></edge>\n'.format(mdl=NODE_MODEL, chip=chip, cpu=CPU1))
						
				#I know that it is more efficient to do one loop,but for testing of files
				#to mimic the orginal, I am doing on CPU1 then CPU2
		dl365.write('\n\n')
		with open("../clusterdesign/amd-epyc.xml") as epycChips:
			for processor in epycChips:
				if ("<item " in processor):
					chip = findall(r'(?!<)[^>]*(?=<)', processor)[0]
					chipNumber = chip.split(" ")[2]
					isDualCPU = chipNumber[-1] != 'P'
					isZen4 = (chipNumber[3] == '4') and (chipNumber[0] == '9')
					if(isDualCPU and isZen4):
						dl365.write('<edge from="{chip}" from-partition="{cpu1}" to-partition="{cpu2}"></edge>\n'.format(chip=chip, cpu1=CPU1, cpu2=CPU2))
						
		dl365.write('\n<item>END_OF_{cpu}</item>\n<connect from-partition="{cpu}" to="END_OF_{cpu}"></connect>\n\n'.format(cpu = CPU2))
		
		
		memoryVersion = ["32 GB PC5-38400 1x32GB 1Rank Memory", "192 GB PC5-38400 6x32GB 1Rank Memory", "384 GB PC5-38400 12x32GB 1Rank Memory"]
		for memory in memoryVersion:
			dl365.write('<edge from="END_OF_{cpu}" to="{mem}" to-partition="RAM_FOR_{cpu}"></edge>\n'.format(cpu=CPU2, mem=memory))
			
		dl365.write('\n\n')
		for memory in memoryVersion:
			dl365.write('<edge from="{mem}" from-partition="RAM_FOR_{cpu2}" to-partition="RAM_FOR_{cpu1}"></edge>\n'.format(mem=memory, cpu1 = CPU1, cpu2 = CPU2))
			
		dl365.write('\n\n')
		dl365.write('<connect from-partition="{cpu}" to-partition="RAM_FOR_{cpu}"></connect>\n'.format(cpu=CPU1))
					
		dl365.write('<edge from="Built-In 10GbE NC551i FlexFabric 2 Ports" to="Pre-end"></edge>\n<edge from="Built-In 10GbE NC551i FlexFabric 2 Ports" to="HP 4X QDR IB CX2 Dual Port Mezz HCA"></edge>\n<edge from="HP 4X QDR IB CX2 Dual Port Mezz HCA" to="Pre-end"></edge>\n</itemslist>\n')
				
		copy("../clusterdesign/HPE_DL365_Server.xml","../saddle/db/HPE_DL365_Server.xml")

main()				
	
	
	
