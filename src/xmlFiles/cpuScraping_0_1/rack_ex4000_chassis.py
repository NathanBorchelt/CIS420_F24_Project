#Code works as of June 28, 2023
#for Python Version 3.8.14

#Chat session id: 3797511150
#EX4000 chassis price @ 15 chassis

import RackUnit

from re import findall
from shutil import copy


NODE_COST = 5200
NODE_WEIGHT = 18

RACK_HEIGHT = 48
COMPUTE_NODES_PER_UNIT = 4
NODES_PER_UNIT = 8
NODE_HEIGHT = round(RACK_HEIGHT/(COMPUTE_NODES_PER_UNIT * NODES_PER_UNIT),3)

NODE_VENDOR = "HPE"
NODE_MODEL = NODE_VENDOR + " Cray EX"
NODE_FORM_FACTOR = NODE_VENDOR + " Cray EX4000"

NODE_IDLE = 100

CPU1 = ['CPU1_7002_SERIES', 'CPU1_7003_SERIES', 'CPU1_9004_SERIES', 'CPU1_XEON4_SERIES']
CPU2 = ['CPU2_7002_SERIES', 'CPU2_7003_SERIES', 'CPU2_9004_SERIES', 'CPU2_XEON4_SERIES']


cpuClasses = ['Rome', 'Milan', 'Genoa', 'Sapphire']


ddr4Memory = ["64 GB PC4-25600 1x64GB 2Rank Memory", "256 GB PC4-25600 4x64GB 2Rank Memory", "512 GB PC4-25600 8x64GB 2Rank Memory", "64 GB PC4-23464 1x64GB 2Rank Memory", "256 GB PC4-23464 4x64GB 2Rank Memory", "512 GB PC4-23464 8x64GB 2Rank Memory"]

ddr5Memory_12dimm = ["64 GB PC5-38400 1x64GB 2Rank Memory", "384 GB PC5-38400 6x64GB 2Rank Memory", "768 GB PC5-38400 12x64GB 2Rank Memory"]
ddr5Memory_8dimm = ["64 GB PC5-38400 1x64GB 2Rank Memory", "256 GB PC5-38400 4x64GB 2Rank Memory", "512 GB PC5-38400 8x64GB 2Rank Memory"]


with open("../clusterdesign/HPE_EX4000.xml",'w') as ex4000:
	#boilerplate
	ex4000.write('<?xml version="1.0" encoding="UTF-8"?>\n')
	ex4000.write('<itemslist xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="items-schema.xsd">\n\n')

	#cpu info
	ex4000.write('<include>amd-epyc-Rome.xml</include>\n')
	ex4000.write('<include>amd-epyc-Milan.xml</include>\n')
	ex4000.write('<include>amd-epyc-Genoa.xml</include>\n')
	ex4000.write('<include>intel_xeon_gen4.xml</include>\n')

	#memory info
	ex4000.write('<include>hpe_pc4_memory.xml</include>\n')
	ex4000.write('<include>hpe_pc5_memory.xml</include>\n')

	#temp netowrk still
	ex4000.write('<include>slingshot.xml</include>\n')

	ex4000.write('<item>Start</item>\n')
	ex4000.write('<item>End</item>\n')

	ex4000.write('<item node_peak_performance="cpu_cores * node_cpu_count * cpu_frequency * cpu_flops_per_cycle" node_main_memory_per_core="node_main_memory_size / cpu_cores / node_cpu_count">Update characteristics</item>\n')
	ex4000.write('<item>Pre-end</item>\n')
	ex4000.write('<edge from="Pre-end" to="Update characteristics"></edge>\n')
	ex4000.write('<edge from="Update characteristics" to="End"></edge>\n')

	ex4000.write('<include>HPE_EX425_Rome.xml</include>\n')
	ex4000.write('<include>HPE_EX425_Milan.xml</include>\n')
	ex4000.write('<include>HPE_EX4252_Genoa.xml</include>\n')
	ex4000.write('<include>HPE_EX420_Sapphire.xml</include>\n')

	ex4000.write('</itemslist>')

for cpuSeries in range(len(cpuClasses)):
	if cpuClasses[cpuSeries] == 'Rome':
		dimmSlots = 8
		NODE_MODEL = NODE_VENDOR + " Cray EX425 Rome"
		with open('../clusterdesign/HPE_EX425_Rome.xml','w') as rome:
			rome.write('<?xml version="1.0" encoding="UTF-8"?>\n')
			rome.write('<itemslist xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="items-schema.xsd">\n')
			rome.write('<item node_model="{mdl}" node_vendor="{vnd}" node_form_factor="{form}" node_cost="+{cost}" node_power="{pwr}" node_weight="{wgt}" node_equipment_size="{n_hgt}" enclosure_size="{r_hgt}" node_free_dimm_count="{fds}">{mdl}</item>\n\n'.format(mdl=NODE_MODEL, vnd=NODE_VENDOR, form=NODE_FORM_FACTOR, cost=NODE_COST, pwr=NODE_IDLE, wgt=NODE_WEIGHT, n_hgt=NODE_HEIGHT, r_hgt=RACK_HEIGHT, fds=dimmSlots * 2))
			rome.write('<edge from="Start" to="{}"></edge>\n\n'.format(NODE_MODEL))

			#This is node is for AMD 7xx2 or 7xx2F, while it can do 7xx2P, those are single CPU chips EPYCs, let me know if we want those, and it is just a bit more logic
			with open('../clusterdesign/amd-epyc-Rome.xml', 'r') as epyc7002:
				allCPUS = []
				for processor in epyc7002:
					if ("<item " in processor):
						chip = findall(r'(?!<)[^>]*(?=<)', processor)[0]
						chipNumber = chip.split(" ")[2]
						if( not (chipNumber in allCPUS)):
							allCPUS.append(chip)
							isDualCPU = (chipNumber[-1] != 'P') #seperate out single CPUs
							isCorrect = (chipNumber[3] == '2') and (chipNumber[0] == '7') #filter to make sure XML is only Rome CPUs
							if(isDualCPU and isCorrect):

								rome.write('<edge from="{mdl}" to="{chip}" to-partition="{cpu}"></edge>\n'.format(mdl=NODE_MODEL, chip=chip, cpu=CPU1[cpuSeries]))

			rome.write('\n\n')
			#while it is more efficient to do this all in one loop, I am unsure about the XML reading nicely, so I am itterating through multiple times to keep the format of the original XMLs
			with open('../clusterdesign/amd-epyc-Rome.xml', 'r') as epyc7002:
				allCPUS = []
				for processor in epyc7002:
					if ("<item " in processor):
						chip = findall(r'(?!<)[^>]*(?=<)', processor)[0]
						chipNumber = chip.split(" ")[2]
						if( not (chipNumber in allCPUS)):
							allCPUS.append(chip)
							isDualCPU = (chipNumber[-1] != 'P') #seperate out single CPUs
							isCorrect = (chipNumber[3] == '2') and (chipNumber[0] == '7') #filter to make sure XML is only Rome CPUs
							if(isDualCPU and isCorrect):
								rome.write('<edge from="{chip}" from-partition="{cpu1}" to-partition="{cpu2}"></edge>\n'.format(chip=chip, cpu1=CPU1[cpuSeries], cpu2=CPU2[cpuSeries]))

			rome.write('\n<item>END_OF_{cpu}</item>\n<connect from-partition="{cpu}" to="END_OF_{cpu}"></connect>\n\n'.format(cpu = CPU2[cpuSeries]))

			for memory in ddr4Memory:
				rome.write('<edge from="END_OF_{cpu}" to="{mem}" to-partition="RAM_FOR_{cpu}"></edge>\n'.format(cpu=CPU2[cpuSeries], mem=memory))

			rome.write('\n\n')
			for memory in ddr4Memory:
				rome.write('<edge from="{mem}" from-partition="RAM_FOR_{cpu2}" to-partition="RAM_FOR_{cpu1}"></edge>\n'.format(mem=memory, cpu1 = CPU1[cpuSeries], cpu2 = CPU2[cpuSeries]))


			rome.write('\n\n')
			rome.write('<connect from-partition="{cpu}" to-partition="RAM_FOR_{cpu}"></connect>\n'.format(cpu=CPU1[cpuSeries]))

			rome.write('<connect from-partition="RAM_FOR_{cpu}" to="Built-In HPE Slingshot 200Gb 2 Ports"></connect>\n'.format(cpu=CPU1[cpuSeries]))

			rome.write('<edge from="Built-In HPE Slingshot 200Gb 2 Ports" to="Pre-end"></edge>\n<edge from="Built-In HPE Slingshot 200Gb 2 Ports" to="HPE Slingshot Switch Blade"></edge>\n<edge from="HPE Slingshot Switch Blade" to="Pre-end"></edge>\n</itemslist>\n')

			copy("../clusterdesign/HPE_EX425_Rome.xml","../saddle/db/HPE_EX425_Rome.xml")

	if cpuClasses[cpuSeries] == 'Milan':
		dimmSlots = 8
		NODE_MODEL = NODE_VENDOR + " Cray EX425 Milan"
		with open('../clusterdesign/HPE_EX425_Milan.xml','w') as milan:
			milan.write('<?xml version="1.0" encoding="UTF-8"?>\n')
			milan.write('<itemslist xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="items-schema.xsd">\n')
			milan.write('<item node_model="{mdl}" node_vendor="{vnd}" node_form_factor="{form}" node_cost="+{cost}" node_power="{pwr}" node_weight="{wgt}" node_equipment_size="{n_hgt}" enclosure_size="{r_hgt}" node_free_dimm_count="{fds}">{mdl}</item>\n\n'.format(mdl=NODE_MODEL, vnd=NODE_VENDOR, form=NODE_FORM_FACTOR, cost=NODE_COST, pwr=NODE_IDLE, wgt=NODE_WEIGHT, n_hgt=NODE_HEIGHT, r_hgt=RACK_HEIGHT, fds=dimmSlots * 2))
			milan.write('<edge from="Start" to="{}"></edge>\n\n'.format(NODE_MODEL))

			#This is node is for AMD 7xx3 or 7xx3F, while it can do 7xx3P, those are single CPU chips EPYCs, let me know if we want those, and it is just a bit more logic
			with open('../clusterdesign/amd-epyc-Milan.xml', 'r') as epyc7003:
				allCPUS = []
				for processor in epyc7003:
					if ("<item " in processor):
						chip = findall(r'(?!<)[^>]*(?=<)', processor)[0]
						chipNumber = chip.split(" ")[2]
						if( not (chipNumber in allCPUS)):
							allCPUS.append(chip)
							isDualCPU = (chipNumber[-1] != 'P') #seperate out single CPUs
							isCorrect = (chipNumber[3] == '3') and (chipNumber[0] == '7') #filter to make sure XML is only Millan CPUs
							if(isDualCPU and isCorrect):
								milan.write('\t<edge from="{mdl}" to="{chip}" to-partition="{cpu}"></edge>\n'.format(mdl=NODE_MODEL, chip=chip, cpu=CPU1[cpuSeries]))
			milan.write('\n\n')
			#while it is more efficient to do this all in one loop, I am unsure about the XML reading nicely, so I am itterating through multiple times to keep the format of the original XMLs
			with open('../clusterdesign/amd-epyc-Milan.xml', 'r') as epyc7003:
				allCPUS = []
				for processor in epyc7003:
					if ("<item " in processor):
						chip = findall(r'(?!<)[^>]*(?=<)', processor)[0]
						chipNumber = chip.split(" ")[2]
						if( not (chipNumber in allCPUS)):
							allCPUS.append(chip)
							isDualCPU = (chipNumber[-1] != 'P') #seperate out single CPUs
							isCorrect = (chipNumber[3] == '3') and (chipNumber[0] == '7') #filter to make sure XML is only Millan CPUs
							if(isDualCPU and isCorrect):
								milan.write('<edge from="{chip}" from-partition="{cpu1}" to-partition="{cpu2}"></edge>\n'.format(chip=chip, cpu1=CPU1[cpuSeries], cpu2=CPU2[cpuSeries]))

			milan.write('\n<item>END_OF_{cpu}</item>\n<connect from-partition="{cpu}" to="END_OF_{cpu}"></connect>\n\n'.format(cpu = CPU2[cpuSeries]))

			for memory in ddr4Memory:
				milan.write('<edge from="END_OF_{cpu}" to="{mem}" to-partition="RAM_FOR_{cpu}"></edge>\n'.format(cpu=CPU2[cpuSeries], mem=memory))

			milan.write('\n\n')
			for memory in ddr4Memory:
				milan.write('<edge from="{mem}" from-partition="RAM_FOR_{cpu2}" to-partition="RAM_FOR_{cpu1}"></edge>\n'.format(mem=memory, cpu1 = CPU1[cpuSeries], cpu2 = CPU2[cpuSeries]))


			milan.write('\n\n')
			milan.write('<connect from-partition="{cpu}" to-partition="RAM_FOR_{cpu}"></connect>\n'.format(cpu=CPU1[cpuSeries]))

			milan.write('<connect from-partition="RAM_FOR_{cpu}" to="Built-In HPE Slingshot 200Gb 2 Ports"></connect>\n'.format(cpu=CPU1[cpuSeries]))

			#milan.write('<edge from="Built-In HPE Slingshot 200Gb 2 Ports" to="Pre-end"></edge>\n<edge from="Built-In HPE Slingshot 200Gb 2 Ports" to="HPE Slingshot Switch Blade"></edge>\n<edge from="HPE Slingshot Switch Blade" to="Pre-end"></edge>\n</itemslist>\n')
			milan.write('</itemslist>\n')
			copy("../clusterdesign/HPE_EX425_Milan.xml","../saddle/db/HPE_EX425_Milan.xml")



	if cpuClasses[cpuSeries] == 'Genoa':
		dimmSlots = 12
		NODE_MODEL = NODE_VENDOR + " Cray EX4252 Genoa"
		with open('../clusterdesign/HPE_EX4252_Genoa.xml','w') as genoa:
			genoa.write('<?xml version="1.0" encoding="UTF-8"?>\n')
			genoa.write('<itemslist xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="items-schema.xsd">\n')
			genoa.write('<item node_model="{mdl}" node_vendor="{vnd}" node_form_factor="{form}" node_cost="+{cost}" node_power="{pwr}" node_weight="{wgt}" node_equipment_size="{n_hgt}" enclosure_size="{r_hgt}" node_free_dimm_count="{fds}">{mdl}</item>\n\n'.format(mdl=NODE_MODEL, vnd=NODE_VENDOR, form=NODE_FORM_FACTOR, cost=NODE_COST, pwr=NODE_IDLE, wgt=NODE_WEIGHT, n_hgt=NODE_HEIGHT, r_hgt=RACK_HEIGHT, fds=dimmSlots * 2))
			genoa.write('<edge from="Start" to="{}"></edge>\n\n'.format(NODE_MODEL))

			#This is node is for AMD 9xx4 or 9xx4F, while it can do 9xx4P, those are single CPU chips EPYCs, let me know if we want those, and it is just a bit more logic
			with open('../clusterdesign/amd-epyc-Genoa.xml', 'r') as epyc9004:
				allCPUS = []
				for processor in epyc9004:
					if ("<item " in processor):
						chip = findall(r'(?!<)[^>]*(?=<)', processor)[0]
						chipNumber = chip.split(" ")[2]
						if( not (chipNumber in allCPUS)):
							allCPUS.append(chip)
							isDualCPU = (chipNumber[-1] != 'P') #seperate out single CPUs
							isCorrect = (chipNumber[3] == '4') and (chipNumber[0] == '9') #filter to make sure XML is only Genoa CPUs
							if(isDualCPU and isCorrect):

								genoa.write('<edge from="{mdl}" to="{chip}" to-partition="{cpu}"></edge>\n'.format(mdl=NODE_MODEL, chip=chip, cpu=CPU1[cpuSeries]))

			genoa.write('\n\n')
			#while it is more efficient to do this all in one loop, I am unsure about the XML reading nicely, so I am itterating through multiple times to keep the format of the original XMLs
			with open('../clusterdesign/amd-epyc-Genoa.xml', 'r') as epyc9004:
				allCPUS = []
				for processor in epyc9004:
					if ("<item " in processor):
						chip = findall(r'(?!<)[^>]*(?=<)', processor)[0]
						chipNumber = chip.split(" ")[2]
						if( not (chipNumber in allCPUS)):
							allCPUS.append(chip)
							isDualCPU = (chipNumber[-1] != 'P') #seperate out single CPUs
							isCorrect = (chipNumber[3] == '4') and (chipNumber[0] == '9') #filter to make sure XML is only Genoa CPUs
							if(isDualCPU and isCorrect):
								genoa.write('<edge from="{chip}" from-partition="{cpu1}" to-partition="{cpu2}"></edge>\n'.format(chip=chip, cpu1=CPU1[cpuSeries], cpu2=CPU2[cpuSeries]))

			genoa.write('\n<item>END_OF_{cpu}</item>\n<connect from-partition="{cpu}" to="END_OF_{cpu}"></connect>\n\n'.format(cpu = CPU2[cpuSeries]))

			for memory in ddr5Memory_12dimm:
				genoa.write('<edge from="END_OF_{cpu}" to="{mem}" to-partition="RAM_FOR_{cpu}"></edge>\n'.format(cpu=CPU2[cpuSeries], mem=memory))

			genoa.write('\n\n')
			for memory in ddr5Memory_12dimm:
				genoa.write('<edge from="{mem}" from-partition="RAM_FOR_{cpu2}" to-partition="RAM_FOR_{cpu1}"></edge>\n'.format(mem=memory, cpu1 = CPU1[cpuSeries], cpu2 = CPU2[cpuSeries]))


			genoa.write('\n\n')
			genoa.write('<connect from-partition="{cpu}" to-partition="RAM_FOR_{cpu}"></connect>\n'.format(cpu=CPU1[cpuSeries]))

			genoa.write('<connect from-partition="RAM_FOR_{cpu}" to="Built-In HPE Slingshot 200Gb 2 Ports"></connect>\n'.format(cpu=CPU1[cpuSeries]))

			genoa.write('</itemslist>\n')

			#genoa.write('<edge from="Built-In HPE Slingshot 200Gb 2 Ports" to="Pre-end"></edge>\n<edge from="Built-In HPE Slingshot 200Gb 2 Ports" to="HPE Slingshot Switch Blade"></edge>\n<edge from="HPE Slingshot Switch Blade" to="Pre-end"></edge>\n</itemslist>\n')

			copy("../clusterdesign/HPE_EX4252_Genoa.xml","../saddle/db/HPE_EX4252_Genoa.xml")


	if cpuClasses[cpuSeries] == 'Sapphire':
		dimmSlots = 8
		NODE_MODEL = NODE_VENDOR + " Cray EX420 Sapphire"
		with open('../clusterdesign/HPE_EX420_Sapphire.xml','w') as sapphire:
			sapphire.write('<?xml version="1.0" encoding="UTF-8"?>\n')
			sapphire.write('<itemslist xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="items-schema.xsd">\n')
			sapphire.write('<item node_model="{mdl}" node_vendor="{vnd}" node_form_factor="{form}" node_cost="+{cost}" node_power="{pwr}" node_weight="{wgt}" node_equipment_size="{n_hgt}" enclosure_size="{r_hgt}" node_free_dimm_count="{fds}">{mdl}</item>\n\n'.format(mdl=NODE_MODEL, vnd=NODE_VENDOR, form=NODE_FORM_FACTOR, cost=NODE_COST, pwr=NODE_IDLE, wgt=NODE_WEIGHT, n_hgt=NODE_HEIGHT, r_hgt=RACK_HEIGHT, fds=dimmSlots * 2))
			sapphire.write('<edge from="Start" to="{}"></edge>\n\n'.format(NODE_MODEL))

			#This is node is for AMD 9xx4 or 9xx4F, while it can do 9xx4P, those are single CPU chips EPYCs, let me know if we want those, and it is just a bit more logic
			with open('../clusterdesign/intel_xeon_gen4.xml', 'r') as xeonG4:
				allCPUS = []
				for processor in xeonG4:
					if ("<item " in processor):
						chip = findall(r'(?!<)[^>]*(?=<)', processor)[0]
						chipNumber = chip.split(" ")[-1]
						if( not (chipNumber in allCPUS)):
							sapphire.write('<edge from="{mdl}" to="{chip}" to-partition="{cpu}"></edge>\n'.format(mdl=NODE_MODEL, chip=chip, cpu=CPU1[cpuSeries]))

			sapphire.write('\n\n')
			#while it is more efficient to do this all in one loop, I am unsure about the XML reading nicely, so I am itterating through multiple times to keep the format of the original XMLs
			with open('../clusterdesign/intel_xeon_gen4.xml', 'r') as xeonG4:
				allCPUS = []
				for processor in xeonG4:
					if ("<item " in processor):
						chip = findall(r'(?!<)[^>]*(?=<)', processor)[0]
						chipNumber = chip.split(" ")[-1]
						if( not (chipNumber in allCPUS)):
							sapphire.write('<edge from="{chip}" from-partition="{cpu1}" to-partition="{cpu2}"></edge>\n'.format(chip=chip, cpu1=CPU1[cpuSeries], cpu2=CPU2[cpuSeries]))

			sapphire.write('\n<item>END_OF_{cpu}</item>\n<connect from-partition="{cpu}" to="END_OF_{cpu}"></connect>\n\n'.format(cpu = CPU2[cpuSeries]))

			for memory in ddr5Memory_8dimm:
				sapphire.write('<edge from="END_OF_{cpu}" to="{mem}" to-partition="RAM_FOR_{cpu}"></edge>\n'.format(cpu=CPU2[cpuSeries], mem=memory))

			sapphire.write('\n\n')
			for memory in ddr5Memory_8dimm:
				sapphire.write('<edge from="{mem}" from-partition="RAM_FOR_{cpu2}" to-partition="RAM_FOR_{cpu1}"></edge>\n'.format(mem=memory, cpu1 = CPU1[cpuSeries], cpu2 = CPU2[cpuSeries]))


			sapphire.write('\n\n')
			sapphire.write('<connect from-partition="{cpu}" to-partition="RAM_FOR_{cpu}"></connect>\n'.format(cpu=CPU1[cpuSeries]))

			sapphire.write('<connect from-partition="RAM_FOR_{cpu}" to="Built-In HPE Slingshot 200Gb 2 Ports"></connect>\n'.format(cpu=CPU1[cpuSeries]))

			#genoa.write('<edge from="Built-In HPE Slingshot 200Gb 2 Ports" to="Pre-end"></edge>\n<edge from="Built-In HPE Slingshot 200Gb 2 Ports" to="HPE Slingshot Switch Blade"></edge>\n<edge from="HPE Slingshot Switch Blade" to="Pre-end"></edge>\n</itemslist>\n')
			sapphire.write('</itemslist>\n')

			copy("../clusterdesign/HPE_EX420_Sapphire.xml","../saddle/db/HPE_EX420_Sapphire.xml")














































