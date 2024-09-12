from re import findall
from shutil import copy

ddr4_3200_Mem = ["64 GB PC4-25600 1x64GB 2Rank Memory", "256 GB PC4-25600 4x64GB 2Rank Memory", "512 GB PC4-25600 8x64GB 2Rank Memory"]
ddr4_2933_Mem = ["64 GB PC4-23464 1x64GB 2Rank Memory", "256 GB PC4-23464 4x64GB 2Rank Memory", "512 GB PC4-23464 8x64GB 2Rank Memory"]

ddr5_4800_64_Mem = ["64 GB PC5-38400 1x64GB 2Rank Memory", "384 GB PC5-38400 6x64GB 2Rank Memory", "768 GB PC5-38400 12x64GB 2Rank Memory"]
ddr5_4800_32_Mem = ["32 GB PC5-38400 1x32GB 2Rank Memory", "192 GB PC5-38400 6x32GB 2Rank Memory", "384 GB PC5-38400 12x32GB 2Rank Memory"]
#stick specs DDR4 3200: https://buy.hpe.com/us/en/options/enterprise-memory/server-memory/server-memory/hpe-64gb-1x64gb-dual-rank-x4-ddr4-3200-cas-22-22-22-registered-smart-memory-kit/p/p07650-h21

#stick specs DDR4 2933: https://buy.hpe.com/us/en/options/enterprise-memory/server-memory/server-memory/hpe-64gb-1x64gb-dual-rank-x4-ddr4-2933-cas-21-21-21-registered-smart-memory-kit/p/p00930-b21

#stick specs DDR5 64gb 4800: https://harddiskdirect.com/p50312-b21-hpe-64gb-ddr5-4800mhz-pc5-38400-ecc-registered-cl40-288-pin-rdimm-1-1v-dual-rank-memory-module.html?campaign=686987897&adgrp=1315018196120914&device=c&adposition=&keyword=&utm_source=bing&utm_medium=cpc&utm_campaign=SMART%20SHOPPING%20-%2006/06/2023&utm_term=4585788126352397&utm_content=All%20Product%20Catalog
#stick specs DDR5 32GB 4800: https://buy.hpe.com/us/en/options/enterprise-memory/server-memory/server-memory/hpe-32gb-1x32gb-dual-rank-x8-ddr5-4800-cas-40-39-39-ec8-registered-smart-memory-kit/p/p50311-b21
with open("../clusterdesign/hpe_pc4_memory.xml", 'w') as ddr4:
	ddr4.write('<?xml version="1.0" encoding="UTF-8"?>\n<itemslist xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="parts-schema.xsd">\n\n')
	for memoryLine in ddr4_3200_Mem:
		memory = memoryLine.split()
		capacity = memory[0]
		memGeneration = memory[2].split('-')[0]
		memSpeed = memory[2].split('-')[1]
		form = memory[3]
		takenDimm = form.split('x')[0]

		price = 3592

		ddr4.write('<item main_memory_type="{memType}" main_memory_speed="{speed}" main_memory_registers="registered" node_power="+18" node_free_dimm_count="-{slots}" node_cost="+{cost}" node_main_memory_size="+{cap}" main_memory_layout_per_cpu="{layout}">{description}</item>\n\n'.format(memType=memGeneration, speed=memSpeed, slots=takenDimm, cost=(price*int(takenDimm)), cap=capacity, layout=form, description=memoryLine))

	ddr4.write('/n/n')
	for memoryLine in ddr4_2933_Mem:
		memory = memoryLine.split()
		capacity = memory[0]
		memGeneration = memory[2].split('-')[0]
		memSpeed = memory[2].split('-')[1]
		form = memory[3]
		takenDimm = form.split('x')[0]

		price = 1003

		ddr4.write('<item main_memory_type="{memType}" main_memory_speed="{speed}" main_memory_registers="registered" node_power="+18" node_free_dimm_count="-{slots}" node_cost="+{cost}" node_main_memory_size="+{cap}" main_memory_layout_per_cpu="{layout}">{description}</item>\n\n'.format(memType=memGeneration, speed=memSpeed, slots=takenDimm, cost=(price*int(takenDimm)), cap=capacity, layout=form, description=memoryLine))

	ddr4.write('\n\n</itemslist>')

with open('../clusterdesign/hpe_pc5_memory.xml', 'w') as ddr5:
	ddr5.write('<?xml version="1.0" encoding="UTF-8"?>\n<itemslist xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="parts-schema.xsd">\n\n')
	for memoryLine in ddr5_4800_64_Mem:
		memory = memoryLine.split()
		capacity = memory[0]
		memGeneration = memory[2].split('-')[0]
		memSpeed = memory[2].split('-')[1]
		form = memory[3]
		takenDimm = form.split('x')[0]

		price = 499

		ddr5.write('<item main_memory_type="{memType}" main_memory_speed="{speed}" main_memory_registers="registered" node_power="+18" node_free_dimm_count="-{slots}" node_cost="+{cost}" node_main_memory_size="+{cap}" main_memory_layout_per_cpu="{layout}">{description}</item>\n\n'.format(memType=memGeneration, speed=memSpeed, slots=takenDimm, cost=(price*int(takenDimm)), cap=capacity, layout=form, description=memoryLine))

	for memoryLine in ddr5_4800_32_Mem:
		memory = memoryLine.split()
		capacity = memory[0]
		memGeneration = memory[2].split('-')[0]
		memSpeed = memory[2].split('-')[1]
		form = memory[3]
		takenDimm = form.split('x')[0]

		price = 634

		ddr5.write('<item main_memory_type="{memType}" main_memory_speed="{speed}" main_memory_registers="registered" node_power="+18" node_free_dimm_count="-{slots}" node_cost="+{cost}" node_main_memory_size="+{cap}" main_memory_layout_per_cpu="{layout}">{description}</item>\n\n'.format(memType=memGeneration, speed=memSpeed, slots=takenDimm, cost=(price*int(takenDimm)), cap=capacity, layout=form, description=memoryLine))
	ddr5.write('\n\n</itemslist>')


