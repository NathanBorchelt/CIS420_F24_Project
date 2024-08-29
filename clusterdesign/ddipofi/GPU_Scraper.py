import requests

class GPU:
	vendor = "N/A"
	model = "N/A"
	architecture = "N/A"
	cores = "N/A" #1 amd compute unit = 64 cores
	core_frequency = "N/A"
	vram = "N/A"
	flops_per_cycle = "N/A" #flops / (cores *  freq)
	codename = "N/A"
	l2_cache = "N/A"
	tdp = "N/A"
	acp = "N/A" #tdp / 0.7
	node_power = "N/A" #"+" tdp
	node_count = "+1" #always 1
	feature_size = "N/A"
	node_cost = "N/A"

	def create_xml_statement(self):
		return "<item gpu_vendor=\"{0}\" gpu_codename=\"{1}\" gpu_model=\"{2}\" gpu_architecture=\"{3}\" gpu_cores=\"{4}\" gpu_feature_size=\"{5}\" gpu_core_frequency=\"{6}\"  gpu_vram=\"{7}\" gpu_flops_per_cycle=\"{8}\" gpu_l2_cache=\"{9}\" gpu_acp=\"{10}\" gpu_tdp=\"{11}\" node_power=\"{12}\" node_gpu_count=\"{13}\" node_cost=\"{14}\">{15} {16}</item>".format(self.vendor, self.codename, self.model, self.architecture, self.cores, self.feature_size, self.core_frequency, self.vram, self.flops_per_cycle, self.l2_cache, self.acp, self.tdp, self.node_power, self.node_count, self.node_cost, self.vendor, self.model)

#values that arent in wiki
class Nvidia_DGX_GPU(GPU):
	H100 = {
		"cost" : "$40000"
	}	
	A100_80GB = {
		"cost" : "$15000"
	}
	A100_40GB = {
		"cost" : "$7000"
	}
	V100_32GB = {
		"cost" : "$4500"
	}
	V100_16GB = {
		"cost" : "$2000"
	}
	P100 = {
		"cost" : "$500"
	}
	
#values that arent in wiki
class AMD_Instinct_GPU(GPU): #TODO no information available for most gpus
	MI6 = {
		"cost" : "$1",
		"coreFreq" : "32 MHz",
		"l2Cache" : "1000 KB"
	}
	MI8 = {
		"cost" : "$2",
		"coreFreq" : "33 MHz",
		"l2Cache" : "1001 KB"
	}
	MI25 = {
		"cost" : "$3",
		"coreFreq" : "34 MHz",
		"l2Cache" : "1002 KB"
	}
	MI50 = {
		"cost" : "$4",
		"coreFreq" : "35 MHz",
		"l2Cache" : "1003 KB"
	}
	MI60 = {
		"cost" : "$5",
		"coreFreq" : "36 MHz",
		"l2Cache" : "1004 KB"
	}
	MI100 = {
		"cost" : "$6",
		"coreFreq" : "1502 MHz",
		"l2Cache" : "1005 KB"
	}
	MI210 = {
		"cost" : "$7",
		"coreFreq" : "38 MHz",
		"l2Cache" : "1006 KB"
	}
	MI250 = {
		"cost" : "$8",
		"coreFreq" : "1700 MHz",
		"l2Cache" : "1007 KB"
	}
	MI250X = {
		"cost" : "$9",
		"coreFreq" : "1700 MHz",
		"l2Cache" : "1008 KB"
	}

def skip_line(source, lines):
	for x in range(1, lines):
		source.readline()
	return source.readline()

def main():
	#urls of websites to be scraped
	urls = {
		"Nvidia_DGX" : "https://en.wikipedia.org/wiki/Nvidia_DGX",
		"AMD_Instinct" : "https://en.wikipedia.org/wiki/AMD_Instinct"
	}
	
	for url in urls:
		f = open(url + ".xml", "w")
		r = requests.get(urls.get(url))
		print(r.text, file=f)
		f.close()
		gpus = []
		vendor = url[0:url.find("_")]
		
		f = open(url + ".xml", "r")
		if vendor.lower() == "Nvidia".lower():
			CONST_LABEL_START = "<th style=\"white-space:nowrap;\"><br /><br />Accelerator\n"
			CONST_LABEL_I = "<th style=\"white-space:nowrap;\""
			CONST_LABEL_END = "</th></tr></tbody></table>\n"
			CONST_ENTRY_START = "<td><a href"
			i = 0
			
			for line in f:
				#get vendor and model
				if line == CONST_LABEL_START:
					while line != CONST_LABEL_END:
						line = f.readline()
						if line[0:31] == CONST_LABEL_I:
							gpu = Nvidia_DGX_GPU()
							gpus.append(gpu)
							gpus[i].vendor = vendor
							gpus[i].model = line[32:-1].replace("&#x200b;", "")
							i += 1
					i = 0

				if line[0:11] == CONST_ENTRY_START:
					#set cost
					model = gpus[i].model.replace(" ", "_")
					gpus[i].node_cost = getattr(gpus[i], model).get("cost").replace("$", "+")

					#get architecture
					startIndex = line.find("\">") + 2
					gpus[i].architecture = line[startIndex:-10]
					line = skip_line(f, 3)
					
					#get cores
					gpus[i].cores = line[4:-6]
					line = skip_line(f, 3)
					
					#get core frequency
					gpus[i].core_frequency = line[4:-6].replace("&#160;", " ")
					line = skip_line(f, 4)
					
					#get vram
					gpus[i].vram = line[4:-6]
					line = skip_line(f, 2)
					
					#get flops
					flops = float(line[4:-13]) * 1e12
					cores = int(gpus[i].cores)
					clock = int(gpus[i].core_frequency[0:-5]) * 1e6
					gpus[i].flops_per_cycle = str(round(flops / (cores * clock)))
					line = skip_line(f, 10)
					
					#get codename
					gpus[i].codename = line[4:-6]
					line = skip_line(f, 2)
					
					#get l2 cache
					gpus[i].l2_cache = line[4:-6]
					line = f.readline()
					
					#get power
					gpus[i].tdp = line[4:-6]
					gpus[i].acp = str(round(int(line[4:-7]) * 0.7)) + "W"
					gpus[i].node_power = "+" + gpus[i].tdp
					line = skip_line(f, 3)
					
					#get lithography
					endIndex = line.find("nm") + 2
					gpus[i].feature_size = line[9:endIndex].replace("&#160;", " ")
					
					i += 1
		elif vendor.lower() == "AMD".lower():
			CONST_TABLE_START = "<th>TBP Peak\n"
			CONST_TABLE_END = "</td></tr></tbody></table>\n"
			CONST_ENTRY_START = "<tr>\n"
			i = 0
			line = f.readline()
			
			while line:
				if line == CONST_TABLE_START:
					tableStart = f.tell()
					while line != CONST_TABLE_END:
						line = f.readline()
						if line == CONST_ENTRY_START:
							line = f.readline()
							gpu = AMD_Instinct_GPU()
							gpus.append(gpu)
							#get vendor
							gpus[i].vendor = vendor
							
							#get model
							gpus[i].model = line[4:-1]
							line = skip_line(f, 2)
							
							#set cost
							model = gpus[i].model
							gpus[i].node_cost = getattr(gpus[i], model).get("cost").replace("$", "+")
							
							#set core frequency
							gpus[i].core_frequency = getattr(gpus[i], model).get("coreFreq")
							
							#set l2 cache
							gpus[i].l2_cache = getattr(gpus[i], model).get("l2Cache")

							#get architecture
							gpus[i].architecture = line[4:-1]
								
							#get cores
							line = skip_line(f, 2)
							if line[-3:-1] == "nm":
								line = skip_line(f, 2)
							gpus[i].cores = str(int(line[4:-1]) * 64)
					
							#get flops TODO not all right flops
							lastFlop = f.tell()
							secFlop = f.tell()
							while line[-2:-1] != "W":
								if line[-6:-1] == "FLOPS":
									lastFlop = secFlop
								line = skip_line(f, 1)
								secFlop = f.tell()
								line = skip_line(f, 1)
							f.seek(lastFlop)
							line = f.readline()
							flops = 0.0
							if line[-7:-6] == 'G':
								flops = float(line[4:-8]) * 1e9
							else:
								flops = float(line[4:-8]) * 1e12
							cores = int(gpus[i].cores)
							clock = int(gpus[i].core_frequency[0:-5]) * 1e6
							gpus[i].flops_per_cycle = str(round(flops / (cores * clock)))
							
							#get power
							f.seek(secFlop)
							line = f.readline()
							tdp = round(int(line[4:-3]) * 0.8) #tbp * 0.8 = tdp
							gpus[i].tdp = str(tdp) + " W"
							gpus[i].acp = str(round(tdp * 0.7)) + " W"
							gpus[i].node_power = "+" + gpus[i].tdp
							
							i += 1
					i = 0
					f.seek(tableStart)			
					line = skip_line(f, 2)
					
					for x in range(0, len(gpus)):
						#get lithography
						line = skip_line(f, 5)
						if line[-3:-1] == "nm":	
							startIndex = line.find('>') + 1
							gpus[i].feature_size = line[startIndex:-1]
						else:
							gpus[i].feature_size = gpus[i - 1].feature_size

						#get vram
						line = skip_line(f, 2)
						if line[-3: -1] == "GB":
							startIndex = line.find('>') + 1
							gpus[i].vram = line[startIndex:-1]
						else:
							line = skip_line(f, 2)
							if line[-3: -1] == "GB":
								startIndex = line.find('>') + 1
								gpus[i].vram = line[startIndex:-1]
							else:
								gpus[i].vram = gpus[i - 1].vram
						
						while line[-2:-1] != "W":
							line = skip_line(f, 2)
						line = skip_line(f, 2)
						i += 1
					break
				line = f.readline()
		f.close()

		f = open(url + ".xml", "w")
		print("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<itemslist xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"items-schema.xsd\">", file=f)
		for x in range(0, len(gpus)):
			print(gpus[x].create_xml_statement(), file=f)
		print("</itemslist>", file=f)
		f.close()

main()