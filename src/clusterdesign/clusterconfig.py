
'''
This is an attempt at translating the pascal code written by Konstantin S. Solnushkin into  a newer 
and more serviseable language.

My innnital release will be using all the features given to me, making it probaly have a minimum
requirement of 3.8, but once compiled should be runable by anyone who just wants as executable.

Original pascal files will be included in the distro for historic and reference for anyone who knows 
the program and wants to modify it to be more Pythonic.

I understand that most of this code will not be pythonic,
but it is a start that can progress to be better

Most nameing conventions are translated to what I believe to be their python equivilent

like cPower becomeing POWER

Values was kept as Values as a way to more easily translate, a find and replace could easily
make it values or VALUES

Python translation done by Nathan Borchelt

Comments denoted by // are copied straight from the pascalfiles.
'''

from sys import exit
import requests
from urllib.parse import quote

#Constants 

#node characteristics
NODES = 'nodes'
CPU = 'cpu'
CPU_MODEL = CPU + '_model'
CPU_CORES = CPU + '_cores'
CPU_FREQUENCY = CPU + '_frequency'
CPU_FLOPS_PER_CYCLE = CPU + '_flops_per_cycle'
CORES = 'cores'
CAPEX = 'capex'
OPEX = 'opex'
TCO = 'tco'

NODE_LIST = [NODES, CPU, CPU_MODE, CPU_CORES, CPU_FREQUENCY CPU_FLOPS_PER_CYCLE CORES, CAPEX, OPEX, TCO]

#Per-node characteristics
NODE = 'node'
NODE_MODEL = NODE + '_model'
NODE_PEAK_PERFORMANCE = NODE + '_peak_performance'
NODE_MAIN_MEMORY_PER_CORE = NODE + '_main_memory_per_core'
NODE_MAIN_MEMORY_SIZE = NODE + '_main_memory_size'
NODE_CPU_COUNT = NODE + '_cpu_count'
NODE_COST = NODE + '_cost'
NODE_POWER = NODE + '_power'
NODE_WEIGHT = NODE + '_weight'
NODE_EQUIPMENT_SIZE = NODE + '_equipment_size'

ENCLOSURE_SIZE = 'enclosure_size'

PER_NODE_LIST = [NODE, NODE_MODEL, NODE_PEAK_PERFORMANCE, NODE_MAIN_MEMORY_PER_CORE, NODE_MAIN_MEMORY_SIZE, NODE_CPU_COUNT, NODE_COST, NODE_POWER, NODE_WEIGHT, NODE_EQUIPMENT_SIZE, ENCLOSURE_SIZE]

#NETOWRK CHARACTERISTICS

NETOWRK = 'network'
NETWORK_TECH = NETWORK + '_tech'
NETWORK_COST = NETWORK + '_cost'
NETWORK_POWER = NETWORK + '_power'
NETWORK_WEIGHT = NETWORK + '_weight'
NEWORK_EQUIPMENT_SIZE = NETWORK + '_equipment_size'

NETWORK_LIST = [NETOWRK, NETWORK_TECH, NETWORK_COST, NETWORK_POWER, NETWORK_WEIGHT, NEWORK_EQUIPMENT_SIZE]

#UPS CHARACTERISTICS
UPS = 'ups'
UPS_COST = UPS + '_cost'
UPS_HEAT = UPS + '_heat'
UPS_WEIGHT = UPS + '_weight'
UPS_SIZE = UPS + '_size'
UPS_SIZE_RACKS = UPS + '_size_racks'

UPS_LIST = [UPS, UPS_COST, UPS_HEAT, UPS_WEIGHT, UPS_SIZE, UPS_SIZE_RACKS]

#PER-CLUSTER CHARACTERISTICS
POWER = 'power'
WEIGHT = 'weight'
PERFORMANCE = 'performance'
EQUIPMENT_SIZE = 'equipment_size'
EQUIPMENT_SIZE_RACKS = EQUIPMENT_SIZE + '_racks'

PER_CLUSTER_LIST = [POWER, WEIGHT, PERFORMANCE, EQUIPMENT_SIZE, EQUIPMENT_SIZE_RACKS]

#STRINGS RELATED TO SETTINGS

RACK_HEIGHT = 'rack_height'
SYSTEM_LIFETIME_YEARS = 'system_lifetime_years'
KWH_PRICE = 'kwh_price'
RACK_OPEX_PER_YEAR = 'rack_opex_per_year'
#RELATED TO BUILT IN MODELS
PEAK_PERFORMANCE = 'Peak performance'

OTHER_LIST = [RACK_HEIGHT, SYSTEM_LIFETIME_YEARS, KWH_PRICE, RACK_OPEX_PER_YEAR, PEAK_PERFORMANCE]

CONST_LIST = NODE_LIST + PER_NODE_LIST + NETWORK_LISTUPS_LIST + PER_CLUSTER_LIST + OTHER_LIST

simpleHeuristicTupe = ['Node cost / Peak perf.']
objFuncType = ['TCO / Performance', 'CapEx / Performance']

Values = dict()

#mandatory running, but because of how the pascal code is formated, this is the closest I can get 
#to replicating it
#gladly reformat all this code if you know better
def fillDict(sytemStats : list):
	for constant, value in zip(CONST_LIST, systemStats):
		try:
			valueF = float(value)
			if (valueF % 1.0 == 0):
				value = int(valueF)
			else:
				value = valueF
		except:
			pass
		Values[constant] = value
		


def isInstanceOf(data, dataType):
	return isinstance(data, dataType)
	
def ceil(num, deno):
	return int(-(-num // deno))

def ceil(num):
	return ceil(num,1)
	

def ClusterConfig(object):

	__cores = 0
	__nodes = 0
	__performance = 0
	__nodePeakPerformance = 0
	__nodeMainMemoryPerCore = 0
	__enabled = False
	
	def toggle(self):
		self.__enabled = not self.__enabled
		
	def forceToggle(self, state):
		self.__enabled = state
		
	def getCores(self):
		return self.__cores
	def setCores(self, cores : int):
		try:
			self.__cores = int(cores)
		except:
			print("Error: {reqType} required for {variable}, but datatype {actualType} was given".format(reqType="int", variable='__cores', actualType=type(cores)))
			exit()
		
	def getNodes(self):
		return self.__nodes
	def setNodes(self, nodes):
		try:
			self.__nodes = float(nodes)
		except:
			print("Error: {reqType} required for {variable}, but datatype {actualType} was given".format(reqType="int", variable='__nodes', actualType=type(nodes)))
			exit()
	
	def getPerformance(self):
		return self.__performance
	def setPerformance(self, performance):
		try:
			self.__performance = round(float(performance),1)
		except:
			print("Error: {reqType} required for {variable}, but datatype {actualType} was given".format(reqType="float", variable='__performance', actualType=type(performance)))
			exit()
		
	def getNodePeakPerformance(self):
		return self.__nodePeakPerformance
	def setNodePeakPerformance(self, nodePeakPerformance):
		try:
			self.__nodePeakPerformance = float(nodePeakPerformance)
		except:
			print("Error: {reqType} required for {variable}, but datatype {actualType} was given".format(reqType="float", variable='__noedPeakPerformance', actualType=type(nodePeakPerformance)))
			exit()
	
	def getNodeMainMemoryPerCore(self):
		return self.__nodeMainMemoryPerCore
	def setNodeMainMemoryPerCore(self, nodeMainMemoryPerCore):
		try:
			self.__nodeMainMemoryPerCore = float(nodeMainMemoryPerCore)
		except:
			print("Error: {reqType} required for {variable}, but datatype {actualType} was given".format(reqType="float", variable='__nodeMainMemoryPerCore', actualType=type(nodeMainMemoryPerCore)))
			exit()
			
	def roundUpNodeCount(self):
		
		#current number of cores may respond to a fraction of a node, calculate a nuw number of nodes
		#then ceiling the number
		self.__nodes = ceil(self.__cores / (Values[CPU_CORES] * Values[NODE_CPU_COUNT])
		
		#There could have been a change in the number of nodes, therefore recalculate the 
		#total number of cores
		self.__cores = round(self.__nodes * Values[CPU_CORES] * Values[NODE_CPU_COUNT])
		
	def nodeCostToPeakPerformance(self):
		
		#devide cost by peak performance
		return Values[NODE_COST] / self.__nodePeakPerformance
	
	def capexToPerformance(self):
		return Values[Capex] / self.__performance
	
	def tcoToPerformance(self):
		return Values[TCO] / self.__performance
		
	'''
	// Computes some important technical characteristics.
	// Not very flexible, because some configurations may not have all the characteristics.
	// For example, what happens if compute node weight is not defined for the current configuration
	// shall we just not compute the cluster weight then?

	// This function is called many, many times for each configuration -- and every time it recomputes
	// technical and economic characteristics. It is more convenient to have a single function so that
	// all calculations can be easily tracked. Besides, calling this function every time the
	// next subsystem is designed allows to immediately check for constraint violation, which may
	// disable the configuration, thereby skipping unnecessary calls to web services responsible
	// for other subsystems.

	'''
	
	def computeTechnicalCharacteristics(self):
		HOURS_PER_YEAR = 365 * 24
		
		if self.__enabled: #// Only deal with enabled configurations
			
			#// Calcaluate CapEx:
			#	//Cost of compute nodes:
			capEx = self.__nodes * Values[NODE_COST]
			
			#//   Cost of network:
			if (cost  := Values[NETWORK_COST]) > 0:
				capEx += cost
			
			#//   Cost of UPS:
			if (cost  := Values[UPS_COST]) > 0:
				capEx += cost
			
			#// FinSimpleHeuristicTypal assignment:
			Values[CAPEX] = capex
			
			'''#ORIGINAL DOCS
			  // Calculate "Power", "Weight" and "Equipment size"
			  // XXX: Need to add similar figures from other models (storage subsystem, cooling 
			  // equipment) when they become available
			  // Note: we can have nice properties in TClusterConfig class to access there metrics,
			  // such as "property Power: Integer", etc. Unsure if the sprawl is needed, though.
			  // XXX: For blade systems, power and weight of enclosures is NOT taken into account,
			  // while equipment size is.
			'''
			
			#// Power from compute nodes:
			power  = self.__nodes * Values[NODE_POWER]
			if (netPower := Values[NETWORK_POWER]) > 0:
				power += netPower
			'''
			// Here comes a tricky moment. We need to understand cooling requirements for the
			// configuration, as well as choose the rated current of circuit breakers. As the UPS
			// subsystem is not 100% efficient, some of the electricity it consumes is dissipated as
			// heat and is not passed to the load. Therefore, we need to add that "heat" value to the
			// current value of consumed power. The resulting value of "power" is then good for 
			// cooling system and circuit breaker sizing.
			'''
			#// Heat dissipated by the UPS:
			if (heat := Values[UPS_HEAT]) > 0:
				power += heat
			
			#// Final assignment of power metric
			Values[POWER] = power
			
			#// The same with weight
			#//	Weight of compute nodes:
			weight = self.__nodes * Values[NODE_WEIGHT]
			
			#//   Weight of network:
			if (netWeight := Values[NETWORK_WEIGHT]) > 0:
				weight += netWeight
				
			#//   Weight of UPS:
			if (upsWeight := Values[UPS_WEIGHT] > 0:
				weight += upsWeight
				
			#// Final assignment of weight
			Values[WEIGHT] = round(weight)
			
			#// (Almost) the same with equipment size
			eqSize = self.__nodes * Values[NODE_EQUIPMENT_SIZE]
			
			#// If enclosure size is defined, than we are dealing with blade servers, and shall
			#// round the equipment size up to the nearest number of units the enclosure(s) occupy
			if (encloSize := Values[ENCLOSURE_SIZE]) > 0:
				eqSize = ceil(eqSize, encloSize) * encloSize
			
			#// Size of network equipment
			if (netSize := Values[NETWORK_EQUIPMENT_SIZE]) > 0:
				eqSize += netSize
				
			'''
			// Size of the UPS subsystem.
			// Some UPS systems are located in separate racks. We assume that web service will return
			// size in "cUpsSizeRacks". If, however, the UPS is shipped as ordinary blocks designed
			// for usual installation into racks, then we assume the size will be returned in
			// "cUpsSize", and then we add this value to the overall equipment size.
  			'''
  			#// Initialise with zero; it will be used if nothing is returned by web services:
  			eqSizeRacks = 0
  			
  			#// If UPS size is returned in racks, save it:
  			if (upsSizeRack := Values[UPS_SIZE_RACKS]) > 0:
  				eqSizeRacks = upsSizeRack
  			else: #// If it is returned in units, add to other equipment size:
  				if (upsSize := Values[UPS_SIZE]) > 0:
  					eqSize += upsSize
  			
  			#// Read rack height
  			rackHeight = Values[RACK_HEIGHT]
  			
  			'''
  			// Calculate the number of racks using the user-supplied rack height
 			// Assume that compute nodes and network equipment can be densely packaged into racks.
			// Round the required number of racks upwards, and add it to the already present racks of
			// the UPS system.
  			'''
  			eqSizeRacks + ceil(eqSize / rackHeight)
  			
  			#// Final assignments
			#//   Cumulative size of separate blocks of equipment:
			
			Values[EQUIPMENT_SIZE] = round(eqSize)
			Values[EQUIPMENT_SIZE_RACKS] = round(eqRackSize)
			
			#// Calculate OpEx and then TCO
			
			power = Values[POWER]
			kwhPrice = Values[KWH_PRICE] #// This can be zero due to user request
			rackOpExPerYear = Values[RACK_OPEX_PER YEAR] #// And this, too.
			systemLifetimeYears = Values[SYSTEM_LIFETIME_YEARS] #// And even this. And if it is, OpEx=0
			opEx = round(systemLifetimeYears * (HOURS_PER_YEAR * kwhPrice * power / 1000 + eqSizeRacks * rackOpExPerYear))
			Values[OPEX] = opEx
			
			Values[TCO] = (opEx + capEx)
	
	def computeSimpleObjFunc(self, identifier):
		#// Implements a simple objective function -- the one that can be inferred from the node's
		#// characteristics. Currently, the objective function "node cost divided by node peak
		#// performance" is implemented.
		
		if (self.__enabled): #// Only deal with enabled configurations
			if (identifier == simpleHeuristicType[0]):
				simpleObjFunction = nodeCostToPeakPerformance()
			else:
				print("No such heuristic had been programmed : {heur}, sorry.".format(heur=identifier))
			
			
	def computeObjFunc(self, identifier):
		#// Implements a complex objective function -- the one that requires calculating cluster 
		#// performance first, etc., and therefore cannot be inferred from the characteristics of a 
		#// compute node alone. Currently, objective functions "CapEx / Performance" and "TCO / 
		#//Performance" are implemented.
		if (self.__enabled): #// Only deal with enabled configurations
			if (identifier == arrayObjectFuncType[0]):
				objFunc = tcoToPerformance()
			elif (identifier == arrayObjectFuncType[1]):
				objFunc = capexToPerformance()
				
			else:
				print('No such objective function had been programmed: {objective}, sorry.'.format(objective=identifier)
	
	def essentialCharacteristicsText(self):
		#// Returns the essential technical and economic characteristics of the configuration in a
		#// formatted text string
		template = 'Essential characteristics:\n\nCompute node model: {cnm}\nNumber of compute nodes: {ncn}\nTotal number of cores: {tnc}\nPerformance: {perf}\nEquipment size: {eqSze}\nPower: {pwr}\nWeight: {wgt}\nCapital expenditures: {capExp}\nOperating expenditures: {opExp}\nTCO: {tcoS}\n\nCPU\n\t\tModel: {mdl}\n\t\tCores: {crs}\n\t\tFrequency: {frq}\n\nNetwork technology: {netTech}'
		
		NaN = "N/A"
		#// Model of compute nodes
		nodeModel = ((nodemdl = Values[NODE_MODEL]) if nodemdl != '' else NaN)
		#// Number of compute nodes
		nodes = ((numNodes = Values[NODES]) if numNodes != '' else NaN)
		#// Total number of cores
		cores = ((numCores = Values[CORES]) if numCores != '' else NaN)
		#// Performance (units of measurement depend on what the web service is using)
		performance = ((perf = Values[PERFORMANCE]) if perf != '' else NaN)
		#// Equipment size
		eqSize = ((eqs = Values[EQUIPMENT_SIZE]) if eqs != '' else NaN)
		#// Power
		power = ((pwr = Values[POWER]) if pwr != '' else NaN)
		#// Weight
		weight = ((wgt = Values[WEIGHT]) if wgt != '' else NaN)
		#// Capital expenditures
		capEx = ((capExp = Values[CAPEX]) if capExp != '' else NaN)
		#// Operating expenditures
		opEx = ((opExp = Values[OPEX]) if oEx != '' else NaN)
		#// TCO
		tco = ((tcoT = Values[TCO]) if tcoT != '' else NaN)
		#// CPU Model
		cpuModel = ((cpuMdl = Values[CPU_MODEL]) if cpuMdl != '' else NaN)
		#// Number of cores per CPU
		cpuCores = ((cpuCrs = Values[CPU_CORES]) if cpuCrs != '' else NaN)
		#// CPU frequency
		cpuFrequency = ((cpuFrq = Values[CPU_FREQUENCY]) if cpuFrq != '' else NaN)
		#// Network technology
		networkTech = ((netTech = Values[NETWORK_TECH]) if netTech != '' else NaN)
		
		return template.format(cnm=nodeModel, ncn=nodes, tnc=cores, perf=performance, eqSze=eqSize, pwr=power, wgt=weight, capExp=capEx, opExp=epEx, tcoS=tco, mdl=cpuModel, crs=cpuCores, frq=cpuFrequency, netTech=networkTech)
		
	def runInversePerformanceModel(self, aModel : GenericModel):
	
		#// Queries the web service, provides it with the projected performance and receives the
		#// number of cores (in the current implementation) required to attain the requested
		#// performance
		if (self.__enabled): #// Only deal with enabled configurations
			
			result = False #// The default is failure
			
			#// "Send" contains the name of the variable where the web service 
			#// expects to receive the projected performance.
			aModel.__send = NCPName(aModel.__send) + '=' + aModel.Constraints.Values[MIN_PERFORMANCE]
			
			#// Actually call the model; if it failed, ~~exit~~
			#return False
			result = callModel(aModel)
			if (callModel(aModel)):
					
				#// If we reached here, the result was positive, and the contents of "AlsoReceive"
				#// -- the auxiliary characteristics -- were already stored in the current
				#// configuration. Just the last step remains: save the value of the essential
				#// parameter. "Receive" contains the name-value pair where web service
				#// reports the number of cores
				Values[CORES] = NVPValue(aModel.__receive)
				
				#// Now, the number of cores has changed. The value returned 
				#// by the web service can lead to the fractional number of nodes,
				#// so we need to perform adjustments.
				
				roundUpNodeCount()
				return True
				
				 #// Now that the number of nodes and cores could have increased, it is best to
				 #// compute the performance again, because it has also possibly increased.
				 #// That's why the call to this method is often followed by
				 #// the call to "ComputePerformance".
			
		return False
		
	def runInversePeakPerformanceModel(self, aModel : GenericModel):
		if (self.__enabled): #// Only deal with enabled configurations
		
			#// That's our desired peak performance
			minPeakPerformance = aModel.Constraints.Values[MIN_PERFORMANCE]
			
			#// That's how many nodes we must use to achieve it.
			#// Note that "Ceil" is used to round up the value.
			self.__nodes = ceil(minPeakPerformance, self.__nodePeakPerformance)
			
			#// After rounding up, the performance can be slightly higher than requested.
			#// Compute it and store.
			Values[PERFORMANCE] = (round(self.__nodes * self.__nodePeakPerformance),1)
			
			#// We also need to set the number of cores.
			#// That's how many cores our nodes will contain: Nodes*Cores in one CPU*CPUs in one node
			self.__cores = self.__nodes * Values[CPU_CORES] * Values{NODE_CPU_COUNT]
			
			return True
		
		 return False
		 
	def computePerformance(self, aModel : GenericModel):
		if (self.__enabled):
			#// Make sure the number of cores for this configuration is defined,
			#// otherwise we cannot calculate performance
			if (Values[CORES]):
				#// "Send" contains the name of the variable where the web service expects
				#// to receive the number of cores.
				aModel.__send = NVPName(aModel.__send) += Values[CORES]
				#// Actually call the model; if it failed, ~~exit~~ 
				#return False
				result = callModel(aModel)
				if (callModel(aModel)):
					#// If we reached here, the result was positive, and the contents of
					#// -- "AlsoReceive" -- the auxiliary // characteristics -- were already stored
					#// in the current configuration. Just the last step remains:
					#// save the value of the essential parameter.
					
					
					#// "Receive" contains the name-value pair where web service
					#// reports the performance
					Values[PERFORMANCE] = NVPValue(aModel.__receive);
					
					return True
		return False
				
	def designNetwork(self, aModel : GenericModel):
		if (self.__enabled): # // Only deal with enabled configurations
		
			#// Make sure the number of nodes for this configuration is defined,
			#// otherwise we cannot design a network
			if (Values[NODES]):
				#// "Send" contains the name of the variable where the web service expects
				#// to receive the number of nodes.
				aModel.__send = NVPName(aModel.__send) + '=' + Values[NODES]
				if(callModel(aModel)):
					#// If we reached here, the result was positive, and the contents of "AlsoReceive"
					#// -- the auxiliary characteristics -- were already stored in the current
					#// configuration. Just the last step remains:
					#// save the value of the essential parameter.
					#// "Receive" contains the name-value pair where web service
					#// reports the network cost
					Values[NETWORK_COST] = NVPValue(aModel.__receive)
					return True
		return False
	
	def designUps(self, aModel : GenericModel):
		#// Almost an identical copy of "DesignNetwork"
		if (self.__enabled): # // Only deal with enabled configurations
		
			#// Make sure the number of nodes for this configuration is defined,
			#// otherwise we cannot design a network
			if (Values[POWER]):
				#// "Send" contains the name of the variable where the web service expects
				#// to receive the number of nodes.
				aModel.__send = NVPName(aModel.__send) + '=' + Values[POWER]
				if(callModel(aModel)):
					#// If we reached here, the result was positive, and the contents of "AlsoReceive"
					#// -- the auxiliary characteristics -- were already stored in the current
					#// configuration. Just the last step remains:
					#// save the value of the essential parameter.
					#// "Receive" contains the name-value pair where web service
					#// reports the network cost
					Values[UPS_COST] = NVPValue(aModel.__receive)
					return True
		return False
		
	def callModel(self, aModel : GenericModel):
		#// A generic procedure to call a model
		if (self.__enabled):
			queryURL = aModel.__url
			#// If the "URL" does not yet contain a "?" sign, add it to the tail of "QueryURL"
			if ('?' not in queryURL):
				queryURL += "?"
			else: #// But if the '?' was already there, add the '&'
				queryURL += "&"
				
			#// The essential part that we need to send
			queryURL += aModel.__send
			
			#// Also send additional parameters to the web service
			#Figure out what this line does
			FillValues(aModel.__alsoSend)
			for item in aModel.__alsoSend:
				queryURL += "&" + quote(item)
				
			response = None
				
			try:
				response = requests.get(queryURL)
				if '200' not in response:
					print('Web service <query URL {url}> returnned HTTP code {cde}'.format(url=queryURL, cde=code)
					#// Return failure status
					return False
			except Exception as e:
				print('Error Encounterd: {error}'.format(error=e)
				return False
					#// Return failures
			#// Also, the web service could return an error message
			
			#might need to be requests.post()
			#potentially response.content (returns bytes instead of string)
			#also potential for response.json()
			pageData = response.text
			
			if not pagaData:
				print('Web service returned empty response)
				return False
			for line in pageData.splitLines():
				if '404' in line:
					print(line)
					return false:
					
			#// If we reached here, assume we got a correct response

      		#// First of all, we need to retrieve our "main parameter"
      		#// During repetitive runs, the value of "Receive" may contain not just the name of the 
      		#// variable we need to check for, but also its value, in a form of a "Name=Value" pair. 
      		#// In this case, strip the value
      		
      		#need to do this. just realized now that NVPName is "name-value pair name", so I will 
      		#have plenty of code to modify. without knoing how the webservice return the info. get 
      		#this later
				
				
			
	
#still need sto be done, get later

def ClusterConfigList(object)
	clusterList = []






































