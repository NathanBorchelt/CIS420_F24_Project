import xml.etree.ElementTree as ET
from CPU import CompUnit
import Blade
from Node import ComputeNode
from typing import List, Type
import re
import Chassis
import GUI
import RackMount
from Memory import RAM
from Network import Network


def parseCPUXML(xmlFile):
    # tree = ET.parse(xmlFile)
    # root = tree.getroot()
    root = ET.fromstring(xmlFile)

    CPUs = []

    for CPU in root.findall('item'):
        itemInfo = {
            'cpu_vendor': CPU.get('cpu_vendor'),
            'cpu_codename': CPU.get('cpu_codename'),
            'cpu_model': CPU.get('cpu_model'),
            'cpu_cores': int(CPU.get('cpu_cores')),
            'cpu_feature_size': float(CPU.get('cpu_feature_size')),
            'cpu_frequency': float(CPU.get('cpu_frequency')),
            'cpu_flops_per_cycle': int(CPU.get('cpu_flops_per_cycle')),
            'cpu_l3_cache': CPU.get('cpu_l3_cache'),
            'cpu_acp': int(CPU.get('cpu_acp')),
            'cpu_tdp': int(CPU.get('cpu_tdp')),
            'node_power': int(CPU.get('node_power').replace('+', '')),
            'node_cpu_count': int(CPU.get('node_cpu_count').replace('+', '')),
            'node_cost': int(CPU.get('node_cost').replace('+', '')),
            'description': CPU.text.strip()
        }
        
        comp_unit = CompUnit(
            brand = CPU.get('cpu_vendor'),
            codeName = CPU.get('cpu_codename'),
            model = CPU.get('cpu_model'),
            price = int(CPU.get('node_cost').replace('+', '')),
            cores = int(CPU.get('cpu_cores')),
            cache = {"l3" : itemInfo['cpu_l3_cache']},
            tdp = int(CPU.get('cpu_tdp')),
            clockSpeed = {"all_boost": itemInfo['cpu_frequency']},
            featureSize = float(CPU.get('cpu_feature_size')),
            flopsPerCycle = int(CPU.get('cpu_flops_per_cycle')),
            isJSON=False
        )
        CPUs.append(comp_unit)
    for CPU in CPUs:
        print(CPU)
    return CPUs

def parseNodeXML(xmlFile):
    # tree = ET.parse(xmlFile)
    # root = tree.getroot()
    root = ET.fromstring(xmlFile)

    bladesList = []
    acceptedCPUList = []
    cpuPartiton = []
    dimmGenerationValue = 0
    maxTransferRateValue = 0

    nodes : List[Type[RackMount.RackMount]] = []
    for edge in root.findall('edge'):
        if(edge.get("from").startswith("HPE")):
            bladesList.append(edge.get("to"))

    for item in root.findall('item'):
        itemInfo = {
            'node_model': item.get('node_model'),
            'node_vendor': item.get('node_vendor'),
            'node_form_factor': item.get('node_form_factor'),
            'node_cost': item.get('node_cost'),
            'node_weight': item.get('node_weight'),
            'node_equipment_size': item.get('node_equipment_size'),
            'enclosure_size': item.get('enclosure_size'),
            'node_free_dimm_count': item.get('node_free_dimm_count'),
        }
        # find edges with from property equal to node_model and get the to value from that and that is one of the accepted cpus
        try:
            node = ComputeNode(
                name = item.get('node_model'),
                height = float(item.get('node_equipment_size')),
                acceptedBlades = bladesList,
            )
            nodes.append(node)
        except:
            continue
        for edge in root.findall('edge'):
            if(edge.get('from') == item.get('node_model')):
                acceptedCPUList.append([item.get('to'), item.get('to-partition'), 1])
        for edge in root.findall('edge'):
            for cpu in acceptedCPUList:
                if(edge.get('from') == cpu[0]):
                    cpu[2] += 1
        for edge in root.findall('edge'):
            if('END_OF_CPU' in edge.get('from')):
                generationString = edge.get('to')
                generationStringCleaned = re.match(r'PC(\d+)-(\d+)', generationString)
                if(generationStringCleaned):
                    dimmGenerationValue = int(generationStringCleaned.group(1))
                    maxTransferRateValue = int(generationStringCleaned.group(2))
                    



    for i in range(len(nodes)):
        nodes[i].addBlade(
        Blade.Blade(
                bladeName = nodes[i].getName, 
                acceptedCPUs = acceptedCPUList[i][0], 
                maxCpus = acceptedCPUList[i][2], 
                dimmsPerCPU = itemInfo['node_free_dimm_count']/acceptedCPUList[i][2], 
                maxDimmCapacity = 4 * 1024**4 if acceptedCPUList[i][2] == 2 else 2 * 1024 **4, 
                dimmGeneration = dimmGenerationValue, 
                maxTransferRate = maxTransferRateValue
            ))
    for node in nodes:
        print(node)
    return nodes

def parseMemoryXML(xmlFile):
    root = ET.fromstring(xmlFile)
    memories = []

    for item in root.findall('item'):
        memory = RAM(
            memSpecType=item.get('main_memory_type'),
            generation=int(re.match(r'PC(\d+)').group(1)),
            speed=item.get('main_memory_speed'),
            capacity= int(item.get('node_main_memory_size')),
            price= int(item.get('node_cost')),
            dimms= int(item.get('node_free_dimm_count'))
        )
        memories.append(memory)

    return memories

def parseChassisXML(xmlFile, fileName):
    root = ET.fromstring(xmlFile)

    cpuFiles = []
    nodeFiles = []
    memoryFiles = []
    chassisCPUs = []
    chassisNodes = []
    chassisMemory = []
    chassisNodeBlades = []

    for include in root.findall('include'):
        if(include.text.startswith('amd') or include.text.startswith('intel')):
            cpuFiles.append(include.text)
        elif(include.text.startswith('HPE')):
            nodeFiles.append(include.text)
        elif('memory' in include.text):
            memoryFiles.append(include.text)
    
    for cpuFile in cpuFiles:
        with open(cpuFile, 'r') as xmlData:
            chassisCPUs.append(parseCPUXML(xmlData.read()))
    for nodeFile in nodeFiles:
        with open(nodeFile, 'r') as xmlData:
            chassisNodes.append(parseNodeXML(xmlData.read()))
    for memoryFile in memoryFiles:
        with open(memoryFile, 'r') as xmlData:
            chassisMemory.append(parseMemoryXML(xmlData.read()))

    for chassisNode in chassisNodes:
        for chassisNodeBlade in chassisNode.getBlades():
            for chassisNodeBladeCpu in chassisCPUs:
                chassisNodeBlade.setCPU(chassisNodeBladeCpu)


    for chassisNode in chassisNodes:
        chassis = Chassis.Chassis(
        name = fileName,
        height = GUI.SettingFrame.rackHeightEntry.get()
        ).addItem(chassisNode)
    

    return chassis

def parseNetworkXML(xmlFile):
    root = ET.fromstring(xmlFile)
    networks = []

    for item in root.findall('item'):
        network = Network(
            tech= item.get('network_tech'),
            portCount= int(item.get('node_infiniband_port_count'))
        )
        networks.append(network)

    return networks
