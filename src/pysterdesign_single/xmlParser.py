import xml.etree.ElementTree as ET
from CPU import CompUnit
from Blade import Blade
from Node import ComputeNode
from typing import List, Type
import re


def parseXMLCPU(xmlFile):
    # tree = ET.parse(xmlFile)
    # root = tree.getroot()
    root = ET.fromstring(xmlFile)

    items = []

    for item in root.findall('item'):
        itemInfo = {
            'cpu_vendor': item.get('cpu_vendor'),
            'cpu_codename': item.get('cpu_codename'),
            'cpu_model': item.get('cpu_model'),
            'cpu_cores': int(item.get('cpu_cores')),
            'cpu_feature_size': float(item.get('cpu_feature_size')),
            'cpu_frequency': float(item.get('cpu_frequency')),
            'cpu_flops_per_cycle': int(item.get('cpu_flops_per_cycle')),
            'cpu_l3_cache': item.get('cpu_l3_cache'),
            'cpu_acp': int(item.get('cpu_acp')),
            'cpu_tdp': int(item.get('cpu_tdp')),
            'node_power': int(item.get('node_power').replace('+', '')),
            'node_cpu_count': int(item.get('node_cpu_count').replace('+', '')),
            'node_cost': int(item.get('node_cost').replace('+', '')),
            'description': item.text.strip()
        }
        
        comp_unit = CompUnit(
            brand = item.get('cpu_vendor'),
            codeName = item.get('cpu_codename'),
            model = item.get('cpu_model'),
            price = int(item.get('node_cost').replace('+', '')),
            cores = int(item.get('cpu_cores')),
            cache = {"l3" : itemInfo['cpu_l3_cache']},
            tdp = int(item.get('cpu_tdp')),
            clockSpeed = {"all_boost": itemInfo['cpu_frequency']},
            featureSize = float(item.get('cpu_feature_size')),
            flopsPerCycle = int(item.get('cpu_flops_per_cycle')),
            isJSON=False
        )
        items.append(comp_unit)
    for item in items:
        print(item)
    return items

def parseNodeXML(xmlFile):
    # tree = ET.parse(xmlFile)
    # root = tree.getroot()
    root = ET.fromstring(xmlFile)

    bladesList = []
    acceptedCPUList = []
    cpuPartiton = []
    dimmGenerationValue = 0
    maxTransferRateValue = 0

    nodes : List[Type[ComputeNode]] = []
    for include in root.findall('include'):
        if(include.text.startswith("HPE")):
            bladesList.append(include.text[:-4])

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
        Blade(
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