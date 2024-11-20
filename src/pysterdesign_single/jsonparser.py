from collections import defaultdict
import json
import os
from CPU import CompUnit
from Blade import Blade
from Chassis import Chassis
from GPU import GraphicUnit
from GpuBlade import GpuBlade
from Memory import RAM
import Memory
from Node import ComputeNode
from Network import Network


def parse_cpu_json(json_data):
   cpu_list = []
  
   for cpu_name, cpu_data in json_data['cpus'].items():
       itemInfo = {
           'cpu_brand': cpu_data['brand'],
           'cpu_subbrand': cpu_data['subbrand'],
           'cpu_codename': cpu_data['codename'],
           'cpu_configs': cpu_data['cpu_configs'],
           'cpu_model': cpu_name,
           'cpu_price': cpu_data['price'],
           'cpu_cores': cpu_data['cores'],
           'cpu_cache': cpu_data['cache'],
           'cpu_tdp': cpu_data['tdp'],
           'cpu_memory_specs': cpu_data['memory_specs'],
           'cpu_clock_speed': cpu_data['clock_speed'],
           'cpu_feature_size': float(cpu_data['feature_size']),
           'cpu_flops_per_cycle': cpu_data['flops_per_cycle']
       }
        
       cpu = CompUnit(
           brand=cpu_data['brand'],
           subBrand=cpu_data['subbrand'],
           codeName=cpu_data['codename'],
           model=cpu_name,
           cpuConfigs=cpu_data['cpu_configs'],
           price=cpu_data['price'],
           cores=cpu_data['cores'],
           cache=cpu_data['cache'],
           tdp=cpu_data['tdp'],
           memorySpecs=cpu_data['memory_specs'],
           clockSpeed=cpu_data['clock_speed'],
           featureSize=float(cpu_data['feature_size'].replace('e-9', '')),
           flopsPerCycle=cpu_data['flops_per_cycle'],
           isJSON=True
       )

       cpu_list.append(cpu)

   return cpu_list

def parse_blade_json(json_data):
    blade_list = []
    
    for blade_name, blade_data in json_data['blade'].items():
        # Check if this is a GPU blade
        if 'accepted_gpus' in blade_data and 'max_gpus' in blade_data:
            blade = GpuBlade(
                bladeName=blade_name,
                acceptedCPUs=blade_data['accepted_cpus'],
                maxCpus=blade_data['max_cpus'],
                dimmsPerCPU=blade_data['dimms_per_cpu'],
                maxDimmCapacity=blade_data['max_dimm_capacity'],
                dimmGeneration=blade_data['memory_gen'],
                maxTransferRate=blade_data['max_transfer_rate'],
                acceptedGPUs=blade_data['accepted_gpus'],
                maxGPUs=blade_data['max_gpus']
            )
            if isinstance(blade, GpuBlade):
                print("GPU blade object created")
        else:
            default_ram = Memory.RAM("DDR", 0, 0, 0, 0, 0)
            blade = Blade(
                bladeName=blade_name,
                acceptedCPUs=blade_data['accepted_cpus'],
                maxCpus=blade_data['max_cpus'],
                dimmsPerCPU=blade_data['dimms_per_cpu'],
                maxDimmCapacity=blade_data['max_dimm_capacity'],
                dimmGeneration=blade_data['memory_gen'],
                maxTransferRate=blade_data['max_transfer_rate'],
               
            )
            if isinstance(blade, Blade):
                print("blade object created")

        blade_list.append(blade)
    # print(blade_list)
    return blade_list

def parse_chassis_json(json_data):
   chassis_list = []
   for chassis_name, chassis_data in json_data['chassis'].items():
       chassis = Chassis (
           name = chassis_name,
           height = chassis_data['height'],
       )

       chassis_list.append(chassis)

   return chassis_list

def parse_GPU_json(json_data):
   GPU_list = []
   for gpu_name, gpu_data in json_data['gpus'].items():
       gpu = GraphicUnit (
           name = gpu_name,
           clock = gpu_data['clock'],
           fp64 = gpu_data['FP64'],
           fp32 = gpu_data['FP32'],
           fp16 = gpu_data['FP16'],
           memory = gpu_data['memory'],
           tdp = gpu_data['tdp'],
           maxGpus = gpu_data['max_gpus'],
       )

       GPU_list.append(gpu)

   return GPU_list  

def parse_memory_json(json_data):
    ram_list = []
    
    for measure_style, generations in json_data['memory'].items():
        for gen_info in generations:
            generation = gen_info['gen']
            
            for performance in gen_info['performance']:
                ram = RAM(
                    memSpecType=measure_style,
                    generation=generation,
                    speed=performance['speed'],
                    capacity=performance['capacity'],
                    price=performance['price'],
                    dimms = generation
                )
                
                ram_list.append(ram)
    
    return ram_list


def parse_node_json(json_data):
    node_list= []

    for node_name, node_data in json_data['node'].items():
        node = ComputeNode(
            name=node_name,
            height = node_data['height'],
            bladeQuantity = node_data['blades_per_node'],
            bladesList = node_data['accepted_blades']

        )

        node_list.append(node)

    return node_list

def parse_network_json(json_data):
    network_list =[]

    for manufacturer, devices in json_data['networking'].items():
        for device_name, device_data in devices.items():
            network = Network(
                tech=device_data['infiniband'][0],  # HDR or NDR
                portCount=device_data['port_count'],
                speed=float(device_data['port_speed'])
            )
            
            if isinstance(network, Network):
                print(f"Network object created for {device_name}")
            network_list.append(network)
            
            # Print details for verification
   
            print("Additional JSON Data (not in Network object):")
            print(f"Price: {device_data['price']}")
            if 'switch_throughput' in device_data:
                print(f"Switch Throughput: {device_data['switch_throughput']}")
            print(f"Infiniband: {device_data['infiniband']}")
            print("----------------------------")

def parseJsonTree(jsonFileName) -> dict:

    outputDict = defaultdict(list)
    componentDict = dict()

    headFile = open(jsonFileName)
    headData = json.load(headFile)
    if("components" in headData):
        otherFilesPath = os.path.dirname(os.path.realpath(headFile.name))
        for key, value in headData["components"].items():
            with open(os.path.join(otherFilesPath, key+".json"), "r") as componentFiles:
                componentDict = {value : parse_json_file(componentFiles)}

            for compKey, compValue in componentDict.items():
                outputDict[compKey].append(compValue)
    # print(outputDict)
    return outputDict

def parse_json_file(jsonFile):
   json_data = json.load(jsonFile)
   if 'cpus' in json_data:
       return parse_cpu_json(json_data)
   elif 'blade' in json_data:
       return parse_blade_json(json_data)
   elif 'chassis' in json_data:
       return parse_chassis_json(json_data)
   elif 'gpus' in json_data:
       return parse_GPU_json(json_data)
   elif 'memory' in json_data:
         return parse_memory_json(json_data)
   elif 'node' in json_data:
    return parse_node_json(json_data)
   elif 'networking' in json_data:
    return parse_network_json(json_data)
   else:
       raise ValueError("Unknown JSON structure")