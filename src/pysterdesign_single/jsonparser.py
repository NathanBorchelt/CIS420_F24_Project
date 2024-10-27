import json
from CPU import CompUnit
from Blade import Blade
from Chassis import Chassis
from GPU import GraphicUnit




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
      
       if isinstance(cpu, CompUnit):
           print("CPU Object created successfully")
       cpu_list.append(cpu)


       for i in range (0, len(cpu_list)):
           print(f"\nCPU Object Details for {cpu_name}:")
           print(f"Brand: {cpu.getBrand()}")
           print(f"SubBrand: {cpu.getSubBrand()}")
           print(f"CodeName: {cpu.getCodeName()}")
           print(f"Model: {cpu.getModel()}")
           print(f"CPU Configs: {cpu.getCpuConfigs()}")
           print(f"Price: {cpu.getPrice()}")
           print(f"Cores: {cpu.getCores()}")
           print(f"Cache: {cpu.getCache()}")
           print(f"TDP: {cpu.getTdp()}")
           print(f"Clock Speed: {cpu.getClockSpeed()}")
           print(f"Feature Size: {cpu.getFeatureSize()}")
           print(f"FLOPS per Cycle: {cpu.getFlopsPerCycle()}")
           print(f"Memory Specs: {cpu.getMemorySpecs()}")
           print(f"String Representation: {str(cpu)}")
           print("----------------------------")


   return cpu_list




def parse_blade_json(json_data):
   blade_list = []


   for blade_name, blade_data in json_data['blade'].items():
       blade = Blade (
           bladename = blade_name,
           acceptedCPUs = blade_data['accepted_cpus'],
           maxCpus = blade_data['max_cpus'],
           dimmsPerCPU = blade_data['dimms_per_cpu'],
           maxDimmCapacity = blade_data['max_dimm_capacity'],
       )


def parse_chassis_json(json_data):
   chassis_list = []
   for chassis_name, chassis_data in json_data['chassis'].items():
       chassis = Chassis (
           name = chassis_name,
           height = chassis_data['height'],
       )


       if isinstance(chassis, Chassis):
           print("Chassis Object created successfully")


       chassis_list.append(chassis)


       for i in range (0, len(chassis_list)):
           print(f"\nChassis Object Details for {chassis.name}:")
           print(f"Height: {chassis.height}")
           print(f"String Representation: {str(chassis)}")
           print("----------------------------")
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


       if isinstance(gpu, GraphicUnit):
           print("GPU Object created successfully")


       GPU_list.append(gpu)


       for i in range (0, len(GPU_list)):
           print(f"\nGPU Object Details for {gpu_name}:")
           print(f"Name: {gpu.name}")
           print(f"Clock: {gpu.clock}")
           print(f"FP64: {gpu.fp64}")
           print(f"FP32: {gpu.fp32}")
           print(f"FP16: {gpu.fp16}")
           print(f"Memory: {gpu.memory}")
           print(f"TDP: {gpu.tdp}")
           print(f"Max GPUs: {gpu.maxGpus}")
           print(f"String Representation: {str(gpu)}")
           print("----------------------------")




'''def parse_hpe_ex_json(json_data):
   if 'cpus' in json_data:
       return parse_cpu_json(json_data)
   elif 'blade' in json_data:
       return parse_blade_json(json_data)
   elif 'chassis' in json_data:
       return parse_chassis_json(json_data)
   elif 'gpus' in json_data:
       return parse_GPU_json(json_data)
   else:
       raise ValueError("Unknown JSON structure") '''


def parse_json_file(json_data):
   if 'cpus' in json_data:
       return parse_cpu_json(json_data)
   elif 'blade' in json_data:
       return parse_blade_json(json_data)
   elif 'chassis' in json_data:
       return parse_chassis_json(json_data)
   elif 'gpus' in json_data:
       return parse_GPU_json(json_data)
   else:
       raise ValueError("Unknown JSON structure")