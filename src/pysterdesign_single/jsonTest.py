import json, os, copy
import resource
import CPU, Memory ,Blade, Node, Chassis, GpuBlade, GPU
from glob import glob

#For checking max memory usage durring execution
#print(str(round(resource.getrusage(resource.RUSAGE_SELF).ru_maxrss/1000,2))+"MB")

cpuList = []
ramList = []
bladeList = []
nodeList = []
chassisList = []
gpuList = []

exChassis = "/workspace/borchelt/afrl_cluster_design/src/pysterdesign_single/data/hpe_ex"

def getAllComponents(directory : str, genericPartName : str) -> dict:
    allComponents = {}
    for file in glob(directory+"/*.json"):
        try:
            with open(file , 'r') as componenetFileIn:
                fileJson = json.load(componenetFileIn)
                allComponents.update(fileJson[genericPartName])
        except:
            continue
    return allComponents

for measureStyle, generation in getAllComponents(exChassis, "memory").items():
    for genSpecs in generation:
        for specs in genSpecs["performance"]:
            ramList.append(Memory.RAM(measureStyle, genSpecs["gen"], **specs))

for cpuName, cpuInfo in getAllComponents(exChassis, "cpus").items():
    cpuList.append(CPU.CompUnit(cpuInfo["brand"], cpuInfo["subbrand"], cpuInfo["codename"], cpuName, cpuInfo["cpu_configs"], cpuInfo["price"], cpuInfo["cores"], cpuInfo["cache"], cpuInfo["tdp"], cpuInfo["memory_specs"], cpuInfo["clock_speed"], float(cpuInfo["feature_size"]), cpuInfo["flops_per_cycle"]))


for gpuName, gpuInfo in getAllComponents(exChassis, "gpus").items():
    gpuList.append(GPU.GraphicUnit(gpuName, gpuInfo["clock"], gpuInfo["FP64"], gpuInfo["FP32"], gpuInfo["FP16"], gpuInfo["memory"], gpuInfo["tdp"], gpuInfo["max_gpus"]))

allBlades = []
for bladeName, bladeStats in getAllComponents(exChassis, "blade").items():
    try:
        allBlades.append(GpuBlade.GpuBlade(bladeName, bladeStats["accepted_cpus"], bladeStats["max_cpus"], bladeStats["dimms_per_cpu"], bladeStats["max_dimm_capacity"], bladeStats['memory_gen'], bladeStats["max_transfer_rate"], bladeStats["accepted_gpus"], bladeStats["max_gpus"]))
    except:
        allBlades.append(Blade.Blade(bladeName, bladeStats["accepted_cpus"], bladeStats["max_cpus"], bladeStats["dimms_per_cpu"], bladeStats["max_dimm_capacity"], bladeStats['memory_gen'], bladeStats["max_transfer_rate"]))

#how to add GPUS to GPU blades vs CPU blades
# for blade in allBlades:
#     print(type(blade)==GpuBlade.GpuBlade)

for cpu in cpuList:
    for blade in allBlades:
        newBlade = copy.deepcopy(blade)
        if (newBlade.setCPU(cpu)):
            for ram in ramList:
                newBlade.fastestMemory(ram)
            bladeList.append(newBlade)

allNodes = []
for nodeName, nodeStats in getAllComponents(exChassis, "node").items():
    allNodes.append(Node.ComputeNode(nodeName, nodeStats["height"], nodeStats["accepted_blades"], nodeStats["blades_per_node"]))

removeList = []
for i in range(len(bladeList)):
    if type(bladeList[i]) == GpuBlade.GpuBlade:
        newGpuBlade = copy.deepcopy(bladeList[i])
        for gpu in gpuList:
            if(newGpuBlade.setGPU(gpu)):
                removeList.append(i)
                bladeList.append(newGpuBlade)

bladeList = [i for j, i in enumerate(bladeList) if j not in removeList]

for blade in bladeList:
    for node in allNodes:
        newNode = copy.deepcopy(node)
        if (newNode.setBlade(blade)):
            nodeList.append(newNode)

allChassis = []

for chassisName, chassisStats in getAllComponents(exChassis, "chassis").items():
    allChassis.append(Chassis.Chassis(chassisName, chassisStats["height"]))

for node in nodeList:
    for chassis in allChassis:
        newChassis = copy.deepcopy(chassis)
        newChassis.addItem(node, True)
        chassisList.append(newChassis)

for chassis in chassisList:
    chassis.ownershipCostEfficencyYearly(.1235)
    chassis.totalCostOfOwnership(10, .1235)

# chassisList.sort(key= lambda x: x.yearlyEfficency, reverse=True)
# print("5 most effecient systems per year")
# for i in range(5):
#     chassis = chassisList[i]
#     yFlops = chassis.yearlyFlops
#     oomFlops = 0
#     while yFlops > 1000:
#         oomFlops += 1
#         yFlops /= 1000
#     print(round(yFlops,3),CPU.sizeDecimalLong[oomFlops]+"FLOP/year")

#     watts = chassis.heat
#     oomHeat = 0
#     while watts > 1000:
#         watts /= 1000
#         oomHeat += 1

#     print(round(watts,3),CPU.sizeDecimalLong[oomHeat]+"Watts")


#     oomWH = 0
#     wh = (chassis.heat * 8766)
#     while wh > 1000:
#         wh /= 1000
#         oomWH += 1

#     print(round(wh,3),CPU.sizeDecimalLong[oomWH]+"WattHours per year")

#     print("$"+str(chassis.totalCost))
#     yearEff = chassis.yearlyEfficency
#     oomEff = 0
#     while yearEff > 1000:
#         oomEff += 1
#         yearEff /= 1000
#     print(round(yearEff,3),CPU.sizeDecimalLong[oomEff]+"FLOP/year/$")
#     print(chassis)



for chassis in chassisList:
    chassis.calculateFlops()

chassisList.sort(key= lambda x: x.flops, reverse=True)
effChassList = copy.deepcopy(chassisList)

for chassis in effChassList:
    chassis.calculateEfficiency()
effChassList.sort(key= lambda x: x.heat, reverse=True)

print("5 fastest racks")
for i in range(5):
    flops = chassisList[i].flops
    prefix = 0
    while flops > 1000:
        prefix +=1
        flops/=1000
    print(round(flops,3),CPU.sizeDecimalShort[prefix]+"FLOPs")
    print(chassisList[i])
    print()


# print("5 most efficent(FLOPs/W)")
# for i in range(5):
#     flopEff = effChassList[i].flopEnergyEfficency
#     prefix = 0
#     while flopEff > 1000:
#         prefix +=1
#         flopEff/=1000
#     print(round(flopEff,3),CPU.sizeDecimalShort[prefix]+"FLOPs/Watt")
#     print(chassisList[i])
#     print()


print(len(chassisList))
print(str(round(resource.getrusage(resource.RUSAGE_SELF).ru_maxrss/1000,2))+"MB")