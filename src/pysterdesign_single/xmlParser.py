import xml.etree.ElementTree as ET
from CPU import CompUnit

def parseXML(xmlFile):
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
            'cpu_l3_cache': float(item.get('cpu_l3_cache')),
            'cpu_acp': int(item.get('cpu_acp')),
            'cpu_tdp': int(item.get('cpu_tdp')),
            'node_power': int(item.get('node_power').replace('+', '')),
            'node_cpu_count': int(item.get('node_cpu_count').replace('+', '')),
            'node_cost': int(item.get('node_cost').replace('+', '')),
            'description': item.text.strip()
        }
        
        comp_unit = CompUnit(
            brand : item.get('cpu_vendor'),
            subBrand : str,
            codeName : item.get('cpu_codename'),
            model : item.get('cpu_model'),
            cpuConfigs : List[int],
            price : int(item.get('node_cost').replace('+', '')),
            cores : int(item.get('cpu_cores')),
            cache : float(item.get('cpu_l3_cache')),
            tdp : int(item.get('cpu_tdp')),
            memorySpecs : List[dict],
            clockSpeed : Dict[str, float],
            featureSize : float(item.get('cpu_feature_size')),
            flopsPerCycle : int(item.get('cpu_flops_per_cycle'))
        )
        items.append(itemInfo)
    for item in items:
        print(item)
    return items