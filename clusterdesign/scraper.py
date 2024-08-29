import requests

#f = open("scrapings.txt", "w")
url = 'https://en.wikichip.org/wiki/amd/epyc'
r = requests.get(url)
#print(r.text, file=f)
#f.close()

cpu_vendor = ""
cpu_codename = ""
cpu_model = ""
cpu_cores = ""
cpu_featue_size = ""
cpu_flops_per_cycle = ""
cpu_l3_cache = ""
cpu_acp = ""
cpu_tdp = ""
node_power = ""
node_cpu_count = ""
node_cost = ""

with open(

