class CPU(object):
	def __init__(self, cpu_vendor, cpu_codename, cpu_model, cpu_cores, cpu_feature_size, cpu_frequency, cpu_flops_per_cycle, cpu_l3_cache, cpu_tdp, cpu_cost, node_cpu_count=1):
		self.cpu_vendor = cpu_vendor
		self.cpu_codename = cpu_codename
		self.cpu_model = cpu_model
		self.cpu_cores = int(cpu_cores)
		self.cpu_feature_size = int(round(float(cpu_feature_size),0))
		self.cpu_frequency = float(cpu_frequency)
		self.cpu_flops_per_cycle = int(cpu_flops_per_cycle)
		self.cpu_l3_cache = float(cpu_l3_cache)
		self.cpu_tdp = int(cpu_tdp)
		self.cpu_cost = int(round(float(cpu_cost)))
		self.node_cpu_count = int(node_cpu_count)
		
		self.cpu_acp = int(int(cpu_tdp) * .7)
		
	def __str__(self):
		return '<item cpu_vendor="{vnd}" cpu_codename="{ccn}" cpu_model="{cmdl}" cpu_cores="{ccnt}" cpu_feature_size="{csze}e-9" cpu_frequency="{cfrq}" cpu_flops_per_cycle="{flops}" cpu_l3_cache="{cl3}" cpu_acp="{acp}" cpu_tdp="{tdp}" node_power="{npwr}" node_cpu_count="{ncpuc}" node_cost="+{cost}">{vnd} {ccn} {cmdl}</item>'.format(vnd=self.cpu_vendor, ccn=self.cpu_codename, cmdl=self.cpu_model, ccnt=self.cpu_cores, csze=self.cpu_feature_size, cfrq=self.cpu_frequency, flops=self.cpu_flops_per_cycle, cl3=self.cpu_l3_cache, acp=self.cpu_acp, tdp=self.cpu_tdp, npwr="+"+str(self.cpu_tdp), ncpuc="+"+ str(self.node_cpu_count), cost=self.cpu_cost)
