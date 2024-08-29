class ProcessingUnit:
	def __init__(self, model, vendor, formFactor, cost, power, weight, size):
		self.model = model
		self.vendor = vendor
		self.formFactor = formFactor
		self.cost = cost
		self.power = power #WATTS
		self.weight = weight #in kg, seems to be rounded
		self.size = size # number of rack units (xU) per processing unit
