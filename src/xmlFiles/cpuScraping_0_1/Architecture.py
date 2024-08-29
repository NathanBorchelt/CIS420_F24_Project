class Architecture(object):
	def __init__(self, name, codename, size):
		self.name = name
		self.codename = codename
		self.size = size
		
	def getName(self):
		return self.name
		
	def getSize(self):
		return self.size
		
	def getCodename(self):
		return self.codename
