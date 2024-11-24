#!/usr/local/bin/python3.8

from abc import abstractmethod


class RackMount(object):

    def __init__(self, name: str, height : float):
        self.height = height
        self.name = name

    def getHeight(self):
        return self.height

    def setHeight(self, value):
        self.height = value

    def getName(self):
        return self.name

    def setName(self, value):
        self.name = value


    @abstractmethod
    def calcPrice(self):
        pass

    @abstractmethod
    def calculateHeat(self):
        pass