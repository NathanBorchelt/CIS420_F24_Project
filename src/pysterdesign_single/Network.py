#!/usr/local/bin/python3.8
#Nathan Borchelt


class Network(object):

    def __init__(self, tech: str, portCount : int, speed: float):
        self.tech = tech
        self.portCount = portCount
        self.speed = speed

    def getTech(self) -> str:
        return self.tech

    def getPortCount(self) -> int:
        return self.portCount

    def getSpeed(self) -> float:
        return self.speed



    