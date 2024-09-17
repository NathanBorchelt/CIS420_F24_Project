#!/usr/local/bin/python3.8

from abc import abstractmethod


class RackMount(object):

    def __init__(self, name: str, height : float):
        self.height = height
        self.name = name

    @abstractmethod
    def calcPrice(self):
        pass