#!/usr/local/bin/python3.8

class RAM(object):

    def __init__(self, memSpecType : str, generation : int, speed : int, capacity : int, price : int):
        self.memSpecType = memSpecType
        self.generation = generation
        self.speed = speed
        self.capacity = capacity
        self.price = price
        self.score = capacity * speed

    def __str__(self) -> str:
        return "{cap}GiB of {memT}{gen} @ {spd}MHz for ${prc} per dimm".format(cap=self.capacity, memT=self.memSpecType, gen=self.generation, spd=self.speed, prc=self.price)