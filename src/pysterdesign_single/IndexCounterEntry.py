#!/usr/local/bin/python3.8
from tkinter import BOTTOM, LEFT, RIGHT, TOP, Button, Entry, IntVar, font
from typing import Union

from ToggleFrame import ToggleFrame
import Colors
from abc import ABCMeta, abstractmethod

class IndexCounterEntry(ToggleFrame):
    __metaclass__ = ABCMeta
    indexVar = None


    def __init__(self, minValue: int=0, maxValue: int=10, step: int = 1, fontSize: int = 16, defaultValue: int=False, tbg: str = "#1E1E1E", tfg: str = "#EBEBEB", bbg: str = "#1E1E1E", bfg: str = "#EBEBEB", abbg: str = "#EBEBEB", abfg: str = "#1E1E1E", *args, **kwargs):

        """A small widget that allows for a user to input a number where they may want +/- one index, thus buttons to change index are useful


        There is an issue currently trying to get it so that a user cannot type a value greater or less than the min/max, weird issue with lambda
        """

        super().__init__(*args, **kwargs)
        self.pack()
        self.update()

        self.minValue = minValue
        self.maxValue = maxValue
        self.step = step
        fontSize = fontSize

        self.indexVar = IntVar(master=self.master, value=(defaultValue if bool(defaultValue) else self.minValue))
        indexEntry = Entry(self, bg=tbg, fg=tfg, insertbackground=tfg, textvariable=self.indexVar, width=4, justify=RIGHT, font=font.Font(size=fontSize))
        indexEntry.bind("<Key>", lambda : self.__indexChange(self.indexVar, self.maxValue, self.minValue))
        indexButtonFrame = ToggleFrame(indexEntry.master, bg=self.master['bg'])

        self.indexUpButton = Button(indexButtonFrame, bg=bbg, fg=bfg, text='▲', font=font.Font(size=(fontSize//4), weight='bold'), command= lambda : (self.indexChange(self.indexVar, self.step, self.maxValue), self.OnAction()), width=1, height=1, activebackground=abbg, activeforeground=abfg)
        self.indexDownButton = Button(indexButtonFrame, bg=bbg, fg=bfg, text='▼', font=font.Font(size=(fontSize//4), weight='bold'), command= lambda : (self.indexChange(self.indexVar, -self.step, self.minValue), self.OnAction()), width=1, height=1, activebackground=abbg, activeforeground=abfg)

        indexButtonFrame.pack(side=RIGHT, anchor='e')

        self.indexUpButton.pack(side=TOP, anchor='n')
        self.indexDownButton.pack(side=BOTTOM, anchor='s')
        indexEntry.pack(side=LEFT, anchor='w')
        self.update()

    def get(self) -> int:
        return self.indexVar.get()

    def set(self, i: int) -> None:
        self.indexVar.set(i)

    @abstractmethod
    def OnAction(self) -> None:
        pass
    
    def limitCheck(self, inputVal : Union[int, list] = 0):
        if type(inputVal) == list:
            return len(inputVal)
        return inputVal
    
    def indexChange(self, currentIndex: IntVar, delta: int, limitIn: Union[int, list] = 0) -> None:
        limit = self.limitCheck(limitIn)

        indexValue = currentIndex.get()

        if delta > 0:
            if (value := indexValue+delta) > limit:
                currentIndex.set(limit)
            else:
                currentIndex.set(value)

        elif delta < 0:
            if (value := indexValue+delta) < limit:
                currentIndex.set(limit)
            else:
                currentIndex.set(value)
        self.OnAction()

    def __indexChange(self, currentIndex: IntVar, maxValue: Union[int, list], minValue: int = 0) -> None:

        maxValue = self.limitCheck(maxValue)

        indexValue = currentIndex.get()
        print(indexValue)

        if indexValue > maxValue:
            currentIndex.set(maxValue)
        elif indexValue < minValue:
            currentIndex.set(minValue)

