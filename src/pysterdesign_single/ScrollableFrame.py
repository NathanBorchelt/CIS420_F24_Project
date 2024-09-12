#!/usr/bin/python3.8
from tkinter import LEFT, BOTH, RIGHT, Y, Canvas, Scrollbar, Widget, X, TOP
from typing import Type
from ToggleFrame import ToggleFrame

class ScrollableFrame(ToggleFrame):
    canvas = None
    scrollableFrame = None
    scrollbar = None
    scrollabelWidgets = []
    firstTime = True
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        self.canvas = Canvas(self, bg=kwargs['bg'])
        self.scrollbar = Scrollbar(self, orient='vertical', command=self.canvas.yview)

        self.scrollableFrame = ToggleFrame(self.canvas)

        self.scrollableFrame.bind("<Configure>", lambda e: self.canvas.config(scrollregion=self.canvas.bbox('all')))

        self.canvas.create_window((0,0), window=self.scrollableFrame, anchor='nw')

        self.canvas.config(yscrollcommand=self.scrollbar.set)

        self.canvas.pack(side=LEFT, fill=BOTH, expand=True)

        self.scrollableFrame.pack(side=TOP, fill=X, expand=True)

        self.scrollbar.pack(side=RIGHT, fill=Y)

    def getFrame(self) -> ToggleFrame:
        return self.scrollableFrame

    def add(self, widget: Type[Widget]) -> None:
        self.scrollabelWidgets.append(widget)

    def package(self, *args, **kwargs):
        self.pack(*args, **kwargs)
        for widget in self.scrollabelWidgets:
            widget.config(master=self.scrollableFrame)
            widget.pack(side=TOP, anchor='nw', expand=True, fill=X)
