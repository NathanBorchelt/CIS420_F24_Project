#!/usr/local/bin/python3.8

from tkinter import Radiobutton, StringVar, Entry, Scrollbar, font

from ToggleFrame import ToggleFrame


class URLRadioButton(ToggleFrame):
    radioButton = None
    radioButtonVar = None
    urlEntryVar = None

    def __init__(self, algorithmName: str, radioButtionVar: StringVar, urlVar: StringVar, buttonDictionary: dict, buttonTextFG: str=None,buttonTextBG: str=None, urlEntryTextFG: str=None,urlEntryTextBG: str=None, *args, **kwargs):
        super().__init__(*args, **kwargs)

        NORMAL_FONT = font.Font(size=11)

        if buttonTextBG == None:
            buttonTextBG = kwargs["bg"]
        if buttonTextFG == None:
            buttonTextFG = kwargs["fg"]

        if urlEntryTextBG == None:
            urlEntryTextBG = kwargs["bg"]
        if urlEntryTextFG == None:
            urlEntryTextFG = kwargs["fg"]

        self.pack()
        self.update()

        self.radioButton = Radiobutton(self, text=algorithmName, variable=radioButtionVar, value=)
        self.radioButtonVar = radioButtionVar
        self.urlEntryVar = urlVar
        
        

        

