#!/usr/bin/python3.8
#pip3.8 install tkinter-tooltip


try:
    from tkinter import BOTH, BOTTOM, LEFT, NONE, RAISED, RIGHT, SUNKEN, TOP, X, Y, Button, Checkbutton, DoubleVar, Entry, Frame, IntVar, Label, OptionMenu, PhotoImage, Radiobutton, Scrollbar, StringVar, Text, Tk, font
except ImportError:
   from Tkinter import BOTH, BOTTOM, LEFT, NONE, RAISED, RIGHT, SUNKEN, TOP, X, Y, Button, Checkbutton, DoubleVar, Entry, Frame, IntVar, Label, OptionMenu, PhotoImage, Radiobutton, Scrollbar, StringVar, Text, Tk, font

hasResource : bool = False
try:
    import resource
    hasResource = True
except ImportError:
    pass
from copy import deepcopy
from sys import argv
from tkinter import END, Toplevel, filedialog
import traceback
from typing import List, Tuple, Type, Union

from ToggleFrame import ToggleFrame

import Colors
from ScrollableFrame import ScrollableFrame
from IndexCounterEntry import IndexCounterEntry
import PluginAPI

import Blade, Chassis, RackMount
import xmlParser
from jsonparser import parseJsonTree
import json



'''
Universal methods that are designed to be usable anywhere, thus must come first, even though they get assined to static variables
'''

colorDict = Colors.ColorDictionary

#https://lindevs.com/code-snippets/get-screen-size-of-each-monitor-using-python
def getDisplaySize():
    from screeninfo import get_monitors
    smallestMonitor = [float('inf'), float('inf')]

    for monitor in get_monitors():
        if ((monitor.width * monitor.height) < (smallestMonitor[0] * smallestMonitor[1])):
            smallestMonitor = [monitor.width, monitor.height]
    return smallestMonitor

SCREEN_RESOLUTION = (2560,1080) #getDisplaySize()
TITLE = "Computer Cluster Design Tool"



def selectRadioButton_NetworkAlgorithms(selectedRB: Radiobutton, allRB: List[Radiobutton], extraInfo = None) -> None:
    selectedRB.config(fg=colorDict["network_btn_alt_fg"], bg=colorDict["network_btn_alt_bg"])
    for rButton in allRB:
        if rButton != selectedRB:
            rButton.config(bg=colorDict["network_btn_bg"], fg=colorDict["network_btn_fg"])
    if extraInfo != None:
        print(extraInfo)

def selectRadioButton_UPSAlgorithms(selectedRB: Radiobutton, allRB: List[Radiobutton], extraInfo = None) -> None:
    selectedRB.config(fg=colorDict["ups_btn_alt_fg"], bg=colorDict["ups_btn_alt_bg"])
    for rButton in allRB:
        if rButton != selectedRB:
            rButton.config(bg=colorDict["ups_btn_bg"], fg=colorDict["ups_btn_fg"])
    if extraInfo != None:
        print(extraInfo)

class NodesFrame(ToggleFrame):
    pixel = None
    nodeTextBoxOutput : Text = None

    def cleanupCPUs(frame, doIt: bool) -> None:
        #Frame issue again, passing the var passes two arguments, do not understand why
        print("cleanup:",doIt)

    def imposeNodeLevelConstraints(frame, constraints : Union[str, List[str]], outputVar : StringVar, outBox : Text) -> None:
        print("constraints:",constraints)
        outputVar.set(constraints)
        outBox['state'] = 'normal'
        try:
            outBox.delete("1.0",'end')
        except:
            print("not doing it")
            pass
        outBox.insert('end',constraints)
        outBox['state'] = 'disabled'

    #need ot see how this work, this may simply sort the cpus, but will have to determin that to see how this is affected, may become a sorting algorith, or may just change boolean flags
    def executeHeuristic(frame, functions: dict, heuristic: str) -> Union[bool, str]: #will take in CPU stats, need to create object, then return boolean, potentially cpu itself if cpu has enabled
        #I am unsure what the hell is going on here. when I put in the list of classes, it is senting the frame aswell. unsure why. investigate later, tmep fix for now
        for action in functions:
            if(functions[action]["import"].identity() == heuristic):
                print(action)
                #need ot creeat a more global variable, likely a list of cpus with enable flaggs that this will loop through
                return action
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        padx = colorDict["node_padx"]
        pady = colorDict["node_pady"]
        bg = colorDict["node_bg"]
        txt_bg = colorDict["node_txt_bg"]
        txt_fg = colorDict["node_txt_fg"]
        ent_bg = colorDict["node_ent_bg"]
        ent_fg = colorDict["node_ent_fg"]
        btn_bg = colorDict["node_btn_bg"]
        btn_fg = colorDict["node_btn_fg"]
        btn_alt_bg = colorDict["node_btn_alt_bg"]
        btn_alt_fg = colorDict["node_btn_alt_fg"]

        NORMAL_FONT = font.Font(size=11)
        self.pixel = PhotoImage()
        simpleHeuristicVar = StringVar()
        nodeOutputStringVar = StringVar()

        self.pack()
        self.update()
        nodesOptions = ToggleFrame(self, padx=padx, pady=pady, height=int(self.winfo_height()*.4), width=self.winfo_width(),relief=SUNKEN, bg=bg)
        nodesTextOutputFrame = ToggleFrame(self, padx=padx, pady=pady, height=int(self.winfo_height()*.6), width=self.winfo_width(),relief=SUNKEN, bg=bg)
        nodesOptions.pack(fill=BOTH, expand=True)
        nodesTextOutputFrame.pack(fill=BOTH, expand=True)
        self.update()

        constraintsFrame = ToggleFrame(nodesOptions, padx=padx, pady=pady, width=int(nodesOptions.winfo_width()*.5), height=nodesOptions.winfo_height(),relief=SUNKEN, bg=bg)
        heuristicsFrame = ToggleFrame(nodesOptions, padx=padx, pady=pady, width=int(nodesOptions.winfo_width()*.5), height=nodesOptions.winfo_height(),relief=SUNKEN, bg=bg)

        constraintsFrame.pack(fill=BOTH, expand=True, side=LEFT)
        #constraintsFrame.pack_propagate(0)
        heuristicsFrame.pack(fill=BOTH, expand=True, side=LEFT)
        self.update()

        nodeTextBoxOutput = Text(nodesTextOutputFrame, font=NORMAL_FONT, bg=ent_bg, fg=ent_fg, exportselection=0, state='disabled', wrap=NONE, insertbackground=ent_fg)

        nodeTextOutputYScrollBar = Scrollbar(nodesTextOutputFrame, orient="vertical")
        nodeTextOutputXScrollBar = Scrollbar(nodesTextOutputFrame, orient="horizontal")

        nodeTextBoxOutput.config(yscrollcommand=nodeTextOutputYScrollBar.set, xscrollcommand=nodeTextOutputXScrollBar.set)

        nodeTextOutputYScrollBar.config(command=nodeTextBoxOutput.yview)
        nodeTextOutputXScrollBar.config(command=nodeTextBoxOutput.xview)

        nodeTextOutputYScrollBar.pack(side=RIGHT , fill=Y)
        nodeTextBoxOutput.pack(fill=BOTH, expand=True)
        nodeTextOutputXScrollBar.pack(side=BOTTOM , fill=X)

        self.update()

        nodeConstraintLabel = Label(constraintsFrame, text="Node-level constraints:",padx=padx, pady=pady,font=NORMAL_FONT, bg=txt_bg, fg=txt_fg)
        nodeConstraintInput = Text(constraintsFrame,height=7, font=NORMAL_FONT, bg=ent_bg, fg=ent_fg, insertbackground=ent_fg)
        nodeConstraintScrollbar = Scrollbar(nodeConstraintInput)
        nodeConstraintInput.config(yscrollcommand=nodeConstraintScrollbar.set)

        nodeConstraintScrollbar.config(command=nodeConstraintInput.yview)

        nodeConstraintButton = Button(constraintsFrame, compound='c',image=self.pixel,text="Impose node-level constrainsts", padx=padx, pady=pady, bg=btn_bg, fg=btn_fg, activeforeground=btn_alt_fg, activebackground=btn_alt_bg, command=lambda: self.imposeNodeLevelConstraints(nodeConstraintInput.get('1.0', 'end-1c'),nodeOutputStringVar,nodeTextBoxOutput))

        nodeConstraintButton.pack(anchor='w', side=TOP, expand=False)
        nodeConstraintLabel.pack(anchor='w', side=TOP, expand=False)
        nodeConstraintInput.pack(anchor='w', side=TOP, expand=True, fill=BOTH)
        nodeConstraintScrollbar.pack(side="right", fill=Y)

        self.update()

        applyHeuristicsFrame = ToggleFrame(heuristicsFrame, bg=bg, padx=padx, pady=pady, width=heuristicsFrame.winfo_width(), height=int(heuristicsFrame.winfo_height()*.5), relief=SUNKEN)
        cleanupFrame = ToggleFrame(heuristicsFrame, bg=bg, padx=padx, pady=pady, width=heuristicsFrame.winfo_width(), height=int(heuristicsFrame.winfo_height()*.5), relief=SUNKEN)

        applyHeuristicsFrame.pack(fill=BOTH, side=TOP, expand=True)
        cleanupFrame.pack(fill=BOTH, side=BOTTOM, expand=True)

        heuristicNames = []
        heuristicFunctions = PluginAPI.getPlugins("heuristics")
        for heuristicFile in heuristicFunctions:
            heuristicNames.append(heuristicFunctions[heuristicFile]["import"].identity())
        self.update()
        simpleHeuristicVar.set(heuristicNames[0])
        heuristicOptions = OptionMenu(applyHeuristicsFrame, simpleHeuristicVar, *heuristicNames,)

        heuristicOptions.config(bg=btn_bg, fg=btn_fg, activebackground=btn_alt_bg, activeforeground=btn_alt_fg)
        heuristicOptions["menu"].config(bg=btn_bg, fg=btn_fg)

        applyHeuristicsButton = Button(applyHeuristicsFrame,text="Apply simple heuristics", command=lambda :self.executeHeuristic(heuristicFunctions,simpleHeuristicVar.get()), fg=btn_fg, bg=btn_bg, activebackground=btn_alt_bg, activeforeground=btn_alt_fg)

        applyHeuristicsButton.pack(anchor="nw" , side=TOP)
        heuristicOptions.pack(anchor="nw" , side=TOP )

        self.update()

        doCleanup = IntVar()
        doCleanup.set(0)
        cleanupDeleteCheckbox = Checkbutton(cleanupFrame,text="Delete Disabled Configurations", variable=doCleanup, bg=btn_bg, fg=btn_fg, activebackground=btn_alt_bg, activeforeground=btn_alt_fg)
        cleanupDeleteButton = Button(cleanupFrame, text="Clean up now", command=lambda :self.cleanupCPUs(doCleanup.get()), bg=btn_bg, fg=btn_fg, activebackground=btn_alt_bg, activeforeground=btn_alt_fg)

        cleanupDeleteButton.pack(anchor="sw", side=BOTTOM)
        cleanupDeleteCheckbox.pack(anchor="sw", side=BOTTOM)
        self.update()

    def updateOutput(self):
        self.nodeTextBoxOutput.delete(1.0, "END")
        self.nodeTextBoxOutput.insert("END", CONFIGURATIONS)

class UPSFrame(ToggleFrame):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        padx = colorDict["ups_padx"]
        pady = colorDict["ups_pady"]
        bg = colorDict["ups_bg"]
        txt_bg = colorDict["ups_txt_bg"]
        txt_fg = colorDict["ups_txt_fg"]
        ent_bg = colorDict["ups_ent_bg"]
        ent_fg = colorDict["ups_ent_fg"]
        btn_bg = colorDict["ups_btn_bg"]
        btn_fg = colorDict["ups_btn_fg"]
        btn_alt_bg = colorDict["ups_btn_alt_bg"]
        btn_alt_fg = colorDict["ups_btn_alt_fg"]

        NORMAL_FONT = font.Font(size=11)
        self.pixel = PhotoImage()
        self.pack()
        self.update()

        urlInputFrame = ToggleFrame(self, padx=padx, pady=pady, height=int(self.winfo_height()*.2), width=self.winfo_width(), relief=SUNKEN, bg=bg)
        inputTextFrame = ToggleFrame(urlInputFrame, padx=padx, pady=pady, height=urlInputFrame.winfo_height(), width=int(urlInputFrame.winfo_width()*.75), relief=SUNKEN, bg=bg)

        urlInputFrame.pack(fill=BOTH, side=TOP, anchor='n')
        inputTextFrame.pack(fill=X, side=LEFT, expand=True)
        self.update()

        loadPerModels = Button(urlInputFrame, compound='c',image=self.pixel,text="Load", padx=padx, pady=pady, bg=btn_bg, fg=btn_fg, activebackground=btn_alt_bg, activeforeground=btn_alt_fg, command=lambda: print("load models"))

        loadPerModels.pack(anchor="e", side=RIGHT)

        urlInputLabel = Label(inputTextFrame, bg=txt_bg, fg=txt_fg, font=NORMAL_FONT, text="Load models automatically using:")
        loadModelURLInput = Text(inputTextFrame,height=1, width=50, font=NORMAL_FONT, bg=ent_bg, fg=ent_fg, insertbackground=ent_fg, wrap=NONE)
        modelURLScrollBar = Scrollbar(loadModelURLInput.master, orient="horizontal", command=loadModelURLInput.xview)
        loadModelURLInput.config(xscrollcommand=modelURLScrollBar.set)

        urlInputLabel.pack(anchor='w', side=TOP, expand=False)
        loadModelURLInput.pack(anchor='w',fill=X, expand=True)
        modelURLScrollBar.pack(side=BOTTOM, fill=X)

        upsModelFrame = ToggleFrame(self, padx=padx, pady=pady, width=int(self.winfo_width()*.5), relief=SUNKEN, bg=bg)

        upsModelFrame.pack(side=TOP, anchor='nw')

        upsModelLabel = Label(upsModelFrame, bg=txt_bg, fg=txt_fg, text="UPS Model", font=NORMAL_FONT)
        upsModelSendFrame = ToggleFrame(upsModelFrame, bg=bg)
        upsModelReceiveFrame = ToggleFrame(upsModelFrame, bg=bg)

        upsModelSendLabel = Label(upsModelSendFrame, bg=txt_bg, fg=txt_fg, font=NORMAL_FONT, text="Send required power in: ")
        upsModelReceiveLabel = Label(upsModelReceiveFrame, bg=txt_bg, fg=txt_fg, font=NORMAL_FONT, text="Receive UPS cost in: ")

        upsModelSendVar = StringVar(self, value="power")
        upsModelReceiveVar = StringVar(self, value="ups_cost")
        upsModelSendEntry = Entry(upsModelSendFrame, bg=ent_bg, fg=ent_fg, insertbackground=ent_fg, font=NORMAL_FONT, textvariable=upsModelSendVar, justify=RIGHT)
        upsModelReceiveEntry = Entry(upsModelReceiveFrame, bg=ent_bg, fg=ent_fg, insertbackground=ent_fg, font=NORMAL_FONT, textvariable=upsModelReceiveVar, justify=RIGHT)

        upsModelLabel.pack(side=TOP, anchor='nw')
        upsModelSendFrame.pack(side=TOP, anchor='nw', fill=X)
        upsModelReceiveFrame.pack(side=TOP, anchor='nw', fill=X)
        upsModelSendLabel.pack(side=LEFT, anchor='w')
        upsModelReceiveLabel.pack(side=LEFT, anchor='w')
        upsModelSendEntry.pack(side=RIGHT, anchor='e')
        upsModelReceiveEntry.pack(side=RIGHT, anchor='e')

        additionalIOFrame = ToggleFrame(self,padx=padx, pady=pady, bg=bg)
        additionalIOFrame.pack(side=TOP, anchor='n', fill=X, expand=True)
        self.update()

        additionalSendFrame = ToggleFrame(additionalIOFrame,padx=padx, pady=pady, width=int(additionalIOFrame.winfo_width()*.5), bg=bg)
        additionalReceiveFrame = ToggleFrame(additionalIOFrame,padx=padx, pady=pady, width=int(additionalIOFrame.winfo_width()*.5), bg=bg)

        additionalSendLabel = Label(additionalSendFrame, bg=txt_bg, fg=txt_fg, font=NORMAL_FONT, text="Also send the following parameters:")
        additionalReceiveLabel = Label(additionalReceiveFrame, bg=txt_bg, fg=txt_fg, font=NORMAL_FONT, text="Also send the following parameters:")

        additionalSendTextFrame = ToggleFrame(additionalSendFrame, bg=bg)
        additionalSendTextEntry = Text(additionalSendTextFrame, width=additionalSendTextFrame.winfo_width()//11, bg=ent_bg, fg=ent_fg, insertbackground=ent_fg, wrap=NONE, height=6, font=NORMAL_FONT)
        additionalSendXScrollBar = Scrollbar(additionalSendTextEntry.master, orient='horizontal', command=additionalSendTextEntry.xview)
        additionalSendYScrollBar = Scrollbar(additionalSendTextEntry.master, orient='vertical', command=additionalSendTextEntry.yview)
        additionalSendTextEntry.config(xscrollcommand=additionalSendXScrollBar.set, yscrollcommand=additionalSendYScrollBar.set)

        additionalReceiveTextFrame = ToggleFrame(additionalReceiveFrame)
        additionalReceiveTextEntry = Text(additionalReceiveTextFrame, width=additionalReceiveTextFrame.winfo_width()//11, bg=ent_bg, fg=ent_fg, insertbackground=ent_fg, wrap=NONE, height=6, font=NORMAL_FONT)
        additionalReceiveXScrollBar = Scrollbar(additionalReceiveTextEntry.master, orient='horizontal', command=additionalReceiveTextEntry.xview)
        additionalReceiveYScrollBar = Scrollbar(additionalReceiveTextEntry.master, orient='vertical', command=additionalReceiveTextEntry.yview)
        additionalReceiveTextEntry.config(xscrollcommand=additionalReceiveXScrollBar.set, yscrollcommand=additionalReceiveYScrollBar.set)

        additionalSendFrame.pack(side=LEFT, anchor='nw', fill=X, expand=True)
        additionalSendLabel.pack(side=TOP, anchor='nw')
        additionalSendTextFrame.pack(side=TOP, anchor='nw', fill=X, expand=True)
        additionalSendYScrollBar.pack(side=RIGHT, anchor='e', fill=Y)
        additionalSendTextEntry.pack(side=TOP, anchor='nw', fill=X, expand=True)
        additionalSendXScrollBar.pack(side=BOTTOM, anchor='s', fill=X, expand=True)

        additionalReceiveFrame.pack(side=LEFT, anchor='nw', fill=X, expand=True)
        additionalReceiveLabel.pack(side=TOP, anchor='nw')
        additionalReceiveTextFrame.pack(side=TOP, anchor='nw', fill=X, expand=True)
        additionalReceiveYScrollBar.pack(side=RIGHT, anchor='e', fill=Y)
        additionalReceiveTextEntry.pack(side=TOP, anchor='nw', fill=X, expand=True)
        additionalReceiveXScrollBar.pack(side=BOTTOM, anchor='s', fill=X, expand=True)

        self.update()

        upsModelingAlgorithmFrame = ScrollableFrame(self, bg=bg, padx=padx, pady=pady)
        upsModelingAlgorithmFrame.package(side=TOP, anchor='n', fill=BOTH, expand=True)

        self.update()

        algorithmDictionary = {}
        algorithmFunctions = PluginAPI.getPlugins("ups")

        for algorithm in algorithmFunctions:
            algorithmDictionary.update({
                algorithmFunctions[algorithm]['import'].identity() :
                    {"algorithm" : algorithmFunctions[algorithm]['import'].execute,
                    "url" : algorithmFunctions[algorithm]['import'].url(),
                    "return" : algorithmFunctions[algorithm]['import'].identity()
            }})

        upsAlgorithmInputDict = {}
        upsAlgorithmButtonList = []

        upsAlgorithmSeclection = StringVar(master=self,value=algorithmDictionary[list(algorithmDictionary)[0]]["return"])
        for dictionary in algorithmDictionary:

            upsAlgorithmInputDict.update({
                algorithmDictionary[dictionary]['return']:{
                    "frame" : ToggleFrame(upsModelingAlgorithmFrame.getFrame(), bg=bg, padx=1,pady=1),
                }
            })
            upsAlgorithmInputDict[algorithmDictionary[dictionary]['return']].update({
                "button" : Radiobutton(upsAlgorithmInputDict[algorithmDictionary[dictionary]['return']]['frame'], text=algorithmDictionary[dictionary]['return'], variable=upsAlgorithmSeclection, value=algorithmDictionary[dictionary]['return'], indicatoron=0, bg=btn_bg, fg=btn_fg, font=NORMAL_FONT, activebackground=btn_alt_bg, activeforeground=btn_alt_fg),
                "stringVar" : StringVar(master=upsAlgorithmInputDict[algorithmDictionary[dictionary]['return']]['frame'], value=algorithmDictionary[dictionary]['url'])
            })
            upsAlgorithmInputDict[algorithmDictionary[dictionary]['return']].update({
                "entry": Entry(upsAlgorithmInputDict[algorithmDictionary[dictionary]['return']]['frame'], textvariable=upsAlgorithmInputDict[algorithmDictionary[dictionary]['return']]["stringVar"], bg=ent_bg, fg=ent_fg, insertbackground=ent_fg)
            })

        for actionValues in upsAlgorithmInputDict.values():
            upsAlgorithmButtonList.append(actionValues['button'])
            actionValues['button'].config(command= lambda :selectRadioButton_UPSAlgorithms(actionValues['button'], upsAlgorithmButtonList, actionValues['stringVar'].get()))
            actionValues['frame'].pack(fill=X, side=TOP, anchor='n', expand=True)
            actionValues['button'].pack(side=LEFT, anchor='w')
            actionValues['entry'].pack(side=LEFT, anchor='e', fill=X, expand=True)

        upsModelingAlgorithmFrame.package(side=TOP, anchor='n', fill=BOTH, expand=True)
        self.update()

class SettingsFrame(ToggleFrame):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        fontSize = 14
        NORMAL_FONT = font.Font(size=fontSize)

        padx = colorDict["settings_padx"]
        pady = colorDict["settings_pady"]
        bg = colorDict["settings_bg"]
        txt_bg = colorDict["settings_txt_bg"]
        txt_fg = colorDict["settings_txt_fg"]
        ent_bg = colorDict["settings_ent_bg"]
        ent_fg = colorDict["settings_ent_fg"]
        btn_bg = colorDict["settings_btn_bg"]
        btn_fg = colorDict["settings_btn_fg"]
        btn_alt_bg = colorDict["settings_btn_alt_bg"]
        btn_alt_fg = colorDict["settings_btn_alt_fg"]

        widthLimitFrame = ToggleFrame(self, padx=padx, pady=pady, bg=bg)

        rackHeightFrame = ToggleFrame(widthLimitFrame, padx=padx, pady=pady, bg=bg)
        systemLifetimeFrame = ToggleFrame(widthLimitFrame, padx=padx, pady=pady, bg=bg)
        pricekWhFrame = ToggleFrame(widthLimitFrame, padx=padx, pady=pady, bg=bg)
        yearlyRentalFrame = ToggleFrame(widthLimitFrame, padx=padx, pady=pady, bg=bg)

        rackHeightLabel = Label(rackHeightFrame, bg=txt_bg, fg=txt_fg, text="Rack height, in units:      ", font=NORMAL_FONT)
        rackHeightEntry = IndexCounterEntry(0, (2**31-1), 1, fontSize, 42, ent_bg, ent_fg, btn_bg, btn_fg, btn_alt_bg, btn_alt_fg, master=rackHeightLabel.master, bg=rackHeightLabel['bg'])
        systemLifetimeLabel = Label(systemLifetimeFrame, bg=txt_bg, fg=txt_fg, text="System lifetime, in years:      ", font=NORMAL_FONT)
        systemLifetimeEntry = IndexCounterEntry(0, (2**31-1), 1, fontSize, 3, ent_bg, ent_fg, btn_bg, btn_fg, btn_alt_bg, btn_alt_fg, master=systemLifetimeLabel.master, bg=rackHeightLabel['bg'])
        pricekWhLabel = Label(pricekWhFrame, bg=txt_bg, fg=txt_fg, text="Price of kW*hour of electricity:      ", font=NORMAL_FONT)
        pricekWhVar = DoubleVar(master=pricekWhLabel.master, value=0.35)
        pricekWhEntry = Entry(master=pricekWhLabel.master, bg=ent_bg, fg=ent_fg, insertbackground=ent_fg, textvariable=pricekWhVar, font=NORMAL_FONT, width=7, justify=RIGHT)
        yearlyRentalLabel = Label(yearlyRentalFrame, bg=txt_bg, fg=txt_fg, text="Yearly operating expense per rack:      ", font=NORMAL_FONT)
        yearlyRentalVar = IntVar(master=yearlyRentalLabel.master, value=3000)
        yearlyRentalEntry = Entry(master=yearlyRentalLabel.master, bg=ent_bg, fg=ent_fg, insertbackground=ent_fg, textvariable=yearlyRentalVar, font=NORMAL_FONT, width=7, justify=RIGHT)

        widthLimitFrame.pack(side=TOP, anchor='nw')

        rackHeightFrame.pack(side=TOP, anchor='nw', fill=X)
        systemLifetimeFrame.pack(side=TOP, anchor='nw', fill=X)
        pricekWhFrame.pack(side=TOP, anchor='nw', fill=X)
        yearlyRentalFrame.pack(side=TOP, anchor='nw', fill=X)

        rackHeightLabel.pack(side=LEFT, anchor='w')
        rackHeightEntry.pack(side=RIGHT, anchor='e')

        systemLifetimeLabel.pack(side=LEFT, anchor='w')
        systemLifetimeEntry.pack(side=RIGHT, anchor='e')

        pricekWhLabel.pack(side=LEFT, anchor='w')
        pricekWhEntry.pack(side=RIGHT, anchor='e')

        yearlyRentalLabel.pack(side=LEFT, anchor='w')
        yearlyRentalEntry.pack(side=RIGHT, anchor='e')

class NetworkFrame(ToggleFrame):

    pixel=None

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        padx = colorDict["network_padx"]
        pady = colorDict["network_pady"]
        bg = colorDict["network_bg"]
        txt_bg = colorDict["network_txt_bg"]
        txt_fg = colorDict["network_txt_fg"]
        ent_bg = colorDict["network_ent_bg"]
        ent_fg = colorDict["network_ent_fg"]
        btn_bg = colorDict["network_btn_bg"]
        btn_fg = colorDict["network_btn_fg"]
        btn_alt_bg = colorDict["network_btn_alt_bg"]
        btn_alt_fg = colorDict["network_btn_alt_fg"]

        NORMAL_FONT = font.Font(size=11)
        self.pixel = PhotoImage()
        self.pack()
        self.update()

        urlInputFrame = ToggleFrame(self, padx=padx, pady=pady, height=int(self.winfo_height()*.2), width=self.winfo_width(), relief=SUNKEN, bg=bg)
        inputTextFrame = ToggleFrame(urlInputFrame, padx=padx, pady=pady, height=urlInputFrame.winfo_height(), width=int(urlInputFrame.winfo_width()*.75), relief=SUNKEN, bg=bg)

        urlInputFrame.pack(fill=BOTH, side=TOP, anchor='n')
        inputTextFrame.pack(fill=X, side=LEFT, expand=True)
        self.update()

        loadPerModels = Button(urlInputFrame, compound='c',image=self.pixel,text="Load", padx=padx, pady=pady, bg=btn_bg, fg=btn_fg, activebackground=btn_alt_bg, activeforeground=btn_alt_fg, command=lambda: print("load models"))

        loadPerModels.pack(anchor="e", side=RIGHT)

        urlInputLabel = Label(inputTextFrame, bg=txt_bg, fg=txt_fg, font=NORMAL_FONT, text="Load models automatically using:")
        loadModelURLInput = Text(inputTextFrame,height=1, width=50, font=NORMAL_FONT, bg=ent_bg, fg=ent_fg, insertbackground=ent_fg, wrap=NONE)
        modelURLScrollBar = Scrollbar(loadModelURLInput.master, orient="horizontal", command=loadModelURLInput.xview)
        loadModelURLInput.config(xscrollcommand=modelURLScrollBar.set)

        urlInputLabel.pack(anchor='w', side=TOP, expand=False)
        loadModelURLInput.pack(anchor='w',fill=X, expand=True)
        modelURLScrollBar.pack(side=BOTTOM, fill=X)

        networkModelFrame = ToggleFrame(self, padx=padx, pady=pady, width=int(self.winfo_width()*.5), relief=SUNKEN, bg=bg)

        networkModelFrame.pack(side=TOP, anchor='nw')

        networkModelLabel = Label(networkModelFrame, bg=txt_bg, fg=txt_fg, text="Network Model", font=NORMAL_FONT)
        networkModelSendFrame = ToggleFrame(networkModelFrame, bg=bg)
        networkModelReceiveFrame = ToggleFrame(networkModelFrame, bg=bg)

        networkModelSendLabel = Label(networkModelSendFrame, bg=txt_bg, fg=txt_fg, font=NORMAL_FONT, text="Send the number of nodes in: ")
        networkModelReceiveLabel = Label(networkModelReceiveFrame, bg=txt_bg, fg=txt_fg, font=NORMAL_FONT, text="Receive network cost in: ")

        netowrkModelSendVar = StringVar(self, value="nodes")
        netowrkModelReceiveVar = StringVar(self, value="network_cost")
        netowrkModelSendEntry = Entry(networkModelSendFrame, bg=ent_bg, fg=ent_fg, insertbackground=ent_fg, font=NORMAL_FONT, textvariable=netowrkModelSendVar, justify=RIGHT)
        netowrkModelReceiveEntry = Entry(networkModelReceiveFrame, bg=ent_bg, fg=ent_fg, insertbackground=ent_fg, font=NORMAL_FONT, textvariable=netowrkModelReceiveVar, justify=RIGHT)

        networkModelLabel.pack(side=TOP, anchor='nw')
        networkModelSendFrame.pack(side=TOP, anchor='nw', fill=X)
        networkModelReceiveFrame.pack(side=TOP, anchor='nw', fill=X)
        networkModelSendLabel.pack(side=LEFT, anchor='w')
        networkModelReceiveLabel.pack(side=LEFT, anchor='w')
        netowrkModelSendEntry.pack(side=RIGHT, anchor='e')
        netowrkModelReceiveEntry.pack(side=RIGHT, anchor='e')

        additionalIOFrame = ToggleFrame(self,padx=padx, pady=pady, bg=bg)
        additionalIOFrame.pack(side=TOP, anchor='n', fill=X, expand=True)
        self.update()

        additionalSendFrame = ToggleFrame(additionalIOFrame,padx=padx, pady=pady, width=int(additionalIOFrame.winfo_width()*.5), bg=bg)
        additionalReceiveFrame = ToggleFrame(additionalIOFrame,padx=padx, pady=pady, width=int(additionalIOFrame.winfo_width()*.5), bg=bg)

        additionalSendLabel = Label(additionalSendFrame, bg=txt_bg, fg=txt_fg, font=NORMAL_FONT, text="Also send the following parameters:")
        additionalReceiveLabel = Label(additionalReceiveFrame, bg=txt_bg, fg=txt_fg, font=NORMAL_FONT, text="Also send the following parameters:")

        additionalSendTextFrame = ToggleFrame(additionalSendFrame, bg=bg)
        additionalSendTextEntry = Text(additionalSendTextFrame, width=additionalSendTextFrame.winfo_width()//11,bg=ent_bg, fg=ent_fg, insertbackground=ent_fg, wrap=NONE, height=6, font=NORMAL_FONT)
        additionalSendXScrollBar = Scrollbar(additionalSendTextEntry.master, orient='horizontal', command=additionalSendTextEntry.xview)
        additionalSendYScrollBar = Scrollbar(additionalSendTextEntry.master, orient='vertical', command=additionalSendTextEntry.yview)
        additionalSendTextEntry.config(xscrollcommand=additionalSendXScrollBar.set, yscrollcommand=additionalSendYScrollBar.set)

        additionalReceiveTextFrame = ToggleFrame(additionalReceiveFrame, bg=bg)
        additionalReceiveTextEntry = Text(additionalReceiveTextFrame, width=additionalReceiveTextFrame.winfo_width()//11,bg=ent_bg, fg=ent_fg, insertbackground=ent_fg, wrap=NONE, height=6, font=NORMAL_FONT)
        additionalReceiveXScrollBar = Scrollbar(additionalReceiveTextEntry.master, orient='horizontal', command=additionalReceiveTextEntry.xview)
        additionalReceiveYScrollBar = Scrollbar(additionalReceiveTextEntry.master, orient='vertical', command=additionalReceiveTextEntry.yview)
        additionalReceiveTextEntry.config(xscrollcommand=additionalReceiveXScrollBar.set, yscrollcommand=additionalReceiveYScrollBar.set)

        additionalSendFrame.pack(side=LEFT, anchor='nw', fill=X, expand=True)
        additionalSendLabel.pack(side=TOP, anchor='nw')
        additionalSendTextFrame.pack(side=TOP, anchor='nw', fill=X, expand=True)
        additionalSendYScrollBar.pack(side=RIGHT, anchor='e', fill=Y)
        additionalSendTextEntry.pack(side=TOP, anchor='nw', fill=X, expand=True)
        additionalSendXScrollBar.pack(side=BOTTOM, anchor='s', fill=X, expand=True)

        additionalReceiveFrame.pack(side=LEFT, anchor='nw', fill=X, expand=True)
        additionalReceiveLabel.pack(side=TOP, anchor='nw')
        additionalReceiveTextFrame.pack(side=TOP, anchor='nw', fill=X, expand=True)
        additionalReceiveYScrollBar.pack(side=RIGHT, anchor='e', fill=Y)
        additionalReceiveTextEntry.pack(side=TOP, anchor='nw', fill=X, expand=True)
        additionalReceiveXScrollBar.pack(side=BOTTOM, anchor='s', fill=X, expand=True)

        self.update()

        networkModelingAlgorithmFrame = ScrollableFrame(self, bg=bg, padx=padx, pady=pady)
        networkModelingAlgorithmFrame.package(side=TOP, anchor='n', fill=BOTH, expand=True)

        self.update()

        algorithmDictionary = {}
        algorithmFunctions = PluginAPI.getPlugins("networking")

        for algorithm in algorithmFunctions:
            algorithmDictionary.update({
                algorithmFunctions[algorithm]['import'].identity() :
                    {"algorithm" : algorithmFunctions[algorithm]['import'].execute,
                    "url" : algorithmFunctions[algorithm]['import'].url(),
                    "return" : algorithmFunctions[algorithm]['import'].identity()
            }})

        networkAlgorithmInputDict = {}
        networkAlgorithmButtonList = []

        networkAlgorithmSeclection = StringVar(master=self,value=algorithmDictionary[list(algorithmDictionary)[0]]["return"])
        for dictionary in algorithmDictionary:

            networkAlgorithmInputDict.update({
                algorithmDictionary[dictionary]['return']:{
                    "frame" : ToggleFrame(networkModelingAlgorithmFrame.getFrame(), bg=bg, padx=1,pady=1),
                }
            })
            networkAlgorithmInputDict[algorithmDictionary[dictionary]['return']].update({
                "button" : Radiobutton(networkAlgorithmInputDict[algorithmDictionary[dictionary]['return']]['frame'], text=algorithmDictionary[dictionary]['return'], variable=networkAlgorithmSeclection, value=algorithmDictionary[dictionary]['return'], indicatoron=0, bg=btn_bg, fg=btn_fg, activebackground=btn_alt_bg, activeforeground=btn_alt_fg, font=NORMAL_FONT),
                "stringVar" : StringVar(master=networkAlgorithmInputDict[algorithmDictionary[dictionary]['return']]['frame'], value=algorithmDictionary[dictionary]['url'])
            })
            networkAlgorithmInputDict[algorithmDictionary[dictionary]['return']].update({
                "entry": Entry(networkAlgorithmInputDict[algorithmDictionary[dictionary]['return']]['frame'], textvariable=networkAlgorithmInputDict[algorithmDictionary[dictionary]['return']]["stringVar"], bg=ent_bg, fg=ent_fg, insertbackground=ent_fg)
            })

        for actionValues in networkAlgorithmInputDict.values():
            networkAlgorithmButtonList.append(actionValues['button'])
            actionValues['button'].config(command= lambda :selectRadioButton_NetworkAlgorithms(actionValues['button'], networkAlgorithmButtonList, actionValues['stringVar'].get()))
            actionValues['frame'].pack(fill=X, side=TOP, anchor='n', expand=True)
            actionValues['button'].pack(side=LEFT, anchor='w')
            actionValues['entry'].pack(side=LEFT, anchor='e', fill=X, expand=True)

        networkModelingAlgorithmFrame.package(side=TOP, anchor='n', fill=BOTH, expand=True)
        self.update()

def selectRadioButton_PerfAlgorithms(selectedRB: Radiobutton, allRB: List[Radiobutton], extraInfo = None) -> None:
    selectedRB.config(fg=colorDict["performance_btn_fg"], bg=colorDict["performance_btn_bg"])
    for rButton in allRB:
        if rButton != selectedRB:
            rButton.config(bg=colorDict["performance_btn_alt_bg"], fg=colorDict["performance_btn_alt_fg"])
    if extraInfo != None:
        print(extraInfo)

class PerformanceFrame(ToggleFrame):

    pixel = None
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        NORMAL_FONT = font.Font(size=11)
        self.pixel = PhotoImage()

        self.pack()
        self.update()

        padx = colorDict["performance_padx"]
        pady = colorDict["performance_pady"]
        bg = colorDict["performance_bg"]
        txt_bg = colorDict["performance_txt_bg"]
        txt_fg = colorDict["performance_txt_fg"]
        ent_bg = colorDict["performance_ent_bg"]
        ent_fg = colorDict["performance_ent_fg"]
        btn_bg = colorDict["performance_btn_bg"]
        btn_fg = colorDict["performance_btn_fg"]
        btn_alt_bg = colorDict["performance_btn_alt_bg"]
        btn_alt_fg = colorDict["performance_btn_alt_fg"]

        urlFrame = ToggleFrame(self, padx=padx, pady=pady, height=int(self.winfo_height()*.2), width=self.winfo_width(), relief=SUNKEN, bg=bg)
        modelingFrame = ToggleFrame(self, padx=padx, pady=pady, width=self.winfo_width(), relief=SUNKEN, bg=bg)

        urlFrame.pack(fill=BOTH, side=TOP, anchor='n')
        modelingFrame.pack(side=TOP, anchor='n',fill=X)
        self.update()

        inputTextFrame = ToggleFrame(urlFrame, padx=padx, pady=pady, height=urlFrame.winfo_height(), width=int(urlFrame.winfo_width()*.75), relief=SUNKEN, bg=bg)

        loadPerModels = Button(urlFrame, compound='c',image=self.pixel,text="Load", padx=padx, pady=pady, bg=bg, fg=btn_fg, activebackground=btn_alt_bg, activeforeground=btn_alt_fg, command=lambda: print("load models"))

        loadPerModels.pack(anchor="e", side=RIGHT)
        inputTextFrame.pack(anchor='nw', fill=X, side=TOP, expand=True)
        self.update()

        loadModelURLInput = Text(inputTextFrame,height=1, width=50, font=NORMAL_FONT, bg=ent_bg, fg=ent_fg, insertbackground=ent_fg, wrap=NONE)
        modelURLScrollBar = Scrollbar(loadModelURLInput.master, orient="horizontal", command=loadModelURLInput.xview)
        loadModelURLInput.config(xscrollcommand=modelURLScrollBar.set)

        autoLoadLabel = Label(inputTextFrame, text="Load models automatically using:",font=NORMAL_FONT,padx=padx, pady=pady, bg=txt_bg, fg=txt_fg)

        autoLoadLabel.pack(anchor='w', side=TOP, expand=False)
        loadModelURLInput.pack(anchor='w', side=TOP,fill=X, expand=True)
        modelURLScrollBar.pack(side=BOTTOM, fill=X)

        self.update()

        directModelFrame = ToggleFrame(modelingFrame, padx=padx, pady=pady, height=modelingFrame.winfo_height(), width=int(modelingFrame.winfo_width()*.5), relief=SUNKEN, bg=bg)
        inverseModelFrame = ToggleFrame(modelingFrame, padx=padx, pady=pady, height=modelingFrame.winfo_height(), width=int(modelingFrame.winfo_width()*.5), relief=SUNKEN, bg=bg)

        directModelFrame.pack(anchor='nw', side=LEFT)
        inverseModelFrame.pack(anchor='ne', side=RIGHT)

        self.update()

        directPerfLabel = Label(directModelFrame, text="Direct Performance Model",font=NORMAL_FONT, bg=txt_bg, fg=txt_fg)
        directSendCoresFrame = ToggleFrame(directModelFrame, width=directModelFrame.winfo_width(), relief=SUNKEN, bg=bg)
        directRecievePerformanceFrame= ToggleFrame(directModelFrame, width=directModelFrame.winfo_width(), relief=SUNKEN, bg=bg)

        directPerfLabel.pack(side=TOP, anchor='w')
        directSendCoresFrame.pack(side=TOP, anchor='w', fill=X)
        directRecievePerformanceFrame.pack(side=TOP, anchor='w', fill=X)
        self.update()

        directSendCoreLabel = Label(directSendCoresFrame, text="Send number of cores in:",font=NORMAL_FONT, bg=txt_bg, fg=txt_fg)
        directRecievePerformaceLabel = Label(directRecievePerformanceFrame, text="Receive performance in:",font=NORMAL_FONT, bg=txt_bg, fg=txt_fg)

        directSendCoreVar = StringVar(value="cores")
        directReceivePerformanceVar = StringVar(value="performance")
        directSendCoresEntry = Entry(directSendCoresFrame, bg=ent_bg, fg=ent_fg, insertbackground=ent_fg, font=NORMAL_FONT, textvariable=directSendCoreVar, justify=RIGHT)
        directRecievePerformanceEntry = Entry(directRecievePerformanceFrame, bg=ent_bg, fg=ent_fg, insertbackground=ent_fg, font=NORMAL_FONT, textvariable=directReceivePerformanceVar, justify=RIGHT)

        directSendCoreLabel.pack(side=LEFT, anchor='w')
        directRecievePerformaceLabel.pack(side=LEFT, anchor='w')
        directSendCoresEntry.pack(side=RIGHT, anchor='e')
        directRecievePerformanceEntry.pack(side=RIGHT, anchor='e')
        self.update()

        inversePerfLabel = Label(inverseModelFrame, text="Direct Performance Model",font=NORMAL_FONT, bg=txt_bg, fg=txt_fg)
        inverseSendPerformanceFrame = ToggleFrame(inverseModelFrame, width=inverseModelFrame.winfo_width(), relief=SUNKEN, bg=bg)
        inverseReceiveCoresFrame= ToggleFrame(inverseModelFrame, width=inverseModelFrame.winfo_width(), relief=SUNKEN, bg=bg)

        inversePerfLabel.pack(side=TOP, anchor='w')
        inverseSendPerformanceFrame.pack(side=TOP, anchor='w', fill=X)
        inverseReceiveCoresFrame.pack(side=TOP, anchor='w', fill=X)
        self.update()

        inverseSendPerformanceLabel = Label(inverseSendPerformanceFrame, text="Send projected performance in:",font=NORMAL_FONT, bg=txt_bg, fg=txt_fg)
        inverseReceiveCoresLabel = Label(inverseReceiveCoresFrame, text="Receive the number of cores in:",font=NORMAL_FONT, bg=txt_bg, fg=txt_fg)

        inverseSendPerformanceVar = StringVar(value="cores")
        inverseReceiveCoresVar = StringVar(value="performance")
        inverseSendPerformanceEntry = Entry(inverseSendPerformanceFrame, bg=ent_bg, fg=ent_fg, insertbackground=ent_fg, font=NORMAL_FONT, textvariable=inverseSendPerformanceVar, justify=RIGHT)
        inverseReceiveCoresEntry = Entry(inverseReceiveCoresFrame, bg=ent_bg, fg=ent_fg, insertbackground=ent_fg, font=NORMAL_FONT, textvariable=inverseReceiveCoresVar, justify=RIGHT)

        inverseSendPerformanceLabel.pack(side=LEFT, anchor='w')
        inverseReceiveCoresLabel.pack(side=LEFT, anchor='w')
        inverseSendPerformanceEntry.pack(side=RIGHT, anchor='e')
        inverseReceiveCoresEntry.pack(side=RIGHT, anchor='e')

        self.update()

        otherParametersIOFrame = ToggleFrame(self, padx=padx, pady=pady, width=self.winfo_width(), relief=SUNKEN, bg=bg)
        otherParametersIOFrame.pack(anchor='n', side=TOP, fill=X, expand=True)
        self.update()

        alsoSendParametersFrame = ToggleFrame(otherParametersIOFrame, padx=padx, width=int(otherParametersIOFrame.winfo_width()*.5), pady=pady, relief=SUNKEN, bg=bg)
        alsoRecieveCharacteristicsFrame = ToggleFrame(otherParametersIOFrame, padx=padx, width=int(otherParametersIOFrame.winfo_width()*.5), pady=pady, relief=SUNKEN, bg=bg)

        alsoSendParametersFrame.pack(side=LEFT , anchor='nw', fill=X, expand=True)
        alsoRecieveCharacteristicsFrame.pack(side=RIGHT , anchor='ne', fill=X, expand=True)

        self.update()

        alsoSendParametersLabel = Label(alsoSendParametersFrame, text="Also send the following parameters:", padx=padx, pady=pady, bg=txt_bg, fg=txt_fg, font=NORMAL_FONT, justify=LEFT)
        alsoSendEntryFrame = ToggleFrame(alsoSendParametersFrame, width=alsoSendParametersFrame.winfo_width())
        alsoSendParametersEntry = Text(alsoSendEntryFrame, height=5, width=alsoSendEntryFrame.winfo_width(), font=NORMAL_FONT, bg=ent_bg, fg=ent_fg, insertbackground=ent_fg, wrap=NONE)
        alsoSendParametersXScroll = Scrollbar(alsoSendParametersEntry.master, orient="horizontal", command=alsoSendParametersEntry.xview)
        alsoSendParametersYScroll = Scrollbar(alsoSendParametersEntry.master, orient="vertical", command=alsoSendParametersEntry.yview)
        alsoSendParametersEntry.config(xscrollcommand=alsoSendParametersXScroll.set, yscrollcommand=alsoSendParametersYScroll.set)

        self.update()

        alsoRecieveCharacteristicsLabel = Label(alsoRecieveCharacteristicsFrame, text="Also receive the following characteristics:", padx=padx, pady=pady, bg=txt_bg, fg=txt_fg, font=NORMAL_FONT, justify=LEFT)
        alsoRecieveEntryFrame = ToggleFrame(alsoRecieveCharacteristicsFrame, width=alsoRecieveCharacteristicsFrame.winfo_width())
        alsoRecieveCharacteristicsEntry = Text(alsoRecieveEntryFrame, height=5, width=alsoRecieveEntryFrame.winfo_width()//11, font=NORMAL_FONT, bg=ent_bg, fg=ent_fg, insertbackground=ent_fg, wrap=NONE)
        alsoRecieveCharacteristicsXScroll = Scrollbar(alsoRecieveCharacteristicsEntry.master, orient="horizontal", command=alsoRecieveCharacteristicsEntry.xview)
        alsoRecieveCharacteristicsYScroll = Scrollbar(alsoRecieveCharacteristicsEntry.master, orient="vertical", command=alsoRecieveCharacteristicsEntry.yview)
        alsoRecieveCharacteristicsEntry.config(xscrollcommand=alsoRecieveCharacteristicsXScroll.set, yscrollcommand=alsoRecieveCharacteristicsYScroll.set)

        self.update()

        alsoSendParametersLabel.pack(anchor='nw', side=TOP ,fill=X, expand=True)
        alsoSendEntryFrame.pack(anchor='w', side=BOTTOM, fill=X, expand=True)
        alsoSendParametersXScroll.pack(anchor='s', side=BOTTOM ,fill=X, expand=True)
        alsoSendParametersEntry.pack(anchor='nw',side=LEFT, fill=X, expand=True)
        alsoSendParametersYScroll.pack(anchor='ne', side=RIGHT ,fill=Y)

        alsoRecieveCharacteristicsLabel.pack(anchor='nw', side=TOP ,fill=X)
        alsoRecieveEntryFrame.pack(anchor='w', side=BOTTOM, fill=X, expand=True)
        alsoRecieveCharacteristicsXScroll.pack(anchor='s', side=BOTTOM ,fill=X)
        alsoRecieveCharacteristicsEntry.pack(anchor='nw',side=LEFT,fill=X, expand = True)
        alsoRecieveCharacteristicsYScroll.pack(anchor='ne', side=RIGHT ,fill=Y)

        self.update()

        otherParametersIOFrame.pack(anchor='n', side=TOP, fill=X)

        perfAlgorithmFrame = ToggleFrame(self, padx=padx, pady=pady, width=self.winfo_width(), bg=bg, relief=SUNKEN)
        perfAlgorithmFrame.pack(fill=X, anchor='n', side=TOP, expand=True)
        self.update()

        algorithmDictionary = {}

        algorithmFunctions = PluginAPI.getPlugins("performance")

        for algorithm in algorithmFunctions:
            algorithmDictionary.update({
                algorithmFunctions[algorithm]['import'].identity() :
                    {"algorithm" : algorithmFunctions[algorithm]['import'].execute,
                    "url" : algorithmFunctions[algorithm]['import'].url(),
                    "return" : algorithmFunctions[algorithm]['import'].identity()
            }})

        perfAlgorithmInputDict = {}
        perfAlgorithmButtonList = []

        perfAlgorithmSeclection = StringVar(master=self,value=algorithmDictionary[list(algorithmDictionary)[0]]["return"])
        for dictionary in algorithmDictionary:

            perfAlgorithmInputDict.update({
                algorithmDictionary[dictionary]['return']:{
                    "frame" : ToggleFrame(perfAlgorithmFrame, bg=bg, padx=1,pady=1),
                }
            })
            perfAlgorithmInputDict[algorithmDictionary[dictionary]['return']].update({
                "button" : Radiobutton(perfAlgorithmInputDict[algorithmDictionary[dictionary]['return']]['frame'], text=algorithmDictionary[dictionary]['return'], variable=perfAlgorithmSeclection, value=algorithmDictionary[dictionary]['return'], indicatoron=0, bg=btn_bg, fg=btn_fg, activebackground=btn_alt_bg, activeforeground=btn_alt_fg, font=NORMAL_FONT),
                "stringVar" : StringVar(master=perfAlgorithmInputDict[algorithmDictionary[dictionary]['return']]['frame'], value=algorithmDictionary[dictionary]['url'])
            })
            perfAlgorithmInputDict[algorithmDictionary[dictionary]['return']].update({
                "entry": Entry(perfAlgorithmInputDict[algorithmDictionary[dictionary]['return']]['frame'], textvariable=perfAlgorithmInputDict[algorithmDictionary[dictionary]['return']]["stringVar"], bg=ent_bg, fg=ent_fg)
            })

        for actionValues in perfAlgorithmInputDict.values():
            perfAlgorithmButtonList.append(actionValues['button'])
            actionValues['button'].config(command= lambda :selectRadioButton_PerfAlgorithms(actionValues['button'], perfAlgorithmButtonList, actionValues['stringVar'].get()))
            actionValues['frame'].pack(fill=X, side=TOP, anchor='n', expand=True)
            actionValues['button'].pack(side=LEFT, anchor='w')
            actionValues['entry'].pack(side=LEFT, anchor='e', fill=X, expand=True)

        perfAlgorithmFrame.pack(fill=BOTH, anchor='n', side=TOP)
        self.update()

class DesignFrame(ToggleFrame):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        NORMAL_FONT = font.Font(size=11)
        self.pixel = PhotoImage()
        self.pack()
        self.update()

        padx = colorDict["design_padx"]
        pady = colorDict["design_pady"]
        bg = colorDict["design_bg"]
        txt_bg = colorDict["design_txt_bg"]
        txt_fg = colorDict["design_txt_fg"]
        ent_bg = colorDict["design_ent_bg"]
        ent_fg = colorDict["design_ent_fg"]
        btn_bg = colorDict["design_btn_bg"]
        btn_fg = colorDict["design_btn_fg"]
        btn_alt_bg = colorDict["design_btn_alt_bg"]
        btn_alt_fg = colorDict["design_btn_alt_fg"]

        initiationFrame = ToggleFrame(self, padx=padx, pady=pady, bg=bg)

        startButton = Button(initiationFrame, bg=btn_bg, fg=btn_fg, activebackground=btn_alt_bg, activeforeground=btn_alt_fg, text="Start Design Process", padx=2, pady=2, font=NORMAL_FONT, command= lambda : print("start design process button pressed"))

        progressFrame = ToggleFrame(initiationFrame, bg=bg, padx=2, pady=2)

        cancelButton = Button(initiationFrame, bg=btn_bg, fg=btn_fg, activebackground=btn_alt_bg, activeforeground=btn_alt_fg, text="Cancel", padx=2, pady=2, font=NORMAL_FONT, command= lambda : print("cancel button pressed"))

        initiationFrame.pack(side=TOP, anchor='n', fill=X)

        startButton.pack(side=LEFT, anchor='w')
        cancelButton.pack(side=RIGHT, anchor='e')
        progressFrame.pack(side=LEFT, anchor='w', expand=True, fill=BOTH)

        self.update()

        constraintsFrame = ToggleFrame(self, padx=padx, pady=pady, bg=bg)

        constraintsFrame.pack(side=TOP, anchor='n', fill=X)
        self.update()

        constraintsLabel = Label(constraintsFrame, padx=2, pady=2, bg=txt_bg, fg=txt_fg, font=NORMAL_FONT, text="Global design constraints")

        constraintsInputFrame = ToggleFrame(constraintsFrame, bg=bg)
        constraintsInputTextEntry = Text(constraintsInputFrame, bg=ent_bg, fg=ent_fg, insertbackground=ent_fg, height=4, font=NORMAL_FONT)
        constraintsInputTextEntryXScroll = Scrollbar(constraintsInputTextEntry.master, orient="horizontal", command=constraintsInputTextEntry.xview)
        constraintsInputTextEntryYScroll = Scrollbar(constraintsInputTextEntry.master, orient="vertical", command=constraintsInputTextEntry.yview)
        constraintsInputTextEntry.config(xscrollcommand=constraintsInputTextEntryXScroll.set, yscrollcommand=constraintsInputTextEntryYScroll.set)

        constraintObjectiveFrame = ToggleFrame(constraintsFrame, bg=bg, width=int(constraintsFrame.winfo_width()*.5))
        constraintObjectiveFrame.pack(side=TOP, anchor='w')

        constraintObjectiveVar = StringVar()
        constraintObjectiveNames = ["Test 1", "test 2"]
        constraintObjectiveLabel = Label(constraintObjectiveFrame, padx=2, pady=2, bg=txt_bg, fg=txt_fg, font=NORMAL_FONT, text="Objective Function")
        #TODO API call to get the difference performace algorithms and put them into a list

        #Taken form NodesFrame
        """
        heuristicFunctions = PluginAPI.getPlugins("heuristics")
        for heuristicFile in heuristicFunctions:
            constraintObjectiveNames.append(heuristicFunctions[heuristicFile]["import"].identity())
        """
        constraintObjectiveVar = StringVar(self,value=constraintObjectiveNames[0])
        designAlgorithmOptions = OptionMenu(constraintObjectiveFrame, constraintObjectiveVar, *constraintObjectiveNames)

        designAlgorithmOptions.config(bg=btn_bg, fg=btn_fg, activebackground=btn_alt_bg, activeforeground=btn_alt_fg)
        designAlgorithmOptions["menu"].config(bg=btn_bg, fg=btn_fg)

        constraintsLabel.pack(side=TOP, anchor='nw')
        constraintsInputFrame.pack(side=BOTTOM, anchor='nw', expand=True, fill=X)

        constraintsInputTextEntryYScroll.pack(side=RIGHT, anchor='e', fill=Y)
        constraintsInputTextEntry.pack(side=TOP, anchor='sw', fill=X)
        constraintsInputTextEntryXScroll.pack(side=BOTTOM, anchor='s', fill=X)
        constraintObjectiveLabel.pack(side=LEFT, anchor='nw')
        designAlgorithmOptions.pack(side=RIGHT, anchor='ne')

        constraintsFrame.pack(side=TOP, anchor='n', fill=X)

        configOutputFrame = ToggleFrame(self, padx=padx, pady=pady, bg=bg)

        currentConfigLabel = Label(configOutputFrame, bg=txt_bg, fg=txt_fg, text="Current configuration:", font=NORMAL_FONT)
        currentConfigLabel.pack(side=TOP, anchor='nw')

        configIndexBox = IndexCounterEntry(1, 10, 1, 16, 1, ent_bg, ent_fg, btn_bg, btn_fg, btn_alt_bg, btn_alt_fg, bg=configOutputFrame['bg'], padx=3, pady=3, master=configOutputFrame)
        configIndexBox.pack(side=TOP, anchor='nw')
        self.update()

        configOutputFrame.pack(side=TOP, anchor='nw', fill=BOTH)

        configOutRawDataFrame = ToggleFrame(configOutputFrame, padx=10, bg=configOutputFrame['bg'])
        configOutRawDataText = Text(configOutRawDataFrame, bg=ent_bg, fg=ent_fg, insertbackground=ent_fg, font=NORMAL_FONT, width=configOutRawDataFrame.master.winfo_width()//11, wrap=NONE)
        configOutRawDataTextXScroll = Scrollbar(configOutRawDataText.master, orient="horizontal", command=configOutRawDataText.xview)
        configOutRawDataTextYScroll = Scrollbar(configOutRawDataText.master, orient="vertical", command=configOutRawDataText.yview)
        configOutRawDataText.config(xscrollcommand=configOutRawDataTextXScroll.set, yscrollcommand=configOutRawDataTextYScroll.set)

        configOutHumanDataFrame = ToggleFrame(configOutputFrame, padx=3)
        configOutHumanDataText = Text(configOutHumanDataFrame, bg=ent_bg, fg=ent_fg, insertbackground=ent_fg, font=NORMAL_FONT, width=configOutHumanDataFrame.master.winfo_width()//11, wrap=NONE)
        configOutHumanDataTextXScroll = Scrollbar(configOutHumanDataText.master, orient="horizontal", command=configOutHumanDataText.xview)
        configOutHumanDataTextYScroll = Scrollbar(configOutHumanDataText.master, orient="vertical", command=configOutHumanDataText.yview)
        configOutHumanDataText.config(xscrollcommand=configOutHumanDataTextXScroll.set, yscrollcommand=configOutHumanDataTextYScroll.set)


        configOutRawDataFrame.pack(side=LEFT, anchor='w', fill=BOTH, expand=True)
        configOutRawDataTextYScroll.pack(side=RIGHT, anchor='e', fill=Y)
        configOutRawDataText.pack(side=TOP, anchor='n', fill=BOTH)
        configOutRawDataTextXScroll.pack(side=BOTTOM, anchor='s', fill=X)

        configOutHumanDataFrame.pack(side=RIGHT, anchor='e', fill=BOTH, expand=True)
        configOutHumanDataTextYScroll.pack(side=RIGHT, anchor='e', fill=Y)
        configOutHumanDataText.pack(side=TOP, anchor='n', fill=BOTH)
        configOutHumanDataTextXScroll.pack(side=BOTTOM, anchor='s', fill=X)

class AboutFrame(ToggleFrame):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

def wantedFrame(allFrames: List[ToggleFrame], showFrame: ToggleFrame) -> None:
    for frame in allFrames:
        frame.disable()
        frame.pack_forget()
        frame.update()
    showFrame.enable()
    print(showFrame.__class__.__name__)
    showFrame.pack(fill=BOTH, expand=True)
    showFrame.update()

def selectRadioButton_Actions(selectedRB: Radiobutton, allRB: List[Radiobutton],dictionary : dict,allFramesAffected : List[ToggleFrame],commonVar : Union[StringVar, IntVar]) -> None:
    selectedRB.config(fg=colorDict["btn_bg"], bg=colorDict["btn_fg"])
    for rButton in allRB:
        if rButton != selectedRB:
            rButton.config(fg=colorDict["btn_alt_bg"], bg=colorDict["btn_alt_fg"])
    selectedActionFrame = dictionary[commonVar.get()]["frame"]
    wantedFrame(allFramesAffected, selectedActionFrame)

def themeAPICall(requested: str = None) -> None:
    if requested == None:
        return None
    themeDictionary = {}
    dictionaryFiles = PluginAPI.getPlugins("themes")

    for dictionary in dictionaryFiles:
        try:
            themeDictionary.update({dictionaryFiles[dictionary]['import'].identity() : dictionaryFiles[dictionary]['import'].ColorDictionary()})
        except Exception as e:
            try:
                print(e)
                print("There as an error importing {}, it will not be callable,".format(dictionaryFiles[dictionary]['import'].identity()))
            except:
                print(e)
                print("There was an issue with {}, no identifier [identity()] provided.".format(dictionary))

    try:
        selectedThemeDictionary = themeDictionary[requested]
        #print(selectedThemeDictionary)
    except:
        print(e)
        print("Selected theme, {}, does not exist, reverting to default ".format(requested))

    for item in selectedThemeDictionary:
        #print(item )
        colorDict[item] = selectedThemeDictionary[item]

def ErrorPopUp(errorReadout : Exception, sizeX : int = 250, sizeY : int = 250) -> None:

    mainbg = "#2A2A2A"
    mainfg = "#B6B6B6"
    buttonbg = "#181818"
    nFont=font.Font(size=11)

    win = Toplevel()
    win.config(bg=mainbg)
    win.wm_title("An Error Has Occured")
    win.geometry("{}x{}".format(sizeX,sizeY))

    basicErrorOut = Label(win, text=errorReadout, bg=mainbg, fg=mainfg, font=nFont)
    basicErrorOut.pack(side=TOP, anchor='n', fill=X)

    detailedFrame = Frame(win, bg=mainbg)

    errorDetailed = Text(detailedFrame, font=nFont, wrap=NONE, bg=buttonbg, fg=mainfg, insertbackground=mainfg)
    errorDetailedYScrollBar = Scrollbar(errorDetailed.master, orient="vertical", command=errorDetailed.yview)
    errorDetailedXScrollBar = Scrollbar(errorDetailed.master, orient="horizontal", command=errorDetailed.xview)

    errorDetailed.config(yscrollcommand=errorDetailedYScrollBar.set, xscrollcommand=errorDetailedXScrollBar.set)

    for error in traceback.format_exc():
        errorDetailed.insert(END, error)
    #errorDetailed.insert(END, traceback.extract_tb(errorReadout.__traceback__))

    buttonFrame = Frame(win, bg=mainbg)

    closeButton = Button(buttonFrame, text="    Understood    ", command=win.destroy, bg=buttonbg, fg=mainfg, font=nFont, activebackground=mainfg, activeforeground=buttonbg)
    moreInfoButton = Button(buttonFrame, text="    More info    ", command=lambda : detailedFrame.pack(side=BOTTOM, anchor='w', fill=BOTH), bg=buttonbg, fg=mainfg, font=nFont, activebackground=mainfg, activeforeground=buttonbg)

    buttonFrame.pack(side=TOP, anchor='n')
    closeButton.pack(side=LEFT, anchor='nw')
    moreInfoButton.pack(side=RIGHT, anchor='ne')

    errorDetailedYScrollBar.pack(side=RIGHT, fill=Y, anchor='e')
    errorDetailedXScrollBar.pack(side=BOTTOM, fill=X, anchor='s')
    errorDetailed.pack(side=TOP, fill=BOTH, expand=True, anchor="nw")


def GenericPopUp(title : str, text : str, moreInfo : str = "", sizeX : int = 250, sizeY : int = 250) -> None:
    mainbg = "#2A2A2A"
    mainfg = "#B6B6B6"
    buttonbg = "#181818"
    nFont=font.Font(size=11)

    win = Toplevel()
    win.config(bg=mainbg)
    win.wm_title(title)
    win.geometry("{}x{}".format(sizeX,sizeY))

    basicInfoOut = Label(win, text=text, bg=mainbg, fg=mainfg, font=nFont)
    basicInfoOut.pack(side=TOP, anchor='n', fill=X)

    detailedInfoOut = Label(win, text=moreInfo, font=nFont)

    buttonFrame = Frame(win, bg=mainbg)

    closeButton = Button(buttonFrame, text="    Understood    ", command=win.destroy, bg=buttonbg, fg=mainfg, font=nFont, activebackground=mainfg, activeforeground=buttonbg)
    moreInfoButton = Button(buttonFrame, text="    More info    ", command=lambda : detailedInfoOut.pack(side=BOTTOM, anchor='w', fill=BOTH), bg=buttonbg, fg=mainfg, state=("disabled" if moreInfo == "" else "enabled"), font=nFont, activebackground=mainfg, activeforeground=buttonbg)

    buttonFrame.pack(side=TOP, anchor='n')
    closeButton.pack(side=LEFT, anchor='nw')
    moreInfoButton.pack(side=RIGHT, anchor='ne')


class ClusterDesign(Tk):#predefine the globals here
    pixel = None
    framesDictionary = dict()

    def openFile(self, fileType: List[Tuple[str,str]]):
        fileOpenDialog = filedialog.Open(self, filetypes=fileType)

        fileLoaded = fileOpenDialog.show()

        return fileLoaded

    @classmethod
    def loadAbort(cls):
        GenericPopUp("No File Selected", "Loading of file was aborted")

    def loadJSONButtonAction(self) -> None: #potential accept file data type, nned to find out what the built-in fill opener does
        fileName = self.openFile([("JSON File", "*.json"), ('All Files', '*')])
        if fileName != "":
            try:
                if not fileName.endswith('json'):
                    raise ValueError("Selected file is not an json file")
                components = parseJsonTree(fileName)
                CONFIGURATIONS = list()
                # for key, value in components.items():
                #     print(key, value)

                #cpus, memory, node, blade, chassis
                for cpu in components["cpus"][0]:
                    for chassis in components["chassis"][0]:
                        for node in components["node"][0]:
                            for blade in components["blade"][0]:
                                configChassis : Chassis.Chassis = deepcopy(chassis)
                                configNode : RackMount.RackMount = deepcopy(node)
                                configBlade : Blade.Blade = deepcopy(blade)
                                configBlade.setCPU(cpu)

                                

                                for memory in components["memory"][0]:
                                    configBlade.fastestMemory(memory)

                                configNode.fillBlades(configBlade)

                                #print(configChassis, configNode, configBlade)

                                configNode.calculateHeat()

                                configChassis.addItem(configNode)

                                if(not configNode.isFilled()):
                                    break
                                
                                if(configChassis.isFilled()):
                                    CONFIGURATIONS.append(configChassis)

                                    # print(configChassis)

                for config in CONFIGURATIONS:
                    print(config)
                    
                        
                            
                                    
                
                    
            except Exception as e:
                ErrorPopUp(e)
        else:
            self.loadAbort()

        print(ClusterDesign.framesDictionary)
        ClusterDesign.framesDictionary["nodes"]["frame"].updateOutput()
        print("end Pause")

    def loadXLMButtonAction(self) -> None: #potential accept file data type, nned to find out what the built-in fill opener does
        fileName = self.openFile([("XML File", "*.xml"), ('All Files', '*')])
        if fileName != "":
            try:
                if not fileName.endswith('.xml'):
                    raise ValueError("Selected file is not an XML file")
                with open(fileName, 'r') as xmlData:
                    #print(xmlData.read())
                    xmlParser.parseNodeXML(xmlData.read())
            except Exception as e:
                ErrorPopUp(e)
        else:
            self.loadAbort()



    def loadCSVButtonAction(self, fileType: List[Tuple[str,str]]) -> None: #potential accept file data type, nned to find out what the built-in fill opener does
        print("load csv")

    def writeCSVButtonAction(self, fileType: List[Tuple[str,str]]) -> None: #potential accept file data type, nned to find out what the built-in fill opener does
        print("write csv")

    def undoButtonAction() -> None:
        print("undo")

    def __init__(self, *args, **kwargs):
        Tk.__init__(self, *args, **kwargs)


        #all TypeVars
        NORMAL_FONT = font.Font(size=11)


        self.title(TITLE)
        #print(SCREEN_RESOLUTION)
        self.geometry(str(int(SCREEN_RESOLUTION[0]*.5))+'x'+str(int(SCREEN_RESOLUTION[1]*.5)))
        self.config(bg=colorDict["bg"])
        self.update()

        self.pixel = PhotoImage()

        mainFrame = Frame(self, padx=colorDict["padx"], pady=colorDict["pady"], width=self.winfo_width(), height=self.winfo_height(), bg=colorDict["bg"])

        mainFrame.pack(fill="both", expand=True)
        self.update()

        #area that holds the file IO
        fileOptionsFrame = Frame(mainFrame, padx=colorDict["fileio_padx"], pady=colorDict["fileio_pady"], width=mainFrame.winfo_width(), height=int(mainFrame.winfo_height()*.075), relief=SUNKEN, bg=colorDict["fileio_bg"])


        importJSONdbButton = Button(fileOptionsFrame, text="    Load JSON DB    ", padx=colorDict["fileio_padx"], pady=colorDict["fileio_pady"], bg=colorDict["fileio_btn_bg"], fg=colorDict["fileio_btn_fg"], activebackground=colorDict["fileio_btn_alt_bg"], activeforeground=colorDict["fileio_btn_alt_fg"], command=self.loadJSONButtonAction)
        importXMLdbButton = Button(fileOptionsFrame, text="    Load XML DB (Legacy)    ", padx=colorDict["fileio_padx"], pady=colorDict["fileio_pady"], bg=colorDict["fileio_btn_bg"], fg=colorDict["fileio_btn_fg"], activebackground=colorDict["fileio_btn_alt_bg"], activeforeground=colorDict["fileio_btn_alt_fg"], command=self.loadXLMButtonAction)
        importCSVdbButton = Button(fileOptionsFrame, text="    Load From CSV    ", padx=colorDict["fileio_padx"], pady=colorDict["fileio_pady"], bg=colorDict["fileio_btn_bg"], fg=colorDict["fileio_btn_fg"], activebackground=colorDict["fileio_btn_alt_bg"], activeforeground=colorDict["fileio_btn_alt_fg"])
        exportCSVButton = Button(fileOptionsFrame, text="    Export to CSV    ", padx=colorDict["fileio_padx"], pady=colorDict["fileio_pady"], bg=colorDict["fileio_btn_bg"], fg=colorDict["fileio_btn_fg"], activebackground=colorDict["fileio_btn_alt_bg"], activeforeground=colorDict["fileio_btn_alt_fg"])
        undoButton = Button(fileOptionsFrame, text="    Undo    ", padx=colorDict["fileio_padx"], pady=colorDict["fileio_pady"], bg=colorDict["fileio_btn_bg"], fg=colorDict["fileio_btn_fg"], activebackground=colorDict["fileio_btn_alt_bg"], activeforeground=colorDict["fileio_btn_alt_fg"])



        importJSONdbButton.pack(side=LEFT, anchor='nw')
        importXMLdbButton.pack(side=LEFT, anchor='nw')
        importCSVdbButton.pack(side=LEFT, anchor='nw')
        exportCSVButton.pack(side=LEFT, anchor='nw')
        undoButton.pack(side=LEFT, anchor='nw')

        allActionOptionsFrame = Frame(mainFrame, padx=colorDict["padx"], pady=colorDict["pady"], width=mainFrame.winfo_width(), height=int(mainFrame.winfo_height()*.95), relief=SUNKEN, bg=colorDict["bg"])

        fileOptionsFrame.pack(fill=X)
        allActionOptionsFrame.pack(fill=BOTH,expand=True)
        self.update()

        #top row of options, will allow to swap between diferent parts of the program.
        actionButtonFrame = Frame(allActionOptionsFrame, padx=colorDict["padx"], pady=colorDict["pady"], width=allActionOptionsFrame.winfo_width(), height=int(allActionOptionsFrame.winfo_height()*.085), relief=RAISED, bg=colorDict["bg"])

        actionButtonFrame.pack()
        self.update()

        #creation of all of the different frames, they are all designed similarly as they are mode to fill the same space on the screen.
        nodesFrame = NodesFrame(allActionOptionsFrame, padx=colorDict["padx"], pady=colorDict["pady"], width=allActionOptionsFrame.winfo_width(), height=int(allActionOptionsFrame.winfo_height()*.915), relief=SUNKEN, bg=colorDict["node_bg"])
        performanceFrame = PerformanceFrame(allActionOptionsFrame, padx=colorDict["padx"], pady=colorDict["pady"], width=allActionOptionsFrame.winfo_width(), height=int(allActionOptionsFrame.winfo_height()*.915), relief=SUNKEN, bg=colorDict["performance_bg"])
        networkFrame = NetworkFrame(allActionOptionsFrame, padx=colorDict["padx"], pady=colorDict["pady"], width=allActionOptionsFrame.winfo_width(), height=int(allActionOptionsFrame.winfo_height()*.915), relief=SUNKEN, bg=colorDict["network_bg"])
        upsFrame = UPSFrame(allActionOptionsFrame, padx=colorDict["padx"], pady=colorDict["pady"], width=allActionOptionsFrame.winfo_width(), height=int(allActionOptionsFrame.winfo_height()*.915), relief=SUNKEN, bg=colorDict["ups_bg"])
        designFrame = DesignFrame(allActionOptionsFrame, padx=colorDict["padx"], pady=colorDict["pady"], width=allActionOptionsFrame.winfo_width(), height=int(allActionOptionsFrame.winfo_height()*.915), relief=SUNKEN, bg=colorDict["design_bg"])
        settingsFrame = SettingsFrame(allActionOptionsFrame, padx=colorDict["padx"], pady=colorDict["pady"], width=allActionOptionsFrame.winfo_width(), height=int(allActionOptionsFrame.winfo_height()*.915), relief=SUNKEN, bg=colorDict["settings_bg"])
        aboutFrame = AboutFrame(allActionOptionsFrame, padx=colorDict["padx"], pady=colorDict["pady"], width=allActionOptionsFrame.winfo_width(), height=int(allActionOptionsFrame.winfo_height()*.915), relief=SUNKEN, bg=colorDict["about_bg"])
        allActionFrames=[nodesFrame,performanceFrame,networkFrame,upsFrame,designFrame,settingsFrame,aboutFrame]

        #Dictionary Format:
        '''
        object
            text: name of the object
            return: text for radio buttion, lowercase version of text
            frame: the frame that is used
        '''
        #designed for a type of circular calling

        framesDictionary = {
            "nodes" : {
                "text": "Nodes",
                "return" : "nodes",
                "frame" : nodesFrame
            },
            "performance": {
                "text" : "Performance",
                "return" : "performance",
                "frame" : performanceFrame
            },
            "network": {
                "text" : "Network",
                "return" : "network",
                "frame" : networkFrame
            },
            "ups": {
                "text" : "UPS",
                "return" : "ups",
                "frame" : upsFrame
            },
            "design": {
                "text" : "Design",
                "return" : "design",
                "frame" : designFrame
            },
            "settings": {
                "text" : "Settings",
                "return" : "settings",
                "frame" : settingsFrame
            },
            "about": {
                "text" : "About",
                "return" : "about",
                "frame" : aboutFrame
            }
        }
        actionRadioButtons = []
        actionFrameSelection = StringVar(master=actionButtonFrame,value="nodes")
        for dictionary in framesDictionary:
            #print(framesDictionary[dictionary]['return'])
            rb = Radiobutton(actionButtonFrame, text=framesDictionary[dictionary]['text'], variable=actionFrameSelection, value=framesDictionary[dictionary]['return'], indicatoron=0, bg=colorDict["btn_bg"], fg=colorDict["btn_fg"], font=font.Font(size=14))
            rb.config(command= lambda rButton=rb, listButtons=actionRadioButtons, dictionary=framesDictionary, affected=allActionFrames, commonVar=actionFrameSelection: selectRadioButton_Actions(rButton, listButtons,dictionary, affected, commonVar))
            rb.pack(side=LEFT, fill=X, padx=colorDict["padx"], pady=colorDict["pady"])
            actionRadioButtons.append(rb)

        for frame in allActionFrames:
            frame.pack(fill=BOTH, expand=True)
        wantedFrame(allActionFrames, nodesFrame)
        self.update()

try:
    themeAPICall(argv[1])
except:
    pass
CONFIGURATIONS : List[Type[Chassis.Chassis]] = list()
application = ClusterDesign()
if(hasResource):
    print(str(round(resource.getrusage(resource.RUSAGE_SELF).ru_maxrss/1000,2))+"MB")
print(dir())
application.mainloop()