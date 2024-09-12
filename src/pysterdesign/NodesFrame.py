from tkinter import BOTH, BOTTOM, LEFT, NONE, RIGHT, SUNKEN, TOP, X, Y, Button, Checkbutton, IntVar, Label, OptionMenu, PhotoImage, Scrollbar, StringVar, Text, font
from typing import List, Union

from ToggleFrame import ToggleFrame
import Colors
import PluginAPI

class NodesFrame(ToggleFrame):
    pixel = None

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


        padx = Colors.ColorDictionary["node_padx"]
        pady = Colors.ColorDictionary["node_pady"]
        bg = Colors.ColorDictionary["node_bg"]
        txt_bg = Colors.ColorDictionary["node_txt_bg"]
        txt_fg = Colors.ColorDictionary["node_txt_fg"]
        ent_bg = Colors.ColorDictionary["node_ent_bg"]
        ent_fg = Colors.ColorDictionary["node_ent_fg"]
        btn_bg = Colors.ColorDictionary["node_btn_bg"]
        btn_fg = Colors.ColorDictionary["node_btn_fg"]
        btn_alt_bg = Colors.ColorDictionary["node_btn_alt_bg"]
        btn_alt_fg = Colors.ColorDictionary["node_btn_alt_fg"]

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
        heuristicFunctions = PluginAPI.getPlugins("heuristics", "execute")
        for heuristicFile in heuristicFunctions:
            heuristicNames.append(heuristicFunctions[heuristicFile]["import"].identity())
        self.update()
        simpleHeuristicVar.set(heuristicNames[0])
        heuristicOptions = OptionMenu(applyHeuristicsFrame, simpleHeuristicVar, *heuristicNames,)

        heuristicOptions.config(bg=btn_bg, fg=btn_fg, activebackground=btn_alt_bg, activeforeground=btn_alt_fg)
        heuristicOptions["menu"].config(bg=btn_bg, fg=btn_fg)

        applyHeuristicsButton = Button(applyHeuristicsFrame,text="Apply simple heuristics", command=lambda :self.executeHeuristic(heuristicFunctions,simpleHeuristicVar.get()), fg=btn_fg, bg=btn_fg, activebackground=btn_alt_bg, activeforeground=btn_alt_fg)

        applyHeuristicsButton.pack(anchor="nw" , side=TOP)
        heuristicOptions.pack(anchor="nw" , side=TOP )

        self.update()

        doCleanup = IntVar()
        doCleanup.set(0)
        cleanupDeleteCheckbox = Checkbutton(cleanupFrame,text="Delete Disabled Configurations", variable=doCleanup, bg=btn_bg, fg=btn_fg, activebackground=btn_alt_bg, activeforeground=btn_alt_fg)
        cleanupDeleteButton = Button(cleanupFrame, text="Clean up now", command=lambda :self.cleanupCPUs(doCleanup.get()))

        cleanupDeleteButton.pack(anchor="sw", side=BOTTOM)
        cleanupDeleteCheckbox.pack(anchor="sw", side=BOTTOM)
        self.update()