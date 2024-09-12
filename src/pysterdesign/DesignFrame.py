from tkinter import BOTH, BOTTOM, LEFT, NONE, RIGHT, TOP, X, Y, Button, Label, OptionMenu, PhotoImage, Scrollbar, StringVar, Text, font
from IndexCounterEntry import IndexCounterEntry
from ToggleFrame import ToggleFrame
import Colors

class DesignFrame(ToggleFrame):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        NORMAL_FONT = font.Font(size=11)
        self.pixel = PhotoImage()
        self.pack()
        self.update()

        padx = Colors.ColorDictionary["design_padx"]
        pady = Colors.ColorDictionary["design_pady"]
        bg = Colors.ColorDictionary["design_bg"]
        txt_bg = Colors.ColorDictionary["design_txt_bg"]
        txt_fg = Colors.ColorDictionary["design_txt_fg"]
        ent_bg = Colors.ColorDictionary["design_ent_bg"]
        ent_fg = Colors.ColorDictionary["design_ent_fg"]
        btn_bg = Colors.ColorDictionary["design_btn_bg"]
        btn_fg = Colors.ColorDictionary["design_btn_fg"]
        btn_alt_bg = Colors.ColorDictionary["design_btn_alt_bg"]
        btn_alt_fg = Colors.ColorDictionary["design_btn_alt_fg"]

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
        heuristicFunctions = PluginAPI.getPlugins("heuristics", "execute")
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

        configIndexBox = IndexCounterEntry(minValue=1, maxValue=10,bg=configOutputFrame['bg'], padx=3, pady=3, master=configOutputFrame)
        configIndexBox.pack(side=TOP, anchor='nw')
        self.update()

        configOutputFrame.pack(side=TOP, anchor='nw', fill=BOTH)





        configOutRawDataFrame = ToggleFrame(configOutputFrame, padx=10, bg=configOutputFrame['bg'])
        configOutRawDataText = Text(configOutRawDataFrame, bg=txt_bg, fg=txt_fg, insertbackground=txt_fg, font=NORMAL_FONT, width=configOutRawDataFrame.master.winfo_width()//11, wrap=NONE)
        configOutRawDataTextXScroll = Scrollbar(configOutRawDataText.master, orient="horizontal", command=configOutRawDataText.xview)
        configOutRawDataTextYScroll = Scrollbar(configOutRawDataText.master, orient="vertical", command=configOutRawDataText.yview)
        configOutRawDataText.config(xscrollcommand=configOutRawDataTextXScroll.set, yscrollcommand=configOutRawDataTextYScroll.set)

        configOutHumanDataFrame = ToggleFrame(configOutputFrame, padx=3)
        configOutHumanDataText = Text(configOutHumanDataFrame, bg=txt_bg, fg=txt_fg, font=NORMAL_FONT, width=configOutHumanDataFrame.master.winfo_width()//11, wrap=NONE)
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
