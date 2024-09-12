try:
    from tkinter import BOTH, LEFT, SUNKEN, X, PhotoImage, Radiobutton, StringVar, font, BOTTOM, NONE, RIGHT, TOP, Y, Button, Entry, Label, Scrollbar, Text

except ImportError:
    from Tkinter import BOTH, LEFT, SUNKEN, X, PhotoImage, Radiobutton, StringVar, font, BOTTOM, NONE, RIGHT, TOP, Y, Button, Entry, Label, Scrollbar, Text

from typing import List
from ToggleFrame import ToggleFrame
import Colors
import PluginAPI


def selectRadioButton_PerfAlgorithms(selectedRB: Radiobutton, allRB: List[Radiobutton], extraInfo = None) -> None:
    selectedRB.config(fg=btn_fg, bg=btn_bg)
    for rButton in allRB:
        if rButton != selectedRB:
            rButton.config(bg=Colors.ColorDictionary["performance_btn_alt_bg`"], fg=btn_alt_fg)
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

        padx = Colors.ColorDictionary["performance_padx"]
        pady = Colors.ColorDictionary["performance_pady"]
        bg = Colors.ColorDictionary["performance_bg"]
        txt_bg = Colors.ColorDictionary["performance_txt_bg"]
        txt_fg = Colors.ColorDictionary["performance_txt_fg"]
        ent_bg = Colors.ColorDictionary["performance_ent_bg"]
        ent_fg = Colors.ColorDictionary["performance_ent_fg"]
        btn_bg = Colors.ColorDictionary["performance_btn_bg"]
        btn_fg = Colors.ColorDictionary["performance_btn_fg"]
        btn_alt_bg = Colors.ColorDictionary["performance_btn_alt_bg"]
        btn_alt_fg = Colors.ColorDictionary["performance_btn_alt_fg"]

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

        algorithmFunctions = PluginAPI.getPlugins("performance", "execute")

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
