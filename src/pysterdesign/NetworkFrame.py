
from tkinter import BOTH, BOTTOM, LEFT, NONE, RIGHT, SUNKEN, TOP, X, Y, Button, Entry, Label, PhotoImage, Radiobutton, Scrollbar, StringVar, Text, font
from typing import List
from ToggleFrame import ToggleFrame
from ScrollableFrame import ScrollableFrame
import Colors
import PluginAPI

def selectRadioButton_NetworkAlgorithms(selectedRB: Radiobutton, allRB: List[Radiobutton], extraInfo = None) -> None:
    selectedRB.config(fg=Colors.BUTTON_BG, bg=Colors.BUTTON_FG)
    for rButton in allRB:
        if rButton != selectedRB:
            rButton.config(bg=Colors.BUTTON_BG, fg=Colors.BUTTON_FG)
    if extraInfo != None:
        print(extraInfo)

class NetworkFrame(ToggleFrame):

    pixel=None

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        padx = Colors.ColorDictionary["network_padx"]
        pady = Colors.ColorDictionary["network_pady"]
        bg = Colors.ColorDictionary["network_bg"]
        txt_bg = Colors.ColorDictionary["network_txt_bg"]
        txt_fg = Colors.ColorDictionary["network_txt_fg"]
        ent_bg = Colors.ColorDictionary["network_ent_bg"]
        ent_fg = Colors.ColorDictionary["network_ent_fg"]
        btn_bg = Colors.ColorDictionary["network_btn_bg"]
        btn_fg = Colors.ColorDictionary["network_btn_fg"]
        btn_alt_bg = Colors.ColorDictionary["network_btn_alt_bg"]
        btn_alt_fg = Colors.ColorDictionary["network_btn_alt_fg"]

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
        algorithmFunctions = PluginAPI.getPlugins("networking", "execute")

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