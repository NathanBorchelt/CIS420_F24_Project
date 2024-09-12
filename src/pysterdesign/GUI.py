try:
    from tkinter import *
    from tkinter import font, Text

except ImportError:
    try:
        from Tkinter import *
        from Tkinter import font, Text
    except ImportError:
        print("This system lacks 'Tkinter' and 'tkinter', making the program unable to execute.")
        exit(1)

from typing import List, Union

from ToggleFrame import ToggleFrame
from NodesFrame import NodesFrame
from PerformanceFrame import PerformanceFrame
from NetworkFrame import NetworkFrame
from UPSFrame import UPSFrame
from DesignFrame import DesignFrame
from SettingsFrame import SettingsFrame

import Colors
from ScrollableFrame import ScrollableFrame
from IndexCounterEntry import IndexCounterEntry
import PluginAPI


'''
Universal methods that are designed to be usable anywhere, thus must come first, even though they get assined to static variables
'''

#https://lindevs.com/code-snippets/get-screen-size-of-each-monitor-using-python
def getDisplaySize():
    from screeninfo import get_monitors
    smallestMonitor = [float('inf'), float('inf')]

    for monitor in get_monitors():
        if ((monitor.width * monitor.height) < (smallestMonitor[0] * smallestMonitor[1])):
            smallestMonitor = [monitor.width, monitor.height]
    return smallestMonitor

SCREEN_RESOLUTION = getDisplaySize()
TITLE = "Computer Cluster Design Tool"

framesDictionary = dict()

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
    selectedRB.config(fg=Colors.ColorDictionary["btn_bg"], bg=Colors.ColorDictionary["btn_fg"])
    for rButton in allRB:
        if rButton != selectedRB:
            rButton.config(fg=Colors.ColorDictionary["btn_alt_bg"], bg=Colors.ColorDictionary["btn_alt_fg"])
    selectedActionFrame = dictionary[commonVar.get()]["frame"]
    wantedFrame(allFramesAffected, selectedActionFrame)

class ClusterDesign(Tk):#predefine the globals here
    pixel = None

    def __init__(self, *args, **kwargs):
        Tk.__init__(self, *args, **kwargs)
        #all TypeVars
        NORMAL_FONT = font.Font(size=11)

        self.title(TITLE)
        #print(SCREEN_RESOLUTION)
        self.geometry(str(int(SCREEN_RESOLUTION[0]*((2**.5)/2)))+'x'+str(int(SCREEN_RESOLUTION[1]*((2**.5)/2))))
        self.config(bg=Colors.ColorDictionary["bg"])
        self.update()

        self.pixel = PhotoImage()

        mainFrame = Frame(self, padx=Colors.ColorDictionary["padx"], pady=Colors.ColorDictionary["pady"], width=self.winfo_width(), height=self.winfo_height(), bg=Colors.ColorDictionary["bg"])

        mainFrame.pack(fill="both", expand=True)
        self.update()

        #area that holds the file IO
        fileOptionsFrame = Frame(mainFrame, padx=Colors.ColorDictionary["fileio_padx"], pady=Colors.ColorDictionary["fileio_pady"], width=mainFrame.winfo_width(), height=int(mainFrame.winfo_height()*.075), relief=SUNKEN, bg=Colors.ColorDictionary["fileio_bg"])

        allActionOptionsFrame = Frame(mainFrame, padx=Colors.ColorDictionary["padx"], pady=Colors.ColorDictionary["pady"], width=mainFrame.winfo_width(), height=int(mainFrame.winfo_height()*.95), relief=SUNKEN, bg=Colors.ColorDictionary["bg"])

        fileOptionsFrame.pack(fill=X)
        allActionOptionsFrame.pack(fill=BOTH,expand=True)
        self.update()

        #top row of options, will allow to swap between diferent parts of the program.
        actionButtonFrame = Frame(allActionOptionsFrame, padx=Colors.ColorDictionary["padx"], pady=Colors.ColorDictionary["pady"], width=allActionOptionsFrame.winfo_width(), height=int(allActionOptionsFrame.winfo_height()*.085), relief=RAISED, bg=Colors.ColorDictionary["bg"])

        actionButtonFrame.pack()
        self.update()

        #creation of all of the different frames, they are all designed similarly as they are mode to fill the same space on the screen.
        nodesFrame = NodesFrame(allActionOptionsFrame, padx=Colors.ColorDictionary["padx"], pady=Colors.ColorDictionary["pady"], width=allActionOptionsFrame.winfo_width(), height=int(allActionOptionsFrame.winfo_height()*.915), relief=SUNKEN, bg=Colors.ColorDictionary["node_bg"])
        performanceFrame = PerformanceFrame(allActionOptionsFrame, padx=Colors.ColorDictionary["padx"], pady=Colors.ColorDictionary["pady"], width=allActionOptionsFrame.winfo_width(), height=int(allActionOptionsFrame.winfo_height()*.915), relief=SUNKEN, bg=Colors.ColorDictionary["performance_bg"])
        networkFrame = NetworkFrame(allActionOptionsFrame, padx=Colors.ColorDictionary["padx"], pady=Colors.ColorDictionary["pady"], width=allActionOptionsFrame.winfo_width(), height=int(allActionOptionsFrame.winfo_height()*.915), relief=SUNKEN, bg=Colors.ColorDictionary["network_bg"])
        upsFrame = UPSFrame(allActionOptionsFrame, padx=Colors.ColorDictionary["padx"], pady=Colors.ColorDictionary["pady"], width=allActionOptionsFrame.winfo_width(), height=int(allActionOptionsFrame.winfo_height()*.915), relief=SUNKEN, bg=Colors.ColorDictionary["ups_bg"])
        designFrame = DesignFrame(allActionOptionsFrame, padx=Colors.ColorDictionary["padx"], pady=Colors.ColorDictionary["pady"], width=allActionOptionsFrame.winfo_width(), height=int(allActionOptionsFrame.winfo_height()*.915), relief=SUNKEN, bg=Colors.ColorDictionary["design_bg"])
        settingsFrame = SettingsFrame(allActionOptionsFrame, padx=Colors.ColorDictionary["padx"], pady=Colors.ColorDictionary["pady"], width=allActionOptionsFrame.winfo_width(), height=int(allActionOptionsFrame.winfo_height()*.915), relief=SUNKEN, bg=Colors.ColorDictionary["settings_bg"])
        aboutFrame = AboutFrame(allActionOptionsFrame, padx=Colors.ColorDictionary["padx"], pady=Colors.ColorDictionary["pady"], width=allActionOptionsFrame.winfo_width(), height=int(allActionOptionsFrame.winfo_height()*.915), relief=SUNKEN, bg=Colors.ColorDictionary["about_bg"])
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
            rb = Radiobutton(actionButtonFrame, text=framesDictionary[dictionary]['text'], variable=actionFrameSelection, value=framesDictionary[dictionary]['return'], indicatoron=0, bg=Colors.ColorDictionary["btn_bg"], fg=Colors.ColorDictionary["btn_fg"], font=font.Font(size=14))
            rb.config(command= lambda rButton=rb, listButtons=actionRadioButtons, dictionary=framesDictionary, affected=allActionFrames, commonVar=actionFrameSelection: selectRadioButton_Actions(rButton, listButtons,dictionary, affected, commonVar))
            rb.pack(side=LEFT, fill=X, padx=Colors.ColorDictionary["padx"], pady=Colors.ColorDictionary["pady"])
            actionRadioButtons.append(rb)

        for frame in allActionFrames:
            frame.pack(fill=BOTH, expand=True)
        wantedFrame(allActionFrames, nodesFrame)
        self.update()



application = ClusterDesign()
application.mainloop()

