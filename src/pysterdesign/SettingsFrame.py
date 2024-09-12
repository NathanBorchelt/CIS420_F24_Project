from tkinter import LEFT, RIGHT, TOP, X, DoubleVar, Entry, IntVar, Label, font

from ToggleFrame import ToggleFrame
from IndexCounterEntry import IndexCounterEntry
import Colors

class SettingsFrame(ToggleFrame):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        fontSize = 14
        NORMAL_FONT = font.Font(size=fontSize)

        padx = Colors.ColorDictionary["settings_padx"]
        pady = Colors.ColorDictionary["settings_pady"]
        bg = Colors.ColorDictionary["settings_bg"]
        txt_bg = Colors.ColorDictionary["settings_txt_bg"]
        txt_fg = Colors.ColorDictionary["settings_txt_fg"]
        ent_bg = Colors.ColorDictionary["settings_ent_bg"]
        ent_fg = Colors.ColorDictionary["settings_ent_fg"]
        btn_bg = Colors.ColorDictionary["settings_btn_bg"]
        btn_fg = Colors.ColorDictionary["settings_btn_fg"]
        btn_alt_bg = Colors.ColorDictionary["settings_btn_alt_bg"]
        btn_alt_fg = Colors.ColorDictionary["settings_btn_alt_fg"]

        widthLimitFrame = ToggleFrame(self, padx=padx, pady=pady, bg=bg)

        rackHeightFrame = ToggleFrame(widthLimitFrame, padx=padx, pady=pady, bg=bg)
        systemLifetimeFrame = ToggleFrame(widthLimitFrame, padx=padx, pady=pady, bg=bg)
        pricekWhFrame = ToggleFrame(widthLimitFrame, padx=padx, pady=pady, bg=bg)
        yearlyRentalFrame = ToggleFrame(widthLimitFrame, padx=padx, pady=pady, bg=bg)

        rackHeightLabel = Label(rackHeightFrame, bg=txt_bg, fg=txt_fg, text="Rack height, in units:      ", font=NORMAL_FONT)
        rackHeightEntry = IndexCounterEntry(0, (2**31-1), 1, fontSize, 42, txt_bg, txt_fg, btn_bg, btn_fg, btn_alt_bg, btn_alt_fg, master=rackHeightLabel.master, bg=rackHeightLabel['bg'])
        systemLifetimeLabel = Label(systemLifetimeFrame, bg=txt_bg, fg=txt_fg, text="System lifetime, in years:      ", font=NORMAL_FONT)
        systemLifetimeEntry = IndexCounterEntry(0, (2**31-1), 1, fontSize, 42, txt_bg, txt_fg, btn_bg, btn_fg, btn_alt_bg, btn_alt_fg, master=systemLifetimeLabel.master, bg=rackHeightLabel['bg'])
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