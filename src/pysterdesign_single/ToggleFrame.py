#https://stackoverflow.com/a/52152773
from tkinter import Frame

class ToggleFrame(Frame):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
    def enable(self, state='!disabled') -> None:
        def cstate(widget):
            # Is this widget a container?
            if widget.winfo_children:
                # It's a container, so iterate through its children
                for w in widget.winfo_children():
                    # change its state
                    w.state = state
                    # and then recurse to process ITS children
                    cstate(w)

        cstate(self)

    def disable(self):
        self.enable('disabled')