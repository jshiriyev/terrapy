import time

import tkinter as tk

import setup

from stream.graphics import tree

window = tk.Tk()

gui = tree("C:\\Users\\javid.s\\Documents")

t0 = time.time()

gui.draw(window)

t1 = time.time()

total = t1-t0

print(total)

window.mainloop()