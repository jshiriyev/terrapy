import tkinter as tk

from tkinter import ttk

# from PIL import Image, ImageTK

window = tk.Tk()

# [print(dirs) for dirs in dir(window)]

# printhelp(window.size)

window.geometry("300x300")

icon1 = tk.PhotoImage(file=".\\graphics\\Add\\Add-9.png")
icon2 = tk.PhotoImage(file=".\\graphics\\Edit\\Edit-9.png")
icon3 = tk.PhotoImage(file=".\\graphics\\Delete\\Delete-9.png")

button1 = ttk.Button(window,image=icon1)
button1.grid(row=0,column=0)

button2 = ttk.Button(window,image=icon2)
button2.grid(row=0,column=1)

button3 = ttk.Button(window,image=icon3)
button3.grid(row=0,column=2)

button4 = ttk.Button(window,text="+",width=3)
button4.grid(row=0,column=3)



window.mainloop()
