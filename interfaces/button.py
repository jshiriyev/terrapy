import tkinter as tk

from tkinter import ttk

from ttkwidgets.autocomplete import AutocompleteEntryListbox


# from PIL import Image, ImageTK

window = tk.Tk()

# [print(dirs) for dirs in dir(window)]

# printhelp(window.size)

window.geometry("300x300")

# icon1 = tk.PhotoImage(file=".\\graphics\\Add\\Add-9.png")
# icon2 = tk.PhotoImage(file=".\\graphics\\Edit\\Edit-9.png")
# icon3 = tk.PhotoImage(file=".\\graphics\\Delete\\Delete-9.png")

# pane = ttk.PanedWindow(window,orient=tk.VERTICAL)

# frame1 = tk.Frame(window)

# pane.add(frame1)

# button1 = ttk.Button(frame1,image=icon1)
# button1.grid(row=0,column=0)

# button2 = ttk.Button(frame1,image=icon2)
# button2.grid(row=0,column=1)

# button3 = ttk.Button(frame1,image=icon3)
# button3.grid(row=0,column=2)

# # frame1.pack()

# button4 = ttk.Button(window,text="+")
# button4.pack()




# pane.add(button4)

# pane.pack(expand=1,fill=tk.BOTH)

pane_ns = ttk.PanedWindow(window,orient=tk.VERTICAL,width=300)

searchbox = AutocompleteEntryListbox(window,height=250,padding=0)
searchbox.config(completevalues=[],allow_other_values=False)

pane_ns.add(searchbox,weight=1)

template = ttk.Frame(window,height=200)

template.rowconfigure(0,weight=0)
template.rowconfigure(1,weight=1)

template.columnconfigure(0,weight=1)
template.columnconfigure(1,weight=0)
template.columnconfigure(2,weight=0)

template_label = ttk.Label(template,text="Templates")
template_label.grid(row=0,column=0,sticky=tk.EW)

icon1 = tk.PhotoImage(file=".\\graphics\\Add\\Add-9.png")
icon2 = tk.PhotoImage(file=".\\graphics\\Edit\\Edit-9.png")
icon3 = tk.PhotoImage(file=".\\graphics\\Delete\\Delete-9.png")

button1 = ttk.Button(template,image=icon1)
button1.grid(row=0,column=1)

button2 = ttk.Button(template,image=icon2)
button2.grid(row=0,column=2)

button3 = ttk.Button(template,image=icon3)
button3.grid(row=0,column=3)

template_listbox = tk.Listbox(template,exportselection=False)
template_listbox.grid(row=1,column=0,columnspan=4,sticky=tk.NSEW)

pane_ns.add(template,weight=1)

pane_ns.pack(expand=1,fill=tk.BOTH)


window.mainloop()
