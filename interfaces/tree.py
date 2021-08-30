import os

import tkinter as tk

from tkinter import ttk
from tkinter import filedialog
from tkinter import font as tkfont

from ttkwidgets.autocomplete import AutocompleteEntryListbox

import numpy as np

import openpyxl

class tree():

    def __init__(self,dirpath):

        self.dirpath = dirpath

    def draw(self,window,func=None):

        self.root = window

        self.scrollbar = ttk.Scrollbar(self.root)

        self.tree = ttk.Treeview(self.root,show="headings tree",selectmode="browse",yscrollcommand=self.scrollbar.set)

        self.tree.pack(side=tk.LEFT,expand=1,fill=tk.BOTH)

        self.scrollbar.pack(side=tk.LEFT,fill=tk.Y)

        self.scrollbar.config(command=self.tree.yview)

        self.tree.bind("<Button-1>",lambda event: self.set_path(func,event))

        self.refill()

    def refill(self):

        self.tree.heading("#0",text="")

        self.tree.delete(*self.tree.get_children())

        iterator = os.walk(self.dirpath)

        parents_name = []
        parents_link = []

        counter  = 0

        while True:

            try:
                root,dirs,files = next(iterator)
            except StopIteration:
                break

            if counter==0:
                dirname = os.path.split(root)[1]
                self.tree.heading("#0",text=dirname,anchor=tk.W)
                parents_name.append(root)
                parents_link.append("")
            
            parent = parents_link[parents_name.index(root)]

            for directory in dirs:
                link = self.tree.insert(parent,'end',iid=counter,text=directory)
                counter += 1

                parents_name.append(os.path.join(root,directory))
                parents_link.append(link)

            for file in files:            
                self.tree.insert(parent,'end',iid=counter,text=file)
                counter += 1

    def set_path(self,func=None,event=None):

        if event is not None:
            region = self.tree.identify("region",event.x,event.y)
        else:
            return

        if region!="tree":
            return

        item = self.tree.identify("row",event.x,event.y)

        path = self.tree.item(item)['text']

        while True:

            item = self.tree.parent(item)

            if item:
                path = os.path.join(self.tree.item(item)['text'],path)
            else:
                path = os.path.join(self.dirpath,path)
                break

        if func is not None:
            func(path)
        else:
            print(path)

if __name__ == "__main__":

    import time

    window = tk.Tk()

    gui = tree("C:\\Users\\javid.s\\Documents")

    t0 = time.time()

    gui.draw(window)
    
    t1 = time.time()

    total = t1-t0
    
    print(total)

    window.mainloop()
