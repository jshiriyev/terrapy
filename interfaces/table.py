import os

import tkinter as tk

from tkinter import ttk
from tkinter import filedialog
from tkinter import font as tkfont

from ttkwidgets.autocomplete import AutocompleteEntryListbox

import numpy as np

if __name__ == "__main__":
    import setup

class table():

    def __init__(self,window):

        self.root = window

        self.dirname = os.path.dirname(__file__)

    def draw(self,func=None):

        self.scrollbar = tk.Scrollbar(self.root)

        self.columns = ["#"+str(idx) for idx,_ in enumerate(self.headers,start=1)]

        self.tree = ttk.Treeview(self.root,columns=self.columns,show="headings",selectmode="browse",yscrollcommand=self.scrollbar.set)

        self.sortReverseFlag = [False for column in self.columns]

        for idx,(column,header) in enumerate(zip(self.columns,self.headers)):
            self.tree.column(column,anchor=tk.W,stretch=tk.NO)
            self.tree.heading(column,text=header,anchor=tk.W)

        self.tree.column(self.columns[-1],stretch=tk.YES)

        self.tree.pack(side=tk.LEFT,expand=1,fill=tk.BOTH)

        self.scrollbar.pack(side=tk.LEFT,fill=tk.Y)

        self.scrollbar.config(command=self.tree.yview)

        # self.frame = tk.Frame(self.root,width=50)
        # self.frame.configure(background="white")
        # self.frame.pack(side=tk.LEFT,fill=tk.Y)

        self.tree.bind("<KeyPress-i>",self.addItem)

        # self.button_Add = tk.Button(self.frame,text="Add Item",width=50,command=self.addItem)
        # self.button_Add.pack(side=tk.TOP,ipadx=5,padx=10,pady=(5,1))

        self.tree.bind("<KeyPress-e>",self.editItem)
        self.tree.bind("<Double-1>",self.editItem)

        self.tree.bind("<Delete>",self.deleteItem)

        self.tree.bind("<KeyPress-j>",self.moveDown)
        self.tree.bind("<KeyPress-k>",self.moveUp)

        self.tree.bind("<Button-1>",self.sort_column)

        self.tree.bind("<Control-KeyPress-s>",lambda event: self.saveChanges(func,event))

        # self.button_Save = tk.Button(self.frame,text="Save Changes",width=50,command=lambda: self.saveChanges(func))
        # self.button_Save.pack(side=tk.TOP,ipadx=5,padx=10,pady=(10,1))

        self.root.protocol('WM_DELETE_WINDOW',lambda: self.close_no_save(func))

        self.counter = self.running[0].size

        self.iids = np.arange(self.counter)

        self.added = []
        self.edited = []
        self.deleted = []

        self.refill()

    def refill(self):

        self.tree.delete(*self.tree.get_children())

        rows = np.array(self.running).T.tolist()

        for iid,row in zip(self.iids,rows):
            self.tree.insert(parent="",index="end",iid=iid,values=row)

    def addItem(self,event):

        if hasattr(self,"topAddItem"):
            if self.topAddItem.winfo_exists():
                return

        self.topAddItem = tk.Toplevel()

        self.topAddItem.resizable(0,0)

        for index,header in enumerate(self.headers):
            label = "label_"+str(index)
            entry = "entry_"+str(index)
            pady = (30,5) if index==0 else (5,5)
            setattr(self.topAddItem,label,tk.Label(self.topAddItem,text=header,font="Helvetica 11",width=20,anchor=tk.E))
            setattr(self.topAddItem,entry,tk.Entry(self.topAddItem,width=30,font="Helvetica 11"))
            getattr(self.topAddItem,label).grid(row=index,column=0,ipady=5,padx=(10,5),pady=pady)
            getattr(self.topAddItem,entry).grid(row=index,column=1,ipady=5,padx=(5,10),pady=pady)

        self.topAddItem.entry_0.focus()

        self.topAddItem.button = tk.Button(self.topAddItem,text="Add Item",command=self.addItemEnterClicked)
        self.topAddItem.button.grid(row=index+1,column=0,columnspan=2,ipady=5,padx=15,pady=(15,30),sticky=tk.EW)

        self.topAddItem.button.bind('<Return>',self.addItemEnterClicked)

        self.topAddItem.mainloop()

    def addItemEnterClicked(self,event=None):

        if event is not None and event.widget!=self.topAddItem.button:
            return

        values = []

        for idx,header in enumerate(self.headers):
            entry = "entry_"+str(idx)
            value = getattr(self.topAddItem,entry).get()
            values.append(value)

        self.added.append(self.counter)

        self.set_rows(values)

        self.iids = np.append(self.iids,self.counter)

        self.tree.insert(parent="",index="end",iid=self.counter,values=values)

        self.counter += 1

        self.topAddItem.destroy()

    def editItem(self,event):

        region = self.tree.identify('region',event.x,event.y)

        if region=="separator":
            self.autowidth(event)
            return

        if not(region=="cell" or event.char=="e"):
            return

        if hasattr(self,"topEditItem"):
            if self.topEditItem.winfo_exists():
                return

        if not self.tree.selection():
            return
        else:
            item = self.tree.selection()[0]

        values = self.tree.item(item)['values']

        self.topEditItem = tk.Toplevel()

        self.topEditItem.resizable(0,0)

        for idx,(header,explicit) in enumerate(zip(self.headers,self.headers)):
            label = "label_"+str(idx)
            entry = "entry_"+str(idx)
            pady = (30,5) if idx==0 else (5,5)
            setattr(self.topEditItem,label,tk.Label(self.topEditItem,text=explicit,font="Helvetica 11",width=20,anchor=tk.E))
            setattr(self.topEditItem,entry,tk.Entry(self.topEditItem,width=30,font="Helvetica 11"))
            getattr(self.topEditItem,label).grid(row=idx,column=0,ipady=5,padx=(10,5),pady=pady)
            getattr(self.topEditItem,entry).grid(row=idx,column=1,ipady=5,padx=(5,10),pady=pady)
            getattr(self.topEditItem,entry).insert(0,values[idx])

        self.topEditItem.entry_0.focus()

        self.topEditItem.button = tk.Button(self.topEditItem,text="Save Item Edit",command=lambda: self.editItemEnterClicked(item))
        self.topEditItem.button.grid(row=idx+1,column=0,columnspan=2,ipady=5,padx=15,pady=(15,30),sticky=tk.EW)

        self.topEditItem.button.bind('<Return>',lambda event: self.editItemEnterClicked(item,event))

        self.topEditItem.mainloop()

    def editItemEnterClicked(self,item,event=None):

        if event is not None and event.widget!=self.topEditItem.button:
            return

        values = []

        for idx,header in enumerate(self.headers):
            entry = "entry_"+str(idx)
            value = getattr(self.topEditItem,entry).get()
            values.append(value)

        self.edited.append([int(item),self.tree.item(item)["values"]])

        self.set_rows(values,np.argmax(self.iids==int(item)))

        self.tree.item(item,values=values)

        self.topEditItem.destroy()

    def deleteItem(self,event):

        if not self.tree.selection():
            return
        else:
            item = self.tree.selection()[0]

        self.deleted.append([int(item),self.tree.item(item)["values"]])

        self.del_rows(np.argmax(self.iids==int(item)),inplace=True)

        self.iids = np.delete(self.iids,np.argmax(self.iids==int(item)))

        self.tree.delete(item)

    def autowidth(self,event):

        column = self.tree.identify('column',event.x,event.y)

        index = self.columns.index(column)

        if index==len(self.columns)-1:
            return

        header_char_count = len(self.headers[index])

        vcharcount = np.vectorize(lambda x: len(x))

        if self.running[index].size != 0:
            column_char_count = vcharcount(self.running[index].astype(str)).max()
        else:
            column_char_count = 0

        char_count = max(header_char_count,column_char_count)

        width = tkfont.Font(family="Consolas", size=12).measure("A"*char_count)

        column_width_old = self.tree.column(column,"width")

        self.tree.column(column,width=width)

        column_width_new = self.tree.column(column,"width")

        column_width_last_old = self.tree.column(self.columns[-1],"width")

        column_width_last_new = column_width_last_old+column_width_old-column_width_new

        self.tree.column(self.columns[-1],width=column_width_last_new)

    def moveUp(self,event):
 
        if not self.tree.selection():
            return
        else:
            item = self.tree.selection()[0]

        self.tree.move(item,self.tree.parent(item),self.tree.index(item)-1)

    def moveDown(self,event):

        if not self.tree.selection():
            return
        else:
            item = self.tree.selection()[0]

        self.tree.move(item,self.tree.parent(item),self.tree.index(item)+1)

    def sort_column(self,event):

        region = self.tree.identify('region',event.x,event.y)

        if region!="heading":
            return

        column = self.tree.identify('column',event.x,event.y)

        header_index = self.columns.index(column)

        reverseFlag = self.sortReverseFlag[header_index]

        N = self.running[0].size

        argsort = np.argsort(self.running[header_index])

        if reverseFlag:
            argsort = np.flip(argsort)

        for index,column in enumerate(self.running):
            self.running[index] = column[argsort]

        self.iids = self.iids[argsort]
        # indices = np.arange(N)

        # sort_indices = indices[np.argsort(argsort)]

        self.refill()

        # for item,sort_index in zip(self.iids,sort_indices):
        #     self.tree.move(item,self.tree.parent(item),sort_index)

        self.sortReverseFlag[header_index] = not reverseFlag

    def saveChanges(self,func=None,event=None):

        self.added = []
        self.edited = []
        self.deleted = []

        if func is not None:
            func()

    def close_no_save(self,func=None):

        try:
            for deleted in self.deleted:
                self.set_rows(deleted[1])
        except:
            print("Could not bring back deleted rows ...")

        try:
            for edited in self.edited:
                self.set_rows(edited[1],np.argmax(self.iids==edited[0]))
        except:
            print("Could not bring back editions ...")

        added = [np.argmax(self.iids==add) for add in self.added]

        try:
            self.del_rows(added,inplace=True)
        except:
            print("Could not remove additions ...")

        try:
            if func is not None:
                func()
        except:
            print("Could not run the called function ...")

        self.root.destroy()

if __name__ == "__main__":

    window = tk.Tk()

    gui = table(headers=["First Name","Last Name","Contact"])

    gui.draw(window)

    window.mainloop()
