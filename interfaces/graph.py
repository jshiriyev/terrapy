import os

import tkinter as tk

from tkinter import ttk
from tkinter import filedialog

from ttkwidgets.autocomplete import AutocompleteEntryListbox

from matplotlib import pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg

# if __name__ == "__main__":
#     import setup

temp0 = {
    "name": "Standard",
    "subplots": [1,1],
    "twinx": [False],
    "title": [""],
    "xlabel": ["x-axis"],
    "ylabel": [["y-axis"]],
    "xticks": [None],
    "yticks": [None],
    "grid": [True]
}

temp1 = {
    "name": "Standard-dual horizontal stack",
    "subplots": [1,2],
    "twinx": [False,False],
    "title": ["Left","Right"],
    "xlabel": ["x-axis","x-axis"],
    "ylabel": [["y-axis"],["y-axis"]],
    "xticks": [None,None],
    "yticks": [None,None],
    "grid": [True,True]
}

temp2 = {
    "name": "Standard-dual vertical stack",
    "subplots": [2,1],
    "twinx": [False,False],
    "title": ["Top","Bottom"],
    "xlabel": ["x-axis","x-axis"],
    "ylabel": [["y-axis"],["y-axis"]],
    "xticks": [None,None],
    "yticks": [None,None],
    "grid": [True,True]
}

temp3 = {
    "name": "Standard-quadruple",
    "subplots": [2,2],
    "twinx": [True,True,True,False],
    "title": ["NW","NE","SW","SE"],
    "xlabel": ["x-axis","x-axis","x-axis","x-axis"],
    "ylabel": [["y-axis","y-axis-2"],["y-axis","y-axis-2"],["y-axis","y-axis-2"],["y-axis"]],
    "xticks": [None,None,None,None],
    "yticks": [None,None,None,None],
    "grid": [True,True,True,True]
}

templates = (temp0,temp1,temp2,temp3)

func_integer = lambda x: True if x.isdigit() or x == "" else False

class graph():
    
    def __init__(self,window,templates=templates,setLineFunc=None):

        self.root = window

        self.validate_integer = (self.root.register(func_integer),'%P')

        self.dirname = os.path.dirname(__file__)

        # configuration of window pane
        self.pane_NS = ttk.PanedWindow(self.root,orient=tk.VERTICAL,width=1000)

        self.frame_body = ttk.Frame(self.root,height=450)

        self.pane_NS.add(self.frame_body,weight=1)

        self.footer = tk.Listbox(self.root,height=5)

        self.pane_NS.add(self.footer,weight=0)

        self.pane_NS.pack(expand=1,fill=tk.BOTH)

        # configuration of top pane
        self.pane_EW = ttk.PanedWindow(self.frame_body,orient=tk.HORIZONTAL)

        self.frame_side = ttk.Frame(self.frame_body)

        self.pane_EW.add(self.frame_side,weight=0)

        self.figure = plt.Figure()
        self.canvas = FigureCanvasTkAgg(self.figure,self.frame_body)

        self.plot = self.canvas.get_tk_widget()

        self.pane_EW.add(self.plot,weight=1)

        self.pane_EW.pack(expand=1,fill=tk.BOTH)

        # configuration of top left pane
        self.pane_ns = ttk.PanedWindow(self.frame_side,orient=tk.VERTICAL,width=300)

        self.items = AutocompleteEntryListbox(self.frame_side,height=250,padding=0)
        self.items.config(completevalues=[],allow_other_values=False)

        if setLineFunc is not None:
            self.items.listbox.bind('<<ListboxSelect>>',lambda event: setLineFunc(event))

        self.pane_ns.add(self.items,weight=1)

        self.frame_temps = ttk.Frame(self.frame_side,height=200)

        self.frame_temps.rowconfigure(0,weight=0)
        self.frame_temps.rowconfigure(1,weight=1)

        self.frame_temps.columnconfigure(0,weight=1)
        self.frame_temps.columnconfigure(1,weight=0)
        self.frame_temps.columnconfigure(2,weight=0)

        self.frame_temps.label = ttk.Label(self.frame_temps,text="Templates")
        self.frame_temps.label.grid(row=0,column=0,sticky=tk.EW)

        self.frame_temps.addIcon = tk.PhotoImage(file=self.dirname+"\\graphics\\Add\\Add-9.png")
        self.frame_temps.editIcon = tk.PhotoImage(file=self.dirname+"\\graphics\\Edit\\Edit-9.png")
        self.frame_temps.delIcon = tk.PhotoImage(file=self.dirname+"\\graphics\\Delete\\Delete-9.png")

        self.frame_temps.addButton = ttk.Button(self.frame_temps,image=self.frame_temps.addIcon,command=self.addTemp)
        self.frame_temps.addButton.grid(row=0,column=1)

        self.frame_temps.editButton = ttk.Button(self.frame_temps,image=self.frame_temps.editIcon,command=self.editTemp)
        self.frame_temps.editButton.grid(row=0,column=2)

        self.frame_temps.delButton = ttk.Button(self.frame_temps,image=self.frame_temps.delIcon,command=self.delTemp)
        self.frame_temps.delButton.grid(row=0,column=3)

        self.frame_temps.listbox = tk.Listbox(self.frame_temps,exportselection=False)
        self.frame_temps.listbox.grid(row=1,column=0,columnspan=4,sticky=tk.NSEW)

        self.templates = templates

        for template in self.templates:
            self.frame_temps.listbox.insert(tk.END,template.get("name"))

        self.temp = {}

        self.frame_temps.listbox.bind('<<ListboxSelect>>',lambda event: self.setAxesFunc(event))

        self.pane_ns.add(self.frame_temps,weight=1)

        self.pane_ns.pack(expand=1,fill=tk.BOTH)

    def setAxesFunc(self,event):

        if not self.frame_temps.listbox.curselection():
            return

        if len(self.temp)!=0:
            if self.temp == self.templates[self.frame_temps.listbox.curselection()[0]]:
                return

        self.temp = self.templates[self.frame_temps.listbox.curselection()[0]]

        xnum,ynum = self.temp.get("subplots")
        
        if hasattr(self,"axes"):
            [self.figure.delaxes(axis) for axis in self.axes]

        self.axes = []

        for index in range(xnum*ynum):
            axis0 = self.figure.add_subplot(xnum,ynum,index+1)
            if self.temp.get("twinx")[index]:
                axis1 = axis0.twinx()
            axis0.set_title(self.temp.get("title")[index])
            axis0.set_xlabel(self.temp.get("xlabel")[index])
            axis0.set_ylabel(self.temp.get("ylabel")[index][0])
            if self.temp.get("twinx")[index]:
                axis1.set_ylabel(self.temp.get("ylabel")[index][1])
            if self.temp.get("xticks")[index] is not None:
                axis0.set_xticks(self.temp.get("xticks")[index])
            if self.temp.get("yticks")[index] is not None:
                axis0.set_yticks(self.temp.get("yticks")[index])
            axis0.grid(self.temp.get("grid")[index])
            self.axes.append(axis0)
            if self.temp.get("twinx")[index]:
                self.axes.append(axis1)

        # for tick in self.graph.axes[2].get_xticklabels():
        #         tick.set_rotation(45)

        status = "{} template has been selected.".format(self.temp.get("name"))

        self.footer.insert(tk.END,status)
        self.footer.see(tk.END)

        self.figure.set_tight_layout(True)

        self.canvas.draw()

    def addTemp(self):

        self.setTopTemplate()

    def editTemp(self):

        if not self.frame_temps.listbox.curselection(): return
        
        name = self.frame_temps.listbox.get(self.frame_temps.listbox.curselection())

        item = self.frame_temps.get("names").index(name)

        self.setTopTemplate(item=item)

    def delTemp(self):

        if not self.frame_temps.listbox.curselection(): return

        name = self.frame_temps.listbox.get(self.frame_temps.listbox.curselection())

        item = self.temp.get("name").index(name)
        
        self.frame_temps.listbox.delete(item)

        self.temp.get("name").pop(item)
        # self.temp.get("xnumgrid").pop(item)
        # self.temp.get("ynumgrid").pop(item)

    def setTopTemplate(self,item=None):

        if hasattr(self,"topTemplate"):
            if self.topTemplate.winfo_exists(): return

        self.topTemplate = tk.Toplevel()

        self.topTemplate.geometry("700x400")

        self.topTemplate.resizable(0,0)

        self.style = ttk.Style(self.topTemplate)

        self.style.configure("TNotebook.Tab",width=20,anchor=tk.CENTER)

        self.topTempNotebook = ttk.Notebook(self.topTemplate)

        self.topTempNotebookTemplate = tk.Frame(self.topTempNotebook)

        self.topTempNotebookTemplateFrame0 = tk.Frame(self.topTempNotebookTemplate,borderwidth=2,relief=tk.GROOVE)

        self.topTempNotebookTemplateFrame0.tempnameLabel = ttk.Label(self.topTempNotebookTemplateFrame0,text="Template Name")
        self.topTempNotebookTemplateFrame0.tempnameLabel.grid(row=0,column=0,padx=(10,10),pady=(20,2))

        self.topTempNotebookTemplateFrame0.tempname = ttk.Entry(self.topTempNotebookTemplateFrame0,width=30)
        self.topTempNotebookTemplateFrame0.tempname.grid(row=0,column=1,padx=(10,20),pady=(20,2),sticky=tk.EW)

        self.topTempNotebookTemplateFrame0.tempname.focus()

        self.topTempNotebookTemplateFrame0.legendLabel = ttk.Label(self.topTempNotebookTemplateFrame0,text="Legend Position")
        self.topTempNotebookTemplateFrame0.legendLabel.grid(row=1,column=0,padx=(10,10),pady=(2,2))

        self.topTempNotebookTemplateFrame0.legend = ttk.Entry(self.topTempNotebookTemplateFrame0,width=30)
        self.topTempNotebookTemplateFrame0.legend.grid(row=1,column=1,padx=(10,20),pady=(2,2),sticky=tk.EW)

        self.topTempNotebookTemplateFrame0.pack(side=tk.LEFT,expand=0,fill=tk.Y)

        self.topTempNotebookTemplateFrame1 = tk.Frame(self.topTempNotebookTemplate)

        self.topTempNotebookTemplateFrame1.xgridlabel = ttk.Label(self.topTempNotebookTemplateFrame1,text="Grids in Y")
        self.topTempNotebookTemplateFrame1.xgridlabel.grid(row=0,column=0,sticky=tk.EW,padx=(10,10),pady=(20,2))

        self.topTempNotebookTemplateFrame1.xnumgrid = ttk.Entry(self.topTempNotebookTemplateFrame1,width=10,validate="key",validatecommand=self.validate_integer)
        self.topTempNotebookTemplateFrame1.xnumgrid.grid(row=0,column=1,sticky=tk.EW,padx=(10,2),pady=(20,2))

        self.topTempNotebookTemplateFrame1.ygridlabel = ttk.Label(self.topTempNotebookTemplateFrame1,text="Grids in X")
        self.topTempNotebookTemplateFrame1.ygridlabel.grid(row=1,column=0,sticky=tk.EW,padx=(10,10),pady=(2,2))

        self.topTempNotebookTemplateFrame1.ynumgrid = ttk.Entry(self.topTempNotebookTemplateFrame1,width=10,validate="key",validatecommand=self.validate_integer)
        self.topTempNotebookTemplateFrame1.ynumgrid.grid(row=1,column=1,sticky=tk.EW,padx=(10,2),pady=(2,2))

        self.topTempNotebookTemplateFrame1.pack(side=tk.LEFT,expand=1,fill=tk.BOTH)
        
        self.topTempNotebook.add(self.topTempNotebookTemplate,text="Template Options",compound=tk.CENTER)

        self.topTempNotebookLine = tk.Frame(self.topTempNotebook)

        self.topTempNotebook.add(self.topTempNotebookLine,text="Line Options",compound=tk.CENTER)

        self.topTempNotebook.pack(side=tk.TOP,expand=1,fill=tk.BOTH,padx=(0,1))

        if item is not None:

            tempname = self.temp.get("name")[item]
            # xnumgrid = self.temp.get("xnumgrid")[item]
            # ynumgrid = self.temp.get("ynumgrid")[item]

            self.topTempTemplateFrame.tempname.insert(0,tempname)
            self.topTempTemplateFrame.xnumgrid.insert(0,xnumgrid)
            self.topTempTemplateFrame.ynumgrid.insert(0,ynumgrid)

        buttonname = "Add Template" if item is None else "Edit Template"

        self.topTemplate.button = ttk.Button(self.topTemplate,text=buttonname,width=20,command=lambda: self.topTempButtonApply(item))
        self.topTemplate.button.pack(side=tk.TOP,anchor=tk.E,padx=(0,1),pady=(1,1))

        self.topTemplate.button.bind('<Return>',lambda event: self.topTempButtonApply(item,event))

        self.topTemplate.mainloop()

    def topTempButtonApply(self,item=None,event=None):

        if event is not None and event.widget!=self.topTemplate.button:
            return

        if item is not None:
            names = [name for index,name in enumerate(self.temp.get("name")) if index!=item]
        else:
            names = self.temp.get("name")

        name = self.topTempNotebookTemplateFrame0.tempname.get()

        if name in names:
            tk.messagebox.showerror("Error","You have a template with the same name!",parent=self.topTemplate)
            return
        elif name.strip()=="":
            tk.messagebox.showerror("Error","You have not named the template!",parent=self.topTemplate)
            return

        if item is None:
            item = len(self.temps.get("names"))
        else:
            self.frame_temps.listbox.delete(item)
            self.temp.get("name").pop(item)
            # self.temp.get("xnumgrid").pop(item)
            # self.temp.get("ynumgrid").pop(item)
        
        self.frame_temps.listbox.insert(item,name)

        self.temp.get("name").insert(item,name)

        try:
            xnumgrid = int(self.topTempNotebookTemplateFrame1.xnumgrid.get())
        except ValueError:
            xnumgrid = 1

        # self.temp.get("xnumgrid").insert(item,xnumgrid)

        try:
            ynumgrid = int(self.topTempNotebookTemplateFrame1.ynumgrid.get())
        except ValueError:
            ynumgrid = 1

        # self.temp.get("ynumgrid").insert(item,ynumgrid)

        self.topTemplate.destroy()

if __name__ == "__main__":
    
    window = tk.Tk()

    gui = graph(window)

    window.mainloop()
