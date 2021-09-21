import os

import tkinter as tk

from tkinter import ttk
from tkinter import filedialog

from ttkwidgets.autocomplete import AutocompleteEntryListbox

from matplotlib import pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg

if __name__ == "__main__":
    import setup

class graph():

    funcCheckINT = lambda x: True if x.isdigit() or x == "" else False
    
    def __init__(self,window,setPlotAxes=None,setPlotLines=None):

        self.root = window

        self.validateINT = (self.root.register(self.funcCheckINT),'%P')

        self.dirname = os.path.dirname(__file__)

        # configuration of window pane
        self.pane_NS = ttk.PanedWindow(self.root,orient=tk.VERTICAL,width=1000)

        self.body = ttk.Frame(self.root,height=450)

        self.pane_NS.add(self.body,weight=1)

        self.foot = tk.Listbox(self.root,height=5)

        self.pane_NS.add(self.foot,weight=0)

        self.pane_NS.pack(expand=1,fill=tk.BOTH)

        # configuration of top pane
        self.pane_EW = ttk.PanedWindow(self.body,orient=tk.HORIZONTAL)

        self.side = ttk.Frame(self.body)

        self.pane_EW.add(self.side,weight=0)

        self.figure = plt.Figure()
        self.canvas = FigureCanvasTkAgg(self.figure,self.body)

        self.plot = self.canvas.get_tk_widget()

        self.pane_EW.add(self.plot,weight=1)

        self.pane_EW.pack(expand=1,fill=tk.BOTH)

        # configuration of top left pane
        self.pane_ns = ttk.PanedWindow(self.side,orient=tk.VERTICAL,width=300)

        self.items = AutocompleteEntryListbox(self.side,height=250,padding=0)
        self.items.config(completevalues=[],allow_other_values=False)

        if setPlotLines is not None:
            self.items.listbox.bind('<<ListboxSelect>>',lambda event: setPlotLines(None,event))
        else:
            self.items.listbox.bind('<<ListboxSelect>>',lambda event: self.setPlotLines(None,event))

        self.pane_ns.add(self.items,weight=1)

        self.temps = ttk.Frame(self.side,height=200)

        self.temps.rowconfigure(0,weight=0)
        self.temps.rowconfigure(1,weight=1)

        self.temps.columnconfigure(0,weight=1)
        self.temps.columnconfigure(1,weight=0)
        self.temps.columnconfigure(2,weight=0)

        self.temps.label = ttk.Label(self.temps,text="Templates")
        self.temps.label.grid(row=0,column=0,sticky=tk.EW)

        self.temps.addIcon = tk.PhotoImage(file=self.dirname+"\\graphics\\Add\\Add-9.png")
        self.temps.editIcon = tk.PhotoImage(file=self.dirname+"\\graphics\\Edit\\Edit-9.png")
        self.temps.delIcon = tk.PhotoImage(file=self.dirname+"\\graphics\\Delete\\Delete-9.png")

        self.temps.addButton = ttk.Button(self.temps,image=self.temps.addIcon,command=self.addTemp)
        self.temps.addButton.grid(row=0,column=1)

        self.temps.editButton = ttk.Button(self.temps,image=self.temps.editIcon,command=self.editTemp)
        self.temps.editButton.grid(row=0,column=2)

        self.temps.delButton = ttk.Button(self.temps,image=self.temps.delIcon,command=self.delTemp)
        self.temps.delButton.grid(row=0,column=3)

        self.temps.listbox = tk.Listbox(self.temps,exportselection=False)
        self.temps.listbox.grid(row=1,column=0,columnspan=4,sticky=tk.NSEW)

        if setPlotAxes is not None:
            self.temps.listbox.bind('<<ListboxSelect>>',lambda event: setPlotAxes(None,event))
        else:
            self.temps.listbox.bind('<<ListboxSelect>>',lambda event: self.setPlotAxes(None,event))

        self.pane_ns.add(self.temps,weight=1)

        self.pane_ns.pack(expand=1,fill=tk.BOTH)

    def setPlotAxes(self,index=None,event=None):

        if not self.temps.listbox.curselection(): return

        if hasattr(self,"axes"):
            [self.figure.delaxes(axis) for axis in self.axes]

        self.axes = []

        if index is None:
            index = self.temps.listbox.curselection()[0]

        self.tempaxes[index]

        self.figure.set_tight_layout(True)

        self.canvas.draw()

    def setPlotLines(self,index=None,event=None):

        if not self.items.listbox.curselection(): return

        if not hasattr(self,"axes"):
            status = "No template has been selected."
            self.status.insert(tk.END,status)
            self.status.see(tk.END)
            return

        if hasattr(self,"lines"):
            for line in self.lines:
                line.remove()
                
        self.lines = []

        if index is None:
            index = self.temps.listbox.curselection()[0]

        self.templines[index]

        for axis in self.axes:
            axis.relim()
            axis.autoscale_view()

        self.figure.set_tight_layout(True)

        self.plot.draw()

    def addTemp(self):

        self.setTopTemplate()

    def editTemp(self):

        if not self.temps.listbox.curselection(): return
        
        name = self.temps.listbox.get(self.temps.listbox.curselection())

        item = self.temps.get("names").index(name)

        self.setTopTemplate(item=item)

    def delTemp(self):

        if not self.temps.listbox.curselection(): return

        name = self.temps.listbox.get(self.temps.listbox.curselection())

        item = self.temps.get("names").index(name)
        
        self.temps.listbox.delete(item)

        self.temps.get("names").pop(item)
        self.temps.get("xnumgrid").pop(item)
        self.temps.get("ynumgrid").pop(item)

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

        self.topTempNotebookTemplateFrame1.xnumgrid = ttk.Entry(self.topTempNotebookTemplateFrame1,width=10,validate="key",validatecommand=self.validateINT)
        self.topTempNotebookTemplateFrame1.xnumgrid.grid(row=0,column=1,sticky=tk.EW,padx=(10,2),pady=(20,2))

        self.topTempNotebookTemplateFrame1.ygridlabel = ttk.Label(self.topTempNotebookTemplateFrame1,text="Grids in X")
        self.topTempNotebookTemplateFrame1.ygridlabel.grid(row=1,column=0,sticky=tk.EW,padx=(10,10),pady=(2,2))

        self.topTempNotebookTemplateFrame1.ynumgrid = ttk.Entry(self.topTempNotebookTemplateFrame1,width=10,validate="key",validatecommand=self.validateINT)
        self.topTempNotebookTemplateFrame1.ynumgrid.grid(row=1,column=1,sticky=tk.EW,padx=(10,2),pady=(2,2))

        self.topTempNotebookTemplateFrame1.pack(side=tk.LEFT,expand=1,fill=tk.BOTH)
        
        self.topTempNotebook.add(self.topTempNotebookTemplate,text="Template Options",compound=tk.CENTER)

        self.topTempNotebookLine = tk.Frame(self.topTempNotebook)

        self.topTempNotebook.add(self.topTempNotebookLine,text="Line Options",compound=tk.CENTER)

        self.topTempNotebook.pack(side=tk.TOP,expand=1,fill=tk.BOTH,padx=(0,1))

        if item is not None:

            tempname = self.temps.get("names")[item]
            xnumgrid = self.temps.get("xnumgrid")[item]
            ynumgrid = self.temps.get("ynumgrid")[item]

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
            names = [name for index,name in enumerate(self.temps.get("names")) if index!=item]
        else:
            names = self.temps.get("names")

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
            self.temps.listbox.delete(item)
            self.temps.get("names").pop(item)
            self.temps.get("xnumgrid").pop(item)
            self.temps.get("ynumgrid").pop(item)
        
        self.temps.listbox.insert(item,name)

        self.temps.get("names").insert(item,name)

        try:
            xnumgrid = int(self.topTempNotebookTemplateFrame1.xnumgrid.get())
        except ValueError:
            xnumgrid = 1

        self.temps.get("xnumgrid").insert(item,xnumgrid)

        try:
            ynumgrid = int(self.topTempNotebookTemplateFrame1.ynumgrid.get())
        except ValueError:
            ynumgrid = 1

        self.temps.get("ynumgrid").insert(item,ynumgrid)

        # self.setPlotAxes(item)

        self.topTemplate.destroy()

if __name__ == "__main__":
    
    window = tk.Tk()

    gui = graph(window)

    window.mainloop()
