import os

import tkinter as tk

from tkinter import ttk
from tkinter import filedialog

from ttkwidgets.autocomplete import AutocompleteEntryListbox

from matplotlib import pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg

if __name__ == "__main__":
    import setup

from interfaces.database import database

class graph(database):
    
    def __init__(self):

        super().__init__()

        self.dirname = os.path.dirname(__file__)

    def draw(self,window):

        self.root = window

        self.temps = {"title":[],"xnumgrid":[],"ynumgrid":[]}

        self.integerCB = lambda x: True if x.isdigit() or x == "" else False
        self.integerVC = (self.root.register(self.integerCB),'%P')

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

        self.searchbox = AutocompleteEntryListbox(self.side,height=250,padding=0)
        self.searchbox.config(completevalues=[],allow_other_values=False)

        self.pane_ns.add(self.searchbox,weight=1)

        self.template = ttk.Frame(self.side,height=200)

        self.template.rowconfigure(0,weight=0)
        self.template.rowconfigure(1,weight=1)

        self.template.columnconfigure(0,weight=1)
        self.template.columnconfigure(1,weight=0)
        self.template.columnconfigure(2,weight=0)

        self.tempLabel = ttk.Label(self.template,text="Templates")
        self.tempLabel.grid(row=0,column=0,sticky=tk.EW)

        self.iconAdd = tk.PhotoImage(file=self.dirname+"\\graphics\\Add\\Add-9.png")
        self.iconEdt = tk.PhotoImage(file=self.dirname+"\\graphics\\Edit\\Edit-9.png")
        self.iconDel = tk.PhotoImage(file=self.dirname+"\\graphics\\Delete\\Delete-9.png")

        self.buttonAdd = ttk.Button(self.template,image=self.iconAdd,command=self.addTemp)
        self.buttonAdd.grid(row=0,column=1)

        self.buttonEdt = ttk.Button(self.template,image=self.iconEdt,command=self.editTemp)
        self.buttonEdt.grid(row=0,column=2)

        self.buttonDel = ttk.Button(self.template,image=self.iconDel,command=self.delTemp)
        self.buttonDel.grid(row=0,column=3)

        self.tempListbox = tk.Listbox(self.template,exportselection=False)
        self.tempListbox.grid(row=1,column=0,columnspan=4,sticky=tk.NSEW)

        self.tempListbox.bind('<<ListboxSelect>>',lambda event: self.setPlotArea(None,event))

        self.pane_ns.add(self.template,weight=1)

        self.pane_ns.pack(expand=1,fill=tk.BOTH)

    def setPlotArea(self,item=None,event=None):

        if not self.tempListbox.curselection(): return

        if hasattr(self,"axes"):
            [self.figure.delaxes(axis) for axis in self.axes]

        self.axes = []

        if item is None:
            csel = self.tempListbox.curselection()
            name = self.tempListbox.get(csel)
            item = self.temps.get("title").index(name)

        xnumgrid = self.temps.get("xnumgrid")[item]
        ynumgrid = self.temps.get("ynumgrid")[item]

        for sub in range(xnumgrid*ynumgrid):
            self.axes.append(self.figure.add_subplot(xnumgrid,ynumgrid,sub+1))

        self.figure.set_tight_layout(True)

        self.canvas.draw()

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

        self.topTempNotebookTemplateFrame1.xnumgrid = ttk.Entry(self.topTempNotebookTemplateFrame1,width=10,validate="key",validatecommand=self.integerVC)
        self.topTempNotebookTemplateFrame1.xnumgrid.grid(row=0,column=1,sticky=tk.EW,padx=(10,2),pady=(20,2))

        self.topTempNotebookTemplateFrame1.ygridlabel = ttk.Label(self.topTempNotebookTemplateFrame1,text="Grids in X")
        self.topTempNotebookTemplateFrame1.ygridlabel.grid(row=1,column=0,sticky=tk.EW,padx=(10,10),pady=(2,2))

        self.topTempNotebookTemplateFrame1.ynumgrid = ttk.Entry(self.topTempNotebookTemplateFrame1,width=10,validate="key",validatecommand=self.integerVC)
        self.topTempNotebookTemplateFrame1.ynumgrid.grid(row=1,column=1,sticky=tk.EW,padx=(10,2),pady=(2,2))

        self.topTempNotebookTemplateFrame1.pack(side=tk.LEFT,expand=1,fill=tk.BOTH)
        
        self.topTempNotebook.add(self.topTempNotebookTemplate,text="Template Options",compound=tk.CENTER)

        self.topTempNotebookLine = tk.Frame(self.topTempNotebook)

        self.topTempNotebook.add(self.topTempNotebookLine,text="Line Options",compound=tk.CENTER)

        self.topTempNotebook.pack(side=tk.TOP,expand=1,fill=tk.BOTH,padx=(0,1))

        if item is not None:

            tempname = self.temps.get("title")[item]
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
            titles = [name for index,name in enumerate(self.temps.get("title")) if index!=item]
        else:
            titles = self.temps.get("title")

        title = self.topTempNotebookTemplateFrame0.tempname.get()

        if title in titles:
            tk.messagebox.showerror("Error","You have a template with the same name!",parent=self.topTemplate)
            return
        elif title.strip()=="":
            tk.messagebox.showerror("Error","You have not named the template!",parent=self.topTemplate)
            return

        if item is None:
            item = len(self.temps.get("title"))
        else:
            self.tempListbox.delete(item)
            self.temps.get("title").pop(item)
            self.temps.get("xnumgrid").pop(item)
            self.temps.get("ynumgrid").pop(item)
        
        self.tempListbox.insert(item,title)

        self.temps.get("title").insert(item,title)

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

        self.setPlotArea(item)

        self.topTemplate.destroy()

    def addTemp(self):

        self.setTopTemplate()

    def editTemp(self):

        if not self.tempListbox.curselection(): return
        
        tempname = self.tempListbox.get(self.tempListbox.curselection())

        item = self.temps.get("title").index(tempname)

        self.setTopTemplate(item=item)

    def delTemp(self):

        if not self.tempListbox.curselection(): return

        tempname = self.tempListbox.get(self.tempListbox.curselection())

        item = self.temps.get("title").index(tempname)
        
        self.tempListbox.delete(item)

        self.temps.get("title").pop(item)
        self.temps.get("xnumgrid").pop(item)
        self.temps.get("ynumgrid").pop(item)

class templateobject():

    def __init__(self):

        self.title = title
        self.xnumgrid = xnumgrid
        self.ynumgrid = ynumgrid

    def tempHistMatch(self,event):

        if not self.tempListbox.curselection(): return
        
        if hasattr(self,"axes"):
            [self.figure.delaxes(axis) for axis in self.axes]

        self.axes = []

        self.axes.append(self.figure.add_subplot(221))
        self.axes.append(self.axes[0].twinx())
        self.axes.append(self.figure.add_subplot(222))
        self.axes.append(self.axes[2].twinx())
        self.axes.append(self.figure.add_subplot(223))
        self.axes.append(self.axes[4].twinx())
        self.axes.append(self.figure.add_subplot(224))

        self.axes[0].set_xlabel("Date")
        self.axes[2].set_xlabel("Date")
        self.axes[4].set_xlabel("Date")
        self.axes[6].set_xlabel("Date")

        self.axes[0].set_ylabel("Liquid Rate, sm3/day")
        self.axes[2].set_ylabel("Gas Rate, th. sm3/day")
        self.axes[4].set_ylabel("Liquid Rate, sm3/day")
        self.axes[6].set_ylabel("Pressure, Bars")

        self.axes[1].set_ylabel("Liquid Volume, th. sm3")
        self.axes[3].set_ylabel("Surface Gas Volume, mln. sm3")
        self.axes[5].set_ylabel("Liquid Volume, th. sm3")

        self.axes[0].grid()
        self.axes[2].grid()
        self.axes[4].grid()
        self.axes[6].grid()

        self.figure.set_tight_layout(True)

        self.plot.draw()

        status = "Production history match template has been selected."
        self.status.insert(tk.END,status)
        self.status.see(tk.END)
        
    def tempHistMatchSetLines(self,event):

        if not self.listbox.curselection(): return

        if not hasattr(self,"axes"):
            status = "No template has been selected."    
            self.status.insert(tk.END,status)
            self.status.see(tk.END)
            return

        if hasattr(self,"lines"):
            for line in self.lines:
                line.remove()
                
        self.lines = []
        
        try: self.lines.append(self.axes[0].plot(data.date,data.Oil_Rate,c='k')[0])
        except: pass
        try: self.lines.append(self.axes[0].plot(data.date,data.Oil_Rate_H,'--',c='g')[0])
        except: pass
        try: self.lines.append(self.axes[1].plot(data.date,data.Oil_Total/1000,c='k')[0])
        except: pass
        try: self.lines.append(self.axes[1].plot(data.date,data.Oil_Total_H/1000,'--',c='g')[0])
        except: pass
        
        try: self.lines.append(self.axes[2].plot(data.date,data.Gas_Rate/1000,c='k')[0])
        except: pass
        try: self.lines.append(self.axes[2].plot(data.date,data.Gas_Rate_H/1000,'--',c='r')[0])
        except: pass
        try: self.lines.append(self.axes[3].plot(data.date,data.Gas_Total/1000000,c='k')[0])
        except: pass
        try: self.lines.append(self.axes[3].plot(data.date,data.Gas_Total_H/1000000,'--',c='r')[0])
        except: pass
        
        try: self.lines.append(self.axes[4].plot(data.date,data.Water_Rate,c='k')[0])
        except: pass
        try: self.lines.append(self.axes[4].plot(data.date,data.Water_Rate_H,'--',c='b')[0])
        except: pass
        try: self.lines.append(self.axes[5].plot(data.date,data.Water_Total/1000,c='k')[0])
        except: pass
        try: self.lines.append(self.axes[5].plot(data.date,data.Water_Total_H/1000,'--',c='b')[0])
        except: pass
        
        try: self.lines.append(self.axes[6].plot(data.date,data.Bottom_Hole_Pressure,c='k')[0])
        except: self.lines.append(self.axes[6].plot(data.date,data.Avg_Pressure,c='k')[0])
        try: self.lines.append(self.axes[6].plot(data.date,data.Bottom_Hole_Pressure_H,'--',c='m')[0])
        except: pass

        for axis in self.axes:
            axis.relim()
            axis.autoscale_view()

        self.figure.set_tight_layout(True)

        self.plot.draw()

if __name__ == "__main__":
    
    window = tk.Tk()

    gui = graph()

    gui.draw(window)

    window.mainloop()
