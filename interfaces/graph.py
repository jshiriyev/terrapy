import os
import sys

sys.path.append('C:\\Users\\javid.s\\Documents\\bhospy')

import tkinter as tk

from tkinter import ttk
from tkinter import filedialog

from ttkwidgets.autocomplete import AutocompleteEntryListbox

from matplotlib import pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg

from interfaces.data import manager

class graph(manager):

    def __init__(self,filepath=None,**kwargs):

        super().__init__(filepath,**kwargs)

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

        self.iconAdd = tk.PhotoImage(file=".\\graphics\\Add\\Add-9.png")
        self.iconEdt = tk.PhotoImage(file=".\\graphics\\Edit\\Edit-9.png")
        self.iconDel = tk.PhotoImage(file=".\\graphics\\Delete\\Delete-9.png")

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

    def setTopTemplate(self,buttonname="Add Template",item=None):

        if hasattr(self,"topTemplate"):
            if self.topTemplate.winfo_exists(): return

        self.topTemplate = tk.Toplevel()

        self.topTemplate.geometry("250x200")

        self.topTemplate.resizable(0,0)

        self.topTemplate.frame = tk.Frame(self.topTemplate,width=250,height=50)

        self.topTemplate.frame.rowconfigure(0,weight=1)
        self.topTemplate.frame.rowconfigure(1,weight=0)
        self.topTemplate.frame.rowconfigure(2,weight=0)
        self.topTemplate.frame.rowconfigure(3,weight=0)
        self.topTemplate.frame.rowconfigure(4,weight=0)
        self.topTemplate.frame.rowconfigure(5,weight=0)

        self.topTemplate.frame.columnconfigure(0,weight=1)
        self.topTemplate.frame.columnconfigure(1,weight=1)
        self.topTemplate.frame.columnconfigure(2,weight=0)
        self.topTemplate.frame.columnconfigure(3,weight=1)

        self.topTemplate.tempnameLabel = ttk.Label(self.topTemplate.frame,text="Template Name")
        self.topTemplate.tempnameLabel.grid(row=0,column=0,columnspan=4)

        self.topTemplate.tempname = ttk.Entry(self.topTemplate.frame)
        self.topTemplate.tempname.grid(row=1,column=0,columnspan=4,pady=(0,10),sticky=tk.EW)

        self.topTemplate.tempname.focus()

        self.topTemplate.separator0 = ttk.Separator(self.topTemplate.frame,orient=tk.HORIZONTAL)
        self.topTemplate.separator0.grid(row=2,column=0,columnspan=4,sticky=tk.EW)

        self.topTemplate.plotgrid = ttk.Label(self.topTemplate.frame,text="Grids")
        self.topTemplate.plotgrid.grid(row=3,column=0,sticky=tk.EW,padx=(0,10),pady=10)

        self.topTemplate.xnumgrid = ttk.Entry(self.topTemplate.frame,width=10,validate="key",validatecommand=self.integerVC)
        self.topTemplate.xnumgrid.grid(row=3,column=1,sticky=tk.EW,pady=10)

        self.topTemplate.cross = ttk.Label(self.topTemplate.frame,text="x")
        self.topTemplate.cross.grid(row=3,column=2,pady=10)

        self.topTemplate.ynumgrid = ttk.Entry(self.topTemplate.frame,width=10,validate="key",validatecommand=self.integerVC)
        self.topTemplate.ynumgrid.grid(row=3,column=3,sticky=tk.EW,pady=10)

        self.topTemplate.separator1 = ttk.Separator(self.topTemplate.frame,orient=tk.HORIZONTAL)
        self.topTemplate.separator1.grid(row=4,column=0,columnspan=4,sticky=tk.EW)

        if item is not None:

            tempname = self.temps.get("title")[item]
            xnumgrid = self.temps.get("xnumgrid")[item]
            ynumgrid = self.temps.get("ynumgrid")[item]

            self.topTemplate.tempname.insert(0,tempname)
            self.topTemplate.xnumgrid.insert(0,xnumgrid)
            self.topTemplate.ynumgrid.insert(0,ynumgrid)

        self.topTemplate.button = tk.Button(self.topTemplate.frame,text=buttonname,command=lambda: self.btnTopTempClicked(item))
        self.topTemplate.button.grid(row=5,column=0,columnspan=4,sticky=tk.EW,pady=10)

        self.topTemplate.button.bind('<Return>',lambda event: self.btnTopTempClicked(item,event))

        self.topTemplate.frame.place(relx=0.5,rely=0.5,anchor=tk.CENTER)

        self.topTemplate.mainloop()

    def btnTopTempClicked(self,item=None,event=None):

        if event is not None and event.widget!=self.topTemplate.button:
            return

        if item is None:
            item = len(self.temps.get("title"))
        else:
            self.tempListbox.delete(item)
            self.temps.get("title").pop(item)
            self.temps.get("xnumgrid").pop(item)
            self.temps.get("ynumgrid").pop(item)

        tempname = self.topTemplate.tempname.get()
        
        self.tempListbox.insert(item,tempname)

        self.temps.get("title").insert(item,tempname)

        try:
            xnumgrid = int(self.topTemplate.xnumgrid.get())
        except ValueError:
            xnumgrid = 1

        self.temps.get("xnumgrid").insert(item,xnumgrid)

        try:
            ynumgrid = int(self.topTemplate.ynumgrid.get())
        except ValueError:
            ynumgrid = 1

        self.temps.get("ynumgrid").insert(item,ynumgrid)

        self.setPlotArea(item)

        self.topTemplate.destroy()

    def addTemp(self):

        self.setTopTemplate(buttonname="Add Template")

    def editTemp(self):

        if not self.tempListbox.curselection(): return
        
        tempname = self.tempListbox.get(self.tempListbox.curselection())

        item = self.temps.get("title").index(tempname)

        self.setTopTemplate(buttonname="Edit Template",item=item)

    def delTemp(self):

        if not self.tempListbox.curselection(): return

        tempname = self.tempListbox.get(self.tempListbox.curselection())

        item = self.temps.get("title").index(tempname)
        
        self.tempListbox.delete(item)

        self.temps.get("title").pop(item)
        self.temps.get("xnumgrid").pop(item)
        self.temps.get("ynumgrid").pop(item)

    def set_template(self,event):

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
        
    def set_lines(self,event):

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
