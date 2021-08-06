import csv
import datetime

from dateutil.parser import parse
from dateutil.relativedelta import relativedelta

import os
import re
import sys

import matplotlib.pyplot as plt

from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg

import numpy as np

import openpyxl

import sqlite3
from sqlite3 import Error as sqlError

import tkinter as tk

from tkinter import ttk
from tkinter import filedialog

class manager():

    def __init__(self,
        filepath=None,
        headers=None,
        skiplines=1,
        headerline=None,
        equalsize=True,
        min_row=0,
        min_col=0,
        max_row=None,
        max_col=None,
        sheetname=None,
        header_lowest="DATES",
        header_subs=None,
        endline="/",
        endfile="END"):

        self.filepath = filepath
        self.headers_ = headers

        if filepath is not None:

            self.filename = self.filepath.split("\\")[-1]
            self.extension = os.path.splitext(self.filepath)[1]
            
            if equalsize:
                self._read_equal(min_row,min_col,max_row,max_col,sheetname)
            else:
                self._read_unequal(header_lowest,header_subs,endline,endfile)

        elif headers is not None:

            self.running = [self.headers_]

        else:

            return

        self.num_rows = len(self.running)-skiplines
        self.num_cols = len(self.running[0])

        self.header_rows = self.running[:skiplines]

        if self.num_rows==0:
            self.body_cols = [[] for i in range(self.num_cols)]
        else:
            self.body_cols = np.array(self.running[skiplines:]).T.tolist()

        if skiplines==0:
            self.headers_ = ["col #"+str(idx) for idx in range(self.num_cols)]
        elif skiplines!=0:
            if headerline is None:
                linenumber = skiplines-1
            elif headerline < skiplines:
                linenumber = headerline
            else:
                linenumber = skiplines-1
            self.headers_ = self.running[linenumber]

        self.headers = []

        for header in self.headers_:

            header = header.strip()
            header = header.replace(" ","_")
            header = re.sub(r"[^\w]","",header)
            header = header.lower()
            header = "_"+header if header[0].isnumeric() else header

            self.headers.append(header)

    def _read_equal(self,min_row,min_col,max_row,max_col,sheetname):

        if self.extension == ".csv":
            with open(self.filepath,"r") as csv_text:
                self.running = list(csv.reader(csv_text))
            return

        if self.extension == ".db":
            self.conn = None
            try:
                self.conn = sqlite3.connect(self.filepath)
            except Error:
                print(Error)
            return

        if self.extension == ".inc":
            self.running = [["KEYWORDS","DETAILS","DATES"]]
            self.headers_ = headers
            self.header_subs = header_subs
            self._read_unequal(**kwargs)
            self.set_manager(skiplines=1)
            return

        if self.extension == ".txt":
            with open(self.filepath,"r") as textlines:
                self.running = []
                for line in textlines:
                    line = line.split('\n')[0].strip().split("\t")
                    while len(line)<max_col:
                        line.append("")
                    self.running.append(line[min_col:max_col+1])
            self.set_manager(**kwargs)

        if self.extension == ".xlsx":
            wb = openpyxl.load_workbook(self.filepath,read_only=True)
            lines = wb[sheetname].iter_rows(min_row=min_row+1,max_row=max_row,min_col=min_col+1,max_col=max_col,values_only=True)
            self.running = [list(line) for line in lines]
            wb._archive.close()
            # self.set_manager()
            return

    def _read_unequal(self,header_lowest,header_subs,endline,endfile):

        # It is important to note the hierarchy of the keywords and is important to specify
        # the lowest keyword in the line of succession e.g., header_lowest = "DATES"
        # While looping inside of the keyword, lines should end with end of line keyword e.g., endline = "/""
        # File must end with end of file keyword e.g., endfile = "END"

        flagContinueLoopFile = True

        with open(self.filepath,"r") as unequal_text:

            while flagContinueLoopFile:

                phrases = []

                flagContinueLoopHeaders = True

                while flagContinueLoopHeaders:

                    line = next(unequal_text)
                    line = line.split('\n')[0].strip()

                    key_phrase = line.split(" ")[0].strip()

                    flagContinueLoopHeadersSub = True

                    if any([key_phrase == keyword for keyword in self.headers_]):

                        while flagContinueLoopHeadersSub:

                            line = next(unequal_text)
                            line = line.split('\n')[0].strip()

                            sub_phrase = line.split(endline)[0].strip()

                            if key_phrase==header_lowest:
                                flagContinueLoopHeaders = False
                                for phrase in phrases:
                                    phrase.append(sub_phrase)
                                    self.running.append(phrase)
                                break

                            if sub_phrase == "":
                                flagContinueLoopHeadersSub = False
                            elif sub_phrase.split(" ")[0].strip() == self.header_subs:
                                phrases.append([key_phrase,sub_phrase])

                    elif key_phrase == endfile:

                        flagContinueLoopFile = False
                        break

    def get_column(self,header=None,idx=None):

        if idx is None:
            idx = self.headers.index(header)

        return self.body_cols[idx]

    def get_concatenated(self,*args,deliminator="_"):

        items = []

        for arg in args:
            items.append(getattr(self,arg))

        items = np.array(items,'U25')

        return [deliminator.join(row) for row in items.T]

    def todatetime(self,header="date",format=None):

        strings = self.get_column(header.lower())

        dates_list = []

        for string in strings:
            if format is None:
                dates_list.append(parse(string))
            else:
                dates_list.append(datetime.datetime.strptime(string,format))

        setattr(self,header.lower(),dates_list)

    def sortbased(self,header="date"):

        listA = self.get_column(header.lower())

        idx = list(range(len(listA)))

        zipped_lists = zip(listA,idx)
        sorted_pairs = sorted(zipped_lists)

        tuples = zip(*sorted_pairs)

        listA, idx = [ list(tuple) for tuple in  tuples]

        for header in self.headers:
            self.toarray(header)
            setattr(self,header,getattr(self,header)[idx])

    def toarray(self,header):

        list_of_string = getattr(self,header)

        try:
            try:
                array_of_string = np.array(list_of_string).astype('int')
            except:
                array_of_string = np.array(list_of_string).astype('float64')
        except:
            array_of_string = np.array(list_of_string)

        setattr(self,header,array_of_string)

    def _set_database_table(self,sqlite_table):

        # instructor_table = """ CREATE TABLE IF NOT EXISTS instructors (
        #                                     id integer PRIMARY KEY,
        #                                     first_name text NOT NULL,
        #                                     last_name text NOT NULL,
        #                                     patronym text NOT NULL,
        #                                     position text NOT NULL,
        #                                     status text NOT NULL,
        #                                     email text NOT NULL UNIQUE,
        #                                     phone integer NOT NULL UNIQUE);"""

        self.sqlite_table = sqlite_table

        try:
            self.cursor = self.conn.cursor()
            self.cursor.execute(self.sqlite_table)
            self.conn.commit()
        except Error:
            print(Error)

    def _insert_database_table(self,sqlite_table_insert,table_row):

        # instructor_table_insert = """ INSERT INTO instructors(
        #                                     id,
        #                                     first_name,
        #                                     last_name,
        #                                     patronym,
        #                                     position,
        #                                     status,
        #                                     email,
        #                                     phone)
        #                                     VALUES(?,?,?,?,?,?,?,?)"""

        self.sqlite_table_insert = sqlite_table_insert
        self.cursor.execute(self.sqlite_table_insert,table_row)
        self.conn.commit()

        """database manager"""

        # dbpath = r"C:\Users\Cavid\Documents\bhospy\interfaces\instructors.db"
        
        # DB = database_manager(dbpath)

        # DB.create_table(instructor_table)

        # instructor = (7,"Javid","Shiriyev","Farhad",
        #             "Senior Lecturer","Hour Based Teaching",
        #             "cavid.shiriyev@bhos.edu.az","+994508353992")

        # DB.insert_table(instructor_table_insert,instructor)
        # DB.cursor.close()
        # DB.conn.close()

    def write(self,filepath,lines,sheet_title=None):

        extension = os.path.splitext(filepath)[1]

        if extension == ".db":
            return

        if extension == ".csv":
            return

        if extension == ".xlsx":
            wb = openpyxl.Workbook()
            sheet = wb.active
            if sheet_title is not None:
                sheet.title = sheet_title
            for line in lines:
                sheet.append(line)
            wb.save(filepath)
            return

        if extension == ".inc":
            with open(filepath,"w") as writtenfile:
                for line in lines: writtenfile.write(line)
            return

class table(manager):

    def __init__(self,filepath=None,**kwargs):

        super().__init__(filepath,**kwargs)

    def draw(self,window,func=None):

        self.root = window

        self.scrollbar = tk.Scrollbar(self.root)

        self.columns = ["#"+str(idx) for idx,_ in enumerate(self.headers,start=1)]

        self.tree = ttk.Treeview(self.root,columns=self.columns,show="headings",selectmode="browse",yscrollcommand=self.scrollbar.set)

        for idx,(column,header) in enumerate(zip(self.columns,self.headers_),start=1):
            if idx<len(self.headers) :
                self.tree.column(column,anchor=tk.W,width=100)
            elif idx==len(self.headers):
                self.tree.column(column,anchor=tk.W)
            self.tree.heading(column,text=header,anchor=tk.W)

        self.tree.pack(side=tk.LEFT,expand=1,fill=tk.BOTH)

        self.scrollbar.pack(side=tk.LEFT,fill=tk.Y)

        self.scrollbar.config(command=self.tree.yview)

        self.added = []
        self.deleted = []

        self.frame = tk.Frame(self.root,width=50)
        self.frame.configure(background="white")
        self.frame.pack(side=tk.LEFT,fill=tk.Y)

        self.button_Add = tk.Button(self.frame,text="Add Item",width=50,command=self.addItem)
        self.button_Add.pack(side=tk.TOP,ipadx=5,padx=10,pady=(5,1))

        self.button_EditItem = tk.Button(self.frame,text="Edit Item",width=50,command=self.editItem)
        self.button_EditItem.pack(side=tk.TOP,ipadx=5,padx=10,pady=(1,1))

        self.button_Delete = tk.Button(self.frame,text="Delete Item",width=50,command=self.deleteItem)
        self.button_Delete.pack(side=tk.TOP,ipadx=5,padx=10,pady=(1,10))

        self.button_MoveUp = tk.Button(self.frame,text="Move Up",width=50,command=self.moveUp)
        self.button_MoveUp.pack(side=tk.TOP,ipadx=5,padx=10,pady=(10,1))

        self.button_MoveDown = tk.Button(self.frame,text="Move Down",width=50,command=self.moveDown)
        self.button_MoveDown.pack(side=tk.TOP,ipadx=5,padx=10,pady=(1,10))

        self.button_Save = tk.Button(self.frame,text="Save Changes",width=50,command=lambda: self.saveChanges(func))
        self.button_Save.pack(side=tk.TOP,ipadx=5,padx=10,pady=(10,1))

        # self.tree.column("#0",width=0,stretch=tk.NO)

        # for item in self.tree.get_children():
        #     self.tree.delete(item)

        for idx in range(self.num_rows):
            values = []
            for header in self.headers:
                values.append(self.get_col_by_header(header)[idx])
            self.tree.insert(parent="",index="end",iid=idx,values=tuple(values))

    def addItem(self):

        self.topAddItem = tk.Toplevel()

        for idx,(header,explicit) in enumerate(zip(self.headers,self.headers_)):
            label = "label_"+str(idx)
            entry = "entry_"+str(idx)
            pady = (30,5) if idx==0 else (5,5)
            setattr(self.topAddItem,label,tk.Label(self.topAddItem,text=explicit,font="Helvetica 11",width=20,anchor=tk.E))
            setattr(self.topAddItem,entry,tk.Entry(self.topAddItem,width=30,font="Helvetica 11"))
            getattr(self.topAddItem,label).grid(row=idx,column=0,ipady=5,padx=(10,5),pady=pady)
            getattr(self.topAddItem,entry).grid(row=idx,column=1,ipady=5,padx=(5,10),pady=pady)

        self.topAddItem.button = tk.Button(self.topAddItem,text="Add Item",command=self.addItemEnterClicked)
        self.topAddItem.button.grid(row=idx+1,column=0,columnspan=2,ipady=5,padx=15,pady=(15,30),sticky=tk.EW)

        self.topAddItem.button.bind('<Return>',self.addItemEnterClicked)

        self.topAddItem.mainloop()

        # for idx in self.tree.selection():
        #     print(self.tree.item(idx,'values'))

    def addItemEnterClicked(self,event=None):

        if event is not None and event.widget!=self.topAddItem.button:
            return

        values = []

        for idx,header in enumerate(self.headers):
            entry = "entry_"+str(idx)
            value = getattr(self.topAddItem,entry).get()
            self.get_column(header).append(value)
            values.append(value)

        self.tree.insert(parent="",index="end",iid=self.num_rows,values=values)

        self.num_rows += 1

        self.topAddItem.destroy()

    def editItem(self):

        if not self.tree.selection():
            return
        else:
            item = int(self.tree.selection()[0])

        self.topEditItem = tk.Toplevel()

        for idx,(header,explicit) in enumerate(zip(self.headers,self.headers_)):
            label = "label_"+str(idx)
            entry = "entry_"+str(idx)
            pady = (30,5) if idx==0 else (5,5)
            setattr(self.topEditItem,label,tk.Label(self.topEditItem,text=explicit,font="Helvetica 11",width=20,anchor=tk.E))
            setattr(self.topEditItem,entry,tk.Entry(self.topEditItem,width=30,font="Helvetica 11"))
            getattr(self.topEditItem,label).grid(row=idx,column=0,ipady=5,padx=(10,5),pady=pady)
            getattr(self.topEditItem,entry).grid(row=idx,column=1,ipady=5,padx=(5,10),pady=pady)
            getattr(self.topEditItem,entry).insert(0,self.get_column(header)[item])

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
            self.get_column(header)[item] = value
            values.append(value)

        self.tree.item(item,values=values)

        self.topEditItem.destroy()

    def deleteItem(self):

        for item in self.tree.selection():
            index = int(item)
            self.tree.delete(index)
            self.deleted.append(index)

    def moveUp(self):

        if not self.tree.selection():
            return
        else:
            item = self.tree.selection()[0]

        self.tree.move(item,self.tree.parent(item),self.tree.index(item)-1)

    def moveDown(self):

        if not self.tree.selection():
            return
        else:
            item = self.tree.selection()[0]

        self.tree.move(item,self.tree.parent(item),self.tree.index(item)+1)

    def saveChanges(self,func=None):

        self.body_rows = []

        for header in self.headers:
            setattr(self,header,[])
        
        # self.body_rows = np.delete(self.body_rows,self.deleted,axis=0)
        for child in self.tree.get_children():
            line = self.tree.item(child)["values"]
            self.body_rows.append(line)
            for idx,header in enumerate(self.headers):
                getattr(self,header).append(line[idx])

        if func is not None:
            func()

        self.root.destroy()

        # save_to_database:
        # self.conn = sqlite3.connect(self.filepath)
        # # sqlite_table = '''CREATE TABLE SqliteDb_developers (
        # #                  id INTEGER PRIMARY KEY,
        # #                  name TEXT NOT NULL,
        # #                  email text NOT NULL UNIQUE);'''
        # self.cursor = self.conn.cursor()
        # # self.cursor.execute(sqlite_table)
        # # self.conn.commit()
        # self.cursor.close()
        # self.conn.close()

    def autoColumnWidth(self):
        
        for columnNumber in range(self.model.columnCount()):
            self.resizeColumnToContents(columnNumber)

class graph(manager):

    def __init__(self,filepath=None,**kwargs):

        super().__init__(filepath,**kwargs)

    def draw(self,window):

        self.root = window

        self.frame_navigator = tk.Frame(self.root,width=300,height=200)
        self.frame_navigator.configure(background="white")

        tk.Grid.rowconfigure(self.frame_navigator,0,weight=1)
        tk.Grid.rowconfigure(self.frame_navigator,2,weight=1)
        tk.Grid.columnconfigure(self.frame_navigator,0,weight=1)

        self.listbox = tk.Listbox(self.frame_navigator,width=10,height=30,exportselection=False)
        self.listbox.grid(row=0,column=0,sticky=tk.NSEW)
        # self.listbox.bind('<<ListboxSelect>>',self.get_sheet_data)

        self.label_template = tk.Label(self.frame_navigator,text="Plot Templates")
        self.label_template.grid(row=1,column=0,sticky=tk.EW)

        self.listbox_template = tk.Listbox(self.frame_navigator,exportselection=False)
        self.listbox_template.grid(row=2,column=0,sticky=tk.NSEW)
        self.listbox_template.insert(tk.END,"Production History Match")
        self.listbox_template.bind('<<ListboxSelect>>',self.set_template)

        # self.button = tk.Button(self.frame_navigator,text="Set Plot Template",command=self.set_template)
        # self.button.grid(row=1,column=0,sticky=tk.NSEW)

        self.frame_monitor = tk.Frame(self.root,width=300,height=200)
        self.frame_monitor.configure(background="white")

        tk.Grid.rowconfigure(self.frame_monitor,0,weight=1)
        tk.Grid.columnconfigure(self.frame_monitor,0,weight=1)
        
        self.figure = plt.Figure()
        
        self.plot = FigureCanvasTkAgg(self.figure,self.frame_monitor)
        self.plot_widget = self.plot.get_tk_widget()
        self.plot_widget.grid(row=0,column=0,sticky=tk.NSEW)
        
        self.status = tk.Listbox(self.frame_monitor,width=250,height=5)
        self.status.grid(row=1,column=0,sticky=tk.EW)

        self.frame_navigator.pack(side=tk.LEFT,expand=1,fill=tk.BOTH)
        self.frame_monitor.pack(side=tk.LEFT,expand=1,fill=tk.BOTH)

    def set_template(self,event):

        if not self.listbox_template.curselection(): return
        
        if hasattr(self,"axes"):
            for axis in self.axes:
                self.figure.delaxes(axis)

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

    # gui = table("instructors.csv")
    gui = table(headers=["Full Name","Position","Contact"])

    gui.draw(window)

    window.mainloop()
