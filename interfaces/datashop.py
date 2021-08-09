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
        equalsize=True,
        skiplines=1,
        headerline=None,
        min_row=0,
        min_col=0,
        max_row=None,
        max_col=None,
        sheetname=None,
        comment="--",
        deliminator="\t",
        endline="/",
        endfile="END"):

        self.filepath = filepath
        self.headers_ = headers
        self.equalsize = equalsize
        self.skiplines = skiplines

        if headerline is None:
            self.headerline = skiplines-1
        elif headerline<skiplines:
            self.headerline = headerline
        else:
            self.headerline = skiplines-1

        self.min_row = min_row
        self.min_col = min_col
        self.max_row = max_row if max_row is None else max_row+1
        self.max_col = max_col if max_col is None else max_col+1
        self.sheetname = sheetname
        self.comment = comment
        self.deliminator = deliminator
        self.endline = endline
        self.endfile = endfile

        if filepath is not None:
            self.filename = os.path.split(self.filepath)[1]
            self.extension = os.path.splitext(self.filepath)[1]
            
            if self.equalsize:
                self._read_equal()
            else:
                self._read_unequal()

        elif headers is not None:
            self.running = [self.headers_]

        else:
            return

        self.num_rows = len(self.running)-self.skiplines
        self.num_cols = len(self.running[0])

        self.header_rows = self.running[:self.skiplines]

        if self.num_rows==0:
            self.body_cols = [[] for i in range(self.num_cols)]
        else:
            self.body_cols = np.array(self.running[self.skiplines:]).T.tolist()

        if self.skiplines==0:
            self.headers_ = ["col #"+str(idx) for idx in range(self.num_cols)]
        elif skiplines!=0:
            self.headers_ = self.running[self.headerline]

        self.headers = []

        for header in self.headers_:

            header = header.strip()
            header = header.replace(" ","_")
            header = re.sub(r"[^\w]","",header)
            header = header.lower()
            header = "_"+header if header[0].isnumeric() else header

            self.headers.append(header)

    def _read_equal(self):

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

        if self.extension == ".txt":
            self.running = []
            with open(self.filepath,"r") as textlines:
                for line in textlines:
                    line = self._texttocolumn(line)
                    self.running.append(line)
            return

        if self.extension == ".xlsx":
            wb = openpyxl.load_workbook(self.filepath,read_only=True)
            lines = wb[sheetname].iter_rows(min_row=self.min_row+1,max_row=self.max_row,min_col=self.min_col+1,max_col=self.max_col,values_only=True)
            self.running = [list(line) for line in lines]
            wb._archive.close()
            # self.set_manager()
            return

    def _read_unequal(self):

        # While looping inside of the header, lines should end with end of line keyword e.g., endline = "/""
        # File must end with end of file keyword e.g., endfile = "END"

        self.running = []

        if len(self.headers_)==1:
            self.equalsize = True

        flagContinueLoopFile = True

        with open(self.filepath,"r") as unequal_text:

            while flagContinueLoopFile:

                line = next(unequal_text)
                line = line.split('\n')[0].strip()

                header_phrase = line.split(" ")[0].strip()

                if any([header_phrase == header for header in self.headers_]):

                    flagContinueLoopHeadersSub = True

                    while flagContinueLoopHeadersSub:

                        line = next(unequal_text)
                        line = line.split('\n')[0].strip()

                        sub_header_phrase = line.split(self.endline)[0].strip()

                        if sub_header_phrase == "":
                            flagContinueLoopHeadersSub = False
                        else:
                            if sub_header_phrase[:2]!=self.comment:
                                if self.equalsize:
                                    row = self._texttocolumn(sub_header_phrase)
                                else:
                                    row = [sub_header_phrase]
                                row.insert(0,header_phrase)
                                self.running.append(row)

                elif header_phrase == self.endfile:

                    flagContinueLoopFile = False
                    break

    def _texttocolumn(self,line):

        line = line.split('\n')[0].strip()
        line = re.sub(' +',' ',line)
        line = line.split(self.deliminator)

        while len(line)<self.max_col:
            line.append("")

        return line[self.min_col:self.max_col]

    def get_column(self,header=None,idx=None):

        if idx is None:
            idx = self.headers.index(header)

        return self.body_cols[idx]

    def get_concatenated(self,*args,deliminator="_"):

        items = []

        for arg in args:
            items.append(self.get_column(arg))

        items = np.array(items,'U25')

        return [deliminator.join(row) for row in items.T]

    def todatetime(self,header="date",idx=None,format=None):

        if idx is None:
            idx = self.headers.index(header)

        dates_list = []

        for string in self.body_cols[idx]:
            if format is None:
                dates_list.append(parse(string))
            else:
                dates_list.append(datetime.datetime.strptime(string,format))

        self.body_cols[idx] = dates_list

    def toarray(self,header=None,idx=None):

        if idx is None:
            idx = self.headers.index(header)

        try:
            try:
                self.body_cols[idx] = np.array(self.body_cols[idx]).astype('int')
            except:
                self.body_cols[idx] = np.array(self.body_cols[idx]).astype('float64')
        except:
            self.body_cols[idx] = np.array(self.body_cols[idx])

    # def sortbased(self,header="date"):

    #     listA = self.get_column(header.lower())

    #     idx = list(range(len(listA)))

    #     zipped_lists = zip(listA,idx)
    #     sorted_pairs = sorted(zipped_lists)

    #     tuples = zip(*sorted_pairs)

    #     listA, idx = [ list(tuple) for tuple in  tuples]

    #     for header in self.headers:
    #         self.toarray(header)
    #         setattr(self,header,getattr(self,header)[idx])

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

    def write(self,filepath,sheet_title=None):

        running = [self.headers]+np.array(self.body_cols).astype(str).T.tolist()

        extension = os.path.splitext(filepath)[1]

        if extension == ".bhos":
            with open(filepath,"w",encoding='utf-8') as writtenfile:
                for line in running:
                    writtenfile.write("\t".join(line)+"\t/\n")
                writtenfile.write("/\n")
            return

        if extension == ".csv":
            return

        if extension == ".db":
            return

        if extension == ".inc":
            with open(filepath,"w") as writtenfile:
                for line in running:
                    writtenfile.write("\t".join(line)+"\n")
            return

        if extension == ".xlsx":
            wb = openpyxl.Workbook()
            sheet = wb.active
            if sheet_title is not None:
                sheet.title = sheet_title
            for line in running:
                sheet.append(line)
            wb.save(filepath)
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
            self.tree.heading(column,text=header,anchor=tk.W,command=lambda _col = column: self.sort_column(_col))

        self.tree.pack(side=tk.LEFT,expand=1,fill=tk.BOTH)

        self.scrollbar.pack(side=tk.LEFT,fill=tk.Y)

        self.scrollbar.config(command=self.tree.yview)

        self.editedFlag = False

        self.added = []
        self.edited = []
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

        body_rows = np.array(self.body_cols).T.tolist()

        for idx in range(self.num_rows):
            values = body_rows[idx]
            self.tree.insert(parent="",index="end",values=tuple(values))

    def addItem(self):

        if hasattr(self,"topAddItem"):
            if self.topAddItem.winfo_exists():
                return

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
            values.append(value)

        self.editedFlag = True

        self.added.append(values)

        self.tree.insert(parent="",index="end",values=values)

        self.num_rows += 1

        self.topAddItem.destroy()

    def editItem(self):

        if not self.tree.selection():
            return
        else:
            item = self.tree.selection()[0]

        values = self.tree.item(item)['values']

        self.topEditItem = tk.Toplevel()

        for idx,(header,explicit) in enumerate(zip(self.headers,self.headers_)):
            label = "label_"+str(idx)
            entry = "entry_"+str(idx)
            pady = (30,5) if idx==0 else (5,5)
            setattr(self.topEditItem,label,tk.Label(self.topEditItem,text=explicit,font="Helvetica 11",width=20,anchor=tk.E))
            setattr(self.topEditItem,entry,tk.Entry(self.topEditItem,width=30,font="Helvetica 11"))
            getattr(self.topEditItem,label).grid(row=idx,column=0,ipady=5,padx=(10,5),pady=pady)
            getattr(self.topEditItem,entry).grid(row=idx,column=1,ipady=5,padx=(5,10),pady=pady)
            getattr(self.topEditItem,entry).insert(0,values[idx])

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

        self.editedFlag = True

        self.edited.append([self.tree.item(item)['values'],values])

        self.tree.item(item,values=values)

        self.topEditItem.destroy()

    def deleteItem(self):

        if not self.tree.selection():
            return
        else:
            item = self.tree.selection()[0]

        self.deleted.append(self.tree.item(item)['values'])
        self.num_rows -= 1

        self.tree.delete(item)

        self.editedFlag = True

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

        if not self.editedFlag:
            self.root.destroy()
            return

        body_rows = np.array(self.body_cols).T.tolist()

        for values in self.added:
            body_rows.append(values)

        self.added = []

        for row in self.edited:
            body_rows.remove(row[0])
            body_rows.append(row[1])

        self.edited = []

        for values in self.deleted:
            body_rows.remove(values)

        self.deleted = []

        self.body_cols = np.array(body_rows).T.tolist()

        if func is not None:
            func()

        self.editedFlag = False

        self.root.destroy()

    def autoColumnWidth(self):
        
        for columnNumber in range(self.model.columnCount()):
            self.resizeColumnToContents(columnNumber)

    def sort_column(self,column):

        col_and_idx = [(self.tree.set(k,column), k) for k in self.tree.get_children('')]

        col_and_idx.sort()

        for index, (_,k) in enumerate(col_and_idx):
            self.tree.move(k,'',index)

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
