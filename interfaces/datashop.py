import csv
import datetime

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

class data_manager():

    def __init__(self,filepath=None,*args,**kwargs):

        if filepath is None: return

        self.filepath = filepath
        self.filename = self.filepath.split("\\")[-1]

        self.extension = os.path.splitext(self.filepath)[1]

        if self.extension == ".db":
            self.conn = None
            try:
                self.conn = sqlite3.connect(self.filepath)
            except Error:
                print(Error)
        elif self.extension == ".csv":
            with open(self.filepath,"r") as csv_text:
                self.running = list(csv.reader(csv_text))
            self.read_lines(skiplines=1)
        elif self.extension == ".xlsx":
            self.wb = openpyxl.load_workbook(self.filepath)
            # edited_sheetname = re.sub(r"[^\w]","",sheetname)
            sheetname = kwargs["sheetname"]
            lines = self.wb[sheetname].iter_rows(values_only=True)
            self.running = [list(line) for line in lines]
            self.read_lines(skiplines=1)
        else:
            with open(self.filepath,"r") as structured_text:
                self.running = [line.split("\n")[0].split("\t") for line in structured_text.readlines()]
            self.read_lines(skiplines=1,**kwargs)

    def read_lines(self,skiplines=0,headerline=None,type="structured"):

        """
        type can be structured
        type can be unstructured
        """

        self.skiplines = skiplines

        self.body_rows = self.running[self.skiplines:]
        self.body_rows = np.array(self.body_rows)

        self.num_cols = self.body_rows.shape[1]

        if self.skiplines==0:

            self.header_rows = None
            self.headers = ["col_"+str(column_id) for column_id in range(self.num_cols)]
        
        elif self.skiplines!=0:

            if headerline is None:
                linenumber = self.skiplines-1
            elif headerline < self.skiplines:
                linenumber = headerline
            else:
                linenumber = self.skiplines-1

            self.header_rows = self.running[:self.skiplines]
            self.headers = self.running[linenumber]

            for idx,header in enumerate(self.headers):
                header = header.replace(" ","_").lower()
                header = re.sub(r"[^\w]","",header)
                header = "_"+header if header[0].isnumeric() else header
                self.headers[idx] = header
            
        for column_id,header in enumerate(self.headers):
            setattr(self,header,self.body_rows[:,column_id].tolist())

    def todatetime(self,attr_name="date",date_format='%m/%d/%Y'):

        dates_string = getattr(self,attr_name)

        dates = []

        for date_string in dates_string:
            dates.append(datetime.datetime.strptime(date_string,date_format))

        setattr(self,attr_name,dates)

    def sortbased(self,attr_name="date"):

        listA = getattr(self,attr_name)

        idx = list(range(len(listA)))

        zipped_lists = zip(listA,idx)
        sorted_pairs = sorted(zipped_lists)

        tuples = zip(*sorted_pairs)

        listA, idx = [ list(tuple) for tuple in  tuples]

        for header in self.header:
            self.toarray(header)
            setattr(self,header,getattr(self,header)[idx])

    def toarray(self,attr_name):

        list_of_string = getattr(self,attr_name)

        try:
            try:
                array_of_string = np.array(list_of_string).astype('int')
            except:
                array_of_string = np.array(list_of_string).astype('float64')
        except:
            array_of_string = np.array(list_of_string)

        setattr(self,attr_name,array_of_string)

    def set_fullName(self):

        self.full_name = list(" ".join((first,last)) for first,last in zip(self.first_name,self.last_name))

    def db_create_table(self,sqlite_table):

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

    def db_insert_table(self,sqlite_table_insert,table_row):

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

    def df_write(self,outputfile,date_format='%m/%d/%Y'):

        self.outputfile = outputfile

        def create_new_line(idx):
            line = []
            for k,header in enumerate(self.header):
                if k==0:
                    line.append(self.WellName[idx-1])
                elif k==1:
                    line.append((self.Date[idx-1]+relativedelta(months=1)).strftime(date_format))
                else:
                    line.append(0)
            return "\t".join(np.array(line))+"\n"

        def skiptoline(file_read,keyword,file_written=None):

            while True:
                
                line = next(file_read)

                if file_written is not None:
                    file_written.write(line)
                
                if line.split('/')[0].strip() == keyword:
                    break
            
        with open(self.outputfile,"w") as empty_file:

            for linenumber,row in enumerate(self.running):
                index = linenumber-self.skiplines
                # if the time gap with previous line is more than a month
                cond1 = (self.Date[index]-self.Date[index-1]).days>31
                # if the well name is different compared to previous line
                cond2 = self.WellName[index]==self.WellName[index-1]
                # if the date corresponds to previous month
                cond3 = (datetime.datetime.today()-self.Date[index-1]).days<30
                if index<=0: continue
                if cond1 and cond2:
                    empty_file.write(create_new_row(idx))
                if not cond2 and not cond3:
                    empty_file.write(create_new_row(idx))

                empty_file.write(row)

        # with open(self.outputfile,"w") as writtenfile:
        #     for i,date in enumerate(rdate):
        #        skiptoline(rewrittenfile,'DATES',writtenfile)
        #        skiptoline(rewrittenfile,rdate[i],writtenfile)
        #        skiptoline(rewrittenfile,'WCONHIST',writtenfile)
        #        while True:
        #            wline = next(rewrittenfile)
        #            if wline.split('/')[0].strip().split(' ')[0] == '\'BHR_76\'':
        #                notypewritten.write(rcopy[i])
        #                break
        #            else:
        #                notypewritten.write(wline)
        #     while True:
        #        try:
        #            copiedline = next(rewrittenfile)
        #            writtenfile.write(copiedline)
        #        except:
        #            break

class data_table():

    def __init__(self,window,headers):

        self.root = window

        self.headers_explicit = headers

        self.headers = []

        for header in self.headers_explicit:
            header = header.replace(" ","_").lower()
            header = re.sub(r"[^\w]","",header)
            header = "_"+header if header[0].isnumeric() else header
            self.headers.append(header)
        
        self.columns = ["#"+str(idx+1) for idx in range(len(self.headers))]

        self.initialize()

    def initialize(self):

        self.tree = ttk.Treeview(self.root,columns=self.columns,show="headings",selectmode="browse")

        for idx,(column,header) in enumerate(zip(self.columns,self.headers_explicit),start=1):
            if idx<len(self.headers) :
                self.tree.column(column,anchor=tk.W,width=100)
            elif idx==len(self.headers):
                self.tree.column(column,anchor=tk.W)
            self.tree.heading(column,text=header,anchor=tk.W)

        self.tree.pack(side=tk.LEFT,expand=1,fill=tk.BOTH)

        for header in self.headers:
            setattr(self,header,[])

        self.added = []
        self.deleted = []

        self.frame = tk.Frame(self.root,width=50)
        self.frame.configure(background="white")
        self.frame.pack(side=tk.TOP)

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

        self.button_Save = tk.Button(self.frame,text="Save Changes",width=50,command=self.saveChanges)
        self.button_Save.pack(side=tk.TOP,ipadx=5,padx=10,pady=(10,1))

    def set_path(self):

        self.filepath = filedialog.askopenfilename(
            title = "Select a File",
            initialdir = os.getcwd(),
            filetypes = (("Databases","*.db"),
                         ("CSV Files","*.csv"),
                         ("Excel Files","*.xl*"),
                         ("All Files","*")))

        if not self.filepath: return

        self.data = data_manager(self.filepath)

        self.data.set_fullName()

        self.set_tree_view()
        
        # self.wb = openpyxl.load_workbook(self.filepath)

        # for sheet in self.wb.sheetnames:
        #     self.listbox.insert(tk.END,sheet)

        # status = "Imported \""+self.filepath+"\"."
        # self.status.insert(tk.END,status)
        # self.status.see(tk.END)

    def set_tree_view(self):

        # self.tree.column("#0",width=0,stretch=tk.NO)

        for item in self.tree.get_children():
            self.tree.delete(item)

        for idx, _ in enumerate(self.data.body_rows):
            values = (self.data.full_name[idx],self.data.position[idx],self.data.email[idx])
            self.tree.insert(parent="",index="end",iid=idx,values=values)

        for header in self.headers:
            setattr(self,header,getattr(self.data,header))

        self.deleted = []

    def addItem(self):

        self.topAddItem = tk.Toplevel()

        if hasattr(self,"data"):
            headers = self.data.headers
        else:
            headers = self.headers_explicit

        for idx,header in enumerate(headers):
            label = "label_"+str(idx)
            entry = "entry_"+str(idx)
            pady = (30,5) if idx==0 else (5,5)
            setattr(self.topAddItem,label,tk.Label(self.topAddItem,text=header,font="Helvetica 11",width=20,anchor=tk.E))
            setattr(self.topAddItem,entry,tk.Entry(self.topAddItem,width=30,font="Helvetica 11"))
            getattr(self.topAddItem,label).grid(row=idx,column=0,ipady=5,padx=(10,5),pady=pady)
            getattr(self.topAddItem,entry).grid(row=idx,column=1,ipady=5,padx=(5,10),pady=pady)

        self.topAddItem.button = tk.Button(self.topAddItem,text="Add Item",command=self.addItemEnterClicked)
        self.topAddItem.button.grid(row=idx+1,column=0,columnspan=2,ipady=5,padx=15,pady=(15,30),sticky=tk.EW)

        self.topAddItem.bind('<Return>',self.addItemEnterClicked)

        self.topAddItem.mainloop()

        # for idx in self.tree.selection():
        #     print(self.tree.item(idx,'values'))

    def addItemEnterClicked(self,event=None):

        if event is not None:
            if event.widget!=self.topAddItem.button:
                return

        iid = len(getattr(self,self.headers[0]))

        if hasattr(self,"data"):
            headers = self.data.headers
        else:
            headers = self.headers
        
        row = data_manager()

        for idx,header in enumerate(headers):
            entry = "entry_"+str(idx)
            value = getattr(self.topAddItem,entry).get()
            setattr(row,header,[value])
            if hasattr(self,"data"):
                getattr(self.data,header).append(value)

        if hasattr(self,"data"):
            row.set_fullName()

        values = []

        for header in self.headers:
            value = getattr(row,header)[0]
            getattr(self,header).append(value)
            values.append(value)

        self.tree.insert(parent="",index="end",iid=iid,values=values)

        self.topAddItem.destroy()

    def editItem(self):

        if not self.tree.selection():
            return
        else:
            item = int(self.tree.selection()[0])

        # print(item)
        # print(self.tree.index(item))

        self.topEditItem = tk.Toplevel()

        if hasattr(self,"data"):
            headers = self.data.headers
        else:
            headers = self.headers

        for idx,header in enumerate(headers):
            label = "label_"+str(idx)
            entry = "entry_"+str(idx)
            pady = (30,5) if idx==0 else (5,5)
            setattr(self.topEditItem,label,tk.Label(self.topEditItem,text=self.headers_explicit[idx],font="Helvetica 11",width=20,anchor=tk.E))
            setattr(self.topEditItem,entry,tk.Entry(self.topEditItem,width=30,font="Helvetica 11"))
            getattr(self.topEditItem,label).grid(row=idx,column=0,ipady=5,padx=(10,5),pady=pady)
            getattr(self.topEditItem,entry).grid(row=idx,column=1,ipady=5,padx=(5,10),pady=pady)
            if hasattr(self,"data"):
                entry_text = getattr(self.data,header)[item]
            else:
                entry_text = getattr(self,header)[item]
            getattr(self.topEditItem,entry).insert(0,entry_text)

        self.topEditItem.button = tk.Button(self.topEditItem,text="Save Item Edit",command=lambda: self.editItemEnterClicked(item))
        self.topEditItem.button.grid(row=idx+1,column=0,columnspan=2,ipady=5,padx=15,pady=(15,30),sticky=tk.EW)

        # self.topEditItem.bind('<Return>',self.editItemEnterClicked)

        self.topEditItem.mainloop()

    def editItemEnterClicked(self,item,event=None):
        
        if hasattr(self,"data"):
            headers = self.data.headers
        else:
            headers = self.headers
        
        row = data_manager()

        for idx,header in enumerate(headers):
            entry = "entry_"+str(idx)
            value = getattr(self.topEditItem,entry).get()
            setattr(row,header,[value])
            if hasattr(self,"data"):
                getattr(self.data,header)[item] = value

        if hasattr(self,"data"):
            row.set_fullName()

        values = []

        for header in self.headers:
            value = getattr(row,header)[0]
            getattr(self,header)[item] = value
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

    def saveChanges(self):
        
        print(len(self.full_name))
        print(self.full_name)
        # if hasattr(self,"data"):
        #     print(len(self.data.full_name))
        #     print(self.data.full_name)
        print(self.deleted)

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

    def closeEvent(self,event):
        
        if self.treeView.editedFlag:
            choice = QtWidgets.QMessageBox.question(self,'Quit',
                "Do you want to save changes?",QtWidgets.QMessageBox.Yes | 
                QtWidgets.QMessageBox.No)

            if choice == QtWidgets.QMessageBox.Yes:
                self.treeView.writeCSV(self.filepath)

        event.accept()

    def mouseDoubleClickEvent(self,event):

        index = self.indexAt(event.pos())
        #print(index.row(),index.column())

        #item = self.model.item(index.row(),index.column())
        #print(item.data(QtCore.Qt.DisplayRole))
        #item.setEditable(True)

        if index.row()>-1 and index.column()>-1:
            self.edit(index)

    def autoColumnWidth(self):
        
        for columnNumber in range(self.model.columnCount()):
            self.resizeColumnToContents(columnNumber)

class data_graph():

    def __init__(self,window):

        self.root = window

        self.initialize()

    def initialize(self):

        self.frame_navigator = tk.Frame(self.root,width=300,height=200)
        self.frame_navigator.configure(background="white")

        tk.Grid.rowconfigure(self.frame_navigator,0,weight=1)
        tk.Grid.rowconfigure(self.frame_navigator,2,weight=1)
        tk.Grid.columnconfigure(self.frame_navigator,0,weight=1)

        self.listbox = tk.Listbox(self.frame_navigator,width=10,height=30,exportselection=False)
        self.listbox.grid(row=0,column=0,sticky=tk.NSEW)
        self.listbox.bind('<<ListboxSelect>>',self.get_sheet_data)

        self.label_template = tk.Label(self.frame_navigator,text="Plot Templates")
        self.label_template.grid(row=1,column=0,sticky=tk.EW)

        self.listbox_template = tk.Listbox(self.frame_navigator,exportselection=False)
        self.listbox_template.grid(row=2,column=0,sticky=tk.NSEW)
        self.listbox_template.insert(tk.END,"Production History Match")
        self.listbox_template.bind('<<ListboxSelect>>',self.set_figure_template)

        # self.button = tk.Button(self.frame_navigator,text="Set Plot Template",command=self.set_figure_template)
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

    def set_path(self):

        self.filepath = filedialog.askopenfilename(
            title = "Select a File",
            initialdir = os.getcwd(),
            filetypes = (("Excel files","*.xl*"),("All files","*.*")))

        if not self.filepath: return
        
        self.wb = openpyxl.load_workbook(self.filepath)

        for sheet in self.wb.sheetnames:
            self.listbox.insert(tk.END,sheet)

        status = "Imported \""+self.filepath+"\"."
        self.status.insert(tk.END,status)
        self.status.see(tk.END)

    def set_figure_template(self,event):

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

    def get_sheet_data(self,event):

        if not self.listbox.curselection(): return
        
        class data: pass
                
        data.ws = self.wb[self.listbox.get(self.listbox.curselection())]
        
        data.date = list(data.ws.iter_cols(min_row=2,max_col=1,values_only=True))[0]
        
        Head = list(data.ws.iter_rows(max_row=1,min_col=2,values_only=True))[0]
        Body = list(data.ws.iter_cols(min_row=2,min_col=2,values_only=True))

        for i,head in enumerate(Head):
            string = head.split(":")[1].split(",")[0].strip().replace(" ","_")
            string = string.replace("(","").replace(")","").replace(".","")
            setattr(data,string,np.array(Body[i]))
        
        self.set_figure_data(data)
        
    def set_figure_data(self,data):

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

    data = data_manager("courses.xlsx",sheetname="courses")
    
    # window = tk.Tk()
    # window.configure(background="white")

    # # window.geometry("500x500")

    # menubar = tk.Menu(window)

    # window.config(menu=menubar)

    # gui = data_table(window,("Full Name","Position","Email"))
    # # gui = data_graph(window)

    # fileMenu = tk.Menu(menubar,tearoff="Off")
    # fileMenu.add_command(label="Open",command=gui.set_path)

    # menubar.add_cascade(label="File",menu=fileMenu)

    # window.mainloop()
