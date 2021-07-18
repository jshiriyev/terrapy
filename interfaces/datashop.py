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

    def __init__(self,filepath,*args,**kwargs):

        self.path = filepath
        self.name = self.path.split("\\")[-1]

        self.extension = os.path.splitext(self.path)[1]

        if self.extension == ".db":
            self.conn = None
            try:
                self.conn = sqlite3.connect(self.path)
            except Error:
                print(Error)
        elif self.extension == ".csv":
            with open(self.path,"r") as csv_text:
                self.running = list(csv.reader(csv_text))
            # print(self.running)
            # self.set_column(**kwargs)
        elif self.extension == ".xlsx":
            self.wb = openpyxl.load_workbook(self.path)
            # edited_sheetname = re.sub(r"[^\w]","",sheetname)
            self.running = list(self.wb[sheetname].iter_rows(values_only=True))
            self.set_column()
        else:
            with open(self.path) as structured_text:
                self.running = structured_text.readlines()
            self.read_lines(**kwargs)

        # def read_csv(self,*args):
        # reader = np.array(reader)
        # num_row, num_col = reader.shape
        # if sheets['dataTypes']=='col':
        #    tuples = tuple(reader.T)
        # elif sheets['dataTypes']=='row':
        #    tuples = tuple(reader)
        # setparent(tuples,*args)
            
        # def read_xlsx(self,skiplines=0,headerline=None):
        # for i,sheetname in enumerate(sheets["names"]):
        #    sheet = wb[sheetname]
        #    if sheets['dataTypes'][i]=='col':
        #        tuples = tuple(sheet.iter_cols(max_col=sheets['num_cols'][i],values_only=True))
        #    elif sheets['dataTypes'][i]=='row':
        #        tuples = tuple(sheet.iter_rows(max_col=sheets['num_cols'][i],values_only=True))
        #    all_tuples = all_tuples+tuples
        # setparent(all_tuples,*args)

    # def read_list(self,skiplines=1,headerline=None):

    #     self.skiplines = skiplines

    #     if self.skiplines==0:
    #         self.header_rows = None
    #         self.header = ["col_"+str(column_id) for column_id in range(self.num_cols)]
    #         self.body_rows = self.running
    #         self.num_cols = len(self.running[0])
    #         self.set_columns()
        
    #     elif self.skiplines!=0:
    #         if headerline is None:
    #             linenumber = self.skiplines-1
    #         elif headerline < self.skiplines:
    #             linenumber = headerline
    #         else:
    #             linenumber = self.skiplines-1
    #         self.header_rows = self.running[:self.skiplines]
    #         self.header = self.running[linenumber]
    #         self.header = self.header.split("\n")[0].split("\t")
    #         self.body_rows = self.running[self.skiplines:]
    #         self.num_cols = len(self.running[0])
    #         self.set_columns()

    def read_lines(self,skiplines=0,headerline=None,structured=True):

        self.skiplines = skiplines

        if not structured:
            return

        self.body_rows = []

        for linenumber in range(len(self.running)):
            if linenumber>=self.skiplines:
                body_rowline = self.running[linenumber]
                self.body_rows.append(body_rowline.split("\n")[0].split("\t"))

        self.body_rows = np.array(self.body_rows)
        self.num_cols = self.body_rows.shape[1]

        if self.skiplines==0:
            self.header_rows = None
            self.header = ["col_"+str(column_id) for column_id in range(self.num_cols)]
            self.set_columns()
        
        elif self.skiplines!=0:
            if headerline is None:
                linenumber = self.skiplines-1
            elif headerline < self.skiplines:
                linenumber = headerline
            else:
                linenumber = self.skiplines-1
            self.header_rows = self.running[:self.skiplines]
            self.header = self.running[linenumber]
            self.header = self.header.split("\n")[0].split("\t")
            self.set_columns()

    def set_columns():
            
        for column_id,header in enumerate(self.header):
            try:
                header = re.sub(r"[^\w]","",header)
                setattr(self,header,self.body_rows[:,column_id].tolist())
                self.header[column_id] = header
            except:
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

    def __init__(self,window):

        self.root = window

        self.init_interface()

        # self.resize(1200,600)
        
        # filepath = "C:\\Users\\Cavid\\Documents\\bhospy\\interfaces\\sample.csv"
        
        # self.filepath = filepath
        # self.treeView = data_table(parent=self)
        # self.treeView.loadCSV(self.filepath)

        # self.treeView.autoColumnWidth()

        # # self.fileView = FileViewer(parent=self)

        # # print(dir(self.treeView))
        
        # self.centralwidget = QtWidgets.QWidget(self)
        # self.verticalLayout = QtWidgets.QVBoxLayout(self.centralwidget)
        # self.verticalLayout.addWidget(self.treeView)
        # # self.verticalLayout.addWidget(self.fileView)
        # self.setCentralWidget(self.centralwidget)
        
        # self.editedFlag = True

        # self.show()

    def init_interface(self):

        menubar = tk.Menu(self.root)
        
        self.root.config(menu=menubar)

        fileMenu = tk.Menu(menubar,tearoff="Off")

        fileMenu.add_command(label="Open",command=self.set_path)

        menubar.add_cascade(label="File",menu=fileMenu)

        self.tree = ttk.Treeview(self.root)
        self.tree.pack(expand=1,fill=tk.BOTH)

        # self.label_firstname = tk.Label(self.root,text="First Name")
        # self.label_firstname.grid(row=0,column=0)

        # self.label_lastname = tk.Label(self.root,text="Last Name")
        # self.label_lastname.grid(row=1,column=0)

        # self.entry_firstname = tk.Entry(self.root)
        # self.entry_firstname.grid(row=0,column=1)

        # self.entry_lastname = tk.Entry(self.root)
        # self.entry_lastname.grid(row=1,column=1)

        # self.button_enter = tk.Button(self.root,text="Enter",command=self.save_to_database)
        # self.button_enter.grid(row=2,column=1)

    def set_path(self):

        self.filepath = filedialog.askopenfilename(
            title = "Select a File",
            initialdir = os.getcwd(),
            filetypes = (("Databases","*.db"),
                         ("CSV Files","*.csv"),
                         ("Excel Files","*.xl*"),
                         ("All Files","*")))

        if not self.filepath:
            return

        self.dm = data_manager(self.filepath)

        self.set_tree_view()
        
        # self.wb = openpyxl.load_workbook(self.filepath)

        # for sheet in self.wb.sheetnames:
        #     self.listbox.insert(tk.END,sheet)

        # status = "Imported \""+self.filepath+"\"."
        # self.status.insert(tk.END,status)
        # self.status.see(tk.END)

    def set_tree_view(self):

        # self.tree['columns'] = ("Name","Position","Email")
        
        # self.tree.column("Name",anchor=tk.W,width=120)
        # self.tree.column("Position",anchor=tk.W,width=80)
        # self.tree.column("Email",anchor=tk.W,width=150)

        # self.tree.heading("#0",text="Label")
        # self.tree.heading("Name",text="Name",anchor=tk.CENTER)
        # self.tree.heading("Position",text="Position",anchor=tk.W)
        # self.tree.heading("Email",text="Email",anchor=tk.W)

        self.tree.column("#0",width=0,stretch=tk.NO)

        for idx,row in enumerate(self.dm.running):
            if idx==0:
                self.tree["columns"] = tuple(row)
                for idy,head in enumerate(row):
                    self.tree.column(head,anchor=tk.W,width=50,stretch=tk.NO)
                    self.tree.heading(head,text=head,anchor=tk.W)
            else:
                self.tree.insert(parent="",index="end",iid=idx,values=tuple(row))

        # self.tree.insert(parent="",index="end",iid=0,text="parent",values=("","Javid Shiriyev","cavid","cavid"))
        # self.tree.insert(parent="",index="end",iid=1,text="parent",values=("","elmri","elmri","elmri"))
        # self.tree.insert(parent="",index="end",iid=2,text="child",values=("","ferhad","ferhad","ferhad"))
        # self.tree.move(2,0,0)

    def save_to_database(self):

        db_file = r"C:\Users\Cavid\Documents\bhospy\instructors.db"

        self.conn = sqlite3.connect(db_file)
        # sqlite_table = '''CREATE TABLE SqliteDb_developers (
        #                  id INTEGER PRIMARY KEY,
        #                  name TEXT NOT NULL,
        #                  email text NOT NULL UNIQUE);'''
        self.cursor = self.conn.cursor()
        # self.cursor.execute(sqlite_table)
        # self.conn.commit()
        
        self.cursor.close()
        self.conn.close()

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

    def loadCSV(self,filepath):

        csvrunning = list(csv.reader(open(filepath)))

        #self.treeView.setHeaderLabels(csvrunning[0])

        self.model.setHorizontalHeaderLabels(csvrunning[0])

        for line in csvrunning[1:]:
            items = [
                QtGui.QStandardItem(text)
                for text in line
                ]
            self.model.appendRow(items)

    def writeCSV(self,filepath):

        with open(filepath,"w") as fileOutput:
            writer = csv.writer(fileOutput)

            headers = [
                self.model.horizontalHeaderItem(columnNumber).text()
                for columnNumber in range(self.model.columnCount())
            ]

            writer.writerow(headers)

            for rowNumber in range(self.model.rowCount()):
                fields = [
                    self.model.data(
                        self.model.index(rowNumber,columnNumber),
                        QtCore.Qt.DisplayRole
                    )
                    for columnNumber in range(self.model.columnCount())
                ]
                writer.writerow(fields)

    def autoColumnWidth(self):
        
        for columnNumber in range(self.model.columnCount()):
            self.resizeColumnToContents(columnNumber)

class data_graph():

    """
    The task is about the history matches. When we do changes to input files and run
    it, we want to compare two sets of production-history data. It is kinda
    having set point between each runs that allow us to select best path to follow.
    """

    def __init__(self,window):

        self.root = window

        self.init_interface()

    def init_interface(self):
        
        self.root.configure(background="white")

        menubar = tk.Menu(self.root)
        
        self.root.config(menu=menubar)

        fileMenu = tk.Menu(menubar,tearoff="Off")

        fileMenu.add_command(label="Open",command=self.set_path)

        menubar.add_cascade(label="File",menu=fileMenu)

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

        if not self.filepath:
            return
        
        self.wb = openpyxl.load_workbook(self.filepath)

        for sheet in self.wb.sheetnames:
            self.listbox.insert(tk.END,sheet)

        status = "Imported \""+self.filepath+"\"."
        self.status.insert(tk.END,status)
        self.status.see(tk.END)

    def set_figure_template(self,event):

        if not self.listbox_template.curselection():
            return
        
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

        if not self.listbox.curselection():
            return
        
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

    """data table interface"""
    
    window = tk.Tk()
    gui = data_table(window)
    window.mainloop()

    """data plot interface"""
    
    # window = tk.Tk()
    # gui = data_graph(window)
    # window.mainloop()
