import csv
import datetime
import os
import sys

import matplotlib.pyplot as plt

from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg

import numpy as np

import openpyxl

from PyQt5 import QtCore
from PyQt5 import QtGui
from PyQt5 import QtWidgets

import sqlite3

from sqlite3 import Error

import tkinter as tk

from tkinter import ttk
from tkinter import filedialog

class database_manager():

    def __init__(self,dbpath):

        self.dbpath = dbpath

        self.create_connection()

    def create_connection(self):

        self.conn = None

        try:
            self.conn = sqlite3.connect(self.dbpath)
        except Error:
            print(Error)

    def create_table(self,sqlite_table):

        self.sqlite_table = sqlite_table

        try:
            self.cursor = self.conn.cursor()
            self.cursor.execute(self.sqlite_table)
            self.conn.commit()
        except Error:
            print(Error)

    def insert_table(self,sqlite_table_insert,table_row):

        self.sqlite_table_insert = sqlite_table_insert
        self.cursor.execute(self.sqlite_table_insert,table_row)
        self.conn.commit()

class database_interface():

    def __init__(self,window):

        self.root = window

        self.init_interface()

    def init_interface(self):

        self.label_firstname = tk.Label(self.root,text="First Name")
        self.label_firstname.grid(row=0,column=0)

        self.label_lastname = tk.Label(self.root,text="Last Name")
        self.label_lastname.grid(row=1,column=0)

        self.entry_firstname = tk.Entry(self.root)
        self.entry_firstname.grid(row=0,column=1)

        self.entry_lastname = tk.Entry(self.root)
        self.entry_lastname.grid(row=1,column=1)

        self.button_enter = tk.Button(self.root,text="Enter",command=self.save_to_database)
        self.button_enter.grid(row=2,column=1)

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

class datafile_manager():

    """

    The task was to get dates of well from the first input file
    (inputfile0) and modify the production data of input file 2
    (inputfile1) so that the specified well will have new
    production data.

    """

    def __init__(self,filename,sheets,*args):
        
        _,file_extension = os.path.splitext(filename)

        if file_extension == ".csv":
            readcsv(filename,sheets,*args)
        elif file_extension == ".xlsx":
            readxlsx(filename,sheets,*args)

    def reading(self):
        
        with open(inputfile0) as readfile:
            
            rdate = np.array([])
            rcopy = np.array([])

            rlines = readfile.readlines()

            for rlinenumber,rline in enumerate(rlines):

                if rlinenumber>0:

                    alist = rline.split('\t')

                    wellname = alist[0]

                    date_list = alist[1].split('-')

                    if date_list[1]=='01':
                        date_newformat = date_list[2]+' JAN '+date_list[0]
                    elif date_list[1]=='02':
                        date_newformat = date_list[2]+' FEB '+date_list[0]
                    elif date_list[1]=='03':
                        date_newformat = date_list[2]+' MAR '+date_list[0]
                    elif date_list[1]=='04':
                        date_newformat = date_list[2]+' APR '+date_list[0]
                    elif date_list[1]=='05':
                        date_newformat = date_list[2]+' MAY '+date_list[0]
                    elif date_list[1]=='06':
                        date_newformat = date_list[2]+' JUN '+date_list[0]
                    elif date_list[1]=='07':
                        date_newformat = date_list[2]+' JUL '+date_list[0]
                    elif date_list[1]=='08':
                        date_newformat = date_list[2]+' AUG '+date_list[0]
                    elif date_list[1]=='09':
                        date_newformat = date_list[2]+' SEP '+date_list[0]
                    elif date_list[1]=='10':
                        date_newformat = date_list[2]+' OCT '+date_list[0]
                    elif date_list[1]=='11':
                        date_newformat = date_list[2]+' NOV '+date_list[0]
                    elif date_list[1]=='12':
                        date_newformat = date_list[2]+' DEC '+date_list[0]

                    rdate = np.append(rdate,date_newformat)
                        
                    days = alist[2]
                    condensate = alist[3]
                    gas = alist[4]
                    water = alist[5]

                    if rlinenumber+1==len(rlines):
                        copied_line = '\t\''+wellname+'\'\tSTOP\tGRAT\t'+condensate+'\t'+water+'\t'+gas+' /\n'
                    else:
                        copied_line = '\t\''+wellname+'\'\tOPEN\tGRAT\t'+condensate+'\t'+water+'\t'+gas+' /\n'

                    rcopy = np.append(rcopy,copied_line)

    def readcsv(filename,sheets,*args):

        with open(filename) as csvfile:
            reader = list(csv.reader(csvfile))
            reader = np.array(reader)
            num_row, num_col = reader.shape
            if sheets['dataTypes']=='col':
                tuples = tuple(reader.T)
            elif sheets['dataTypes']=='row':
                tuples = tuple(reader)
            setparent(tuples,*args)
            
    def readxlsx(filename,sheets,*args):
        
        all_tuples = ()
        wb = openpyxl.load_workbook(filename)
        for i,sheetname in enumerate(sheets["names"]):
            sheet = wb[sheetname]
            if sheets['dataTypes'][i]=='col':
                tuples = tuple(sheet.iter_cols(max_col=sheets['num_cols'][i],values_only=True))
            elif sheets['dataTypes'][i]=='row':
                tuples = tuple(sheet.iter_rows(max_col=sheets['num_cols'][i],values_only=True))
            all_tuples = all_tuples+tuples
        setparent(all_tuples,*args)

    def skiptoline(file_read,keyword,file_written=None):
    
        while True:
            
            line = next(file_read)

            if file_written is not None:
                file_written.write(line)
            
            if line.split('/')[0].strip() == keyword:
                break

    def setparent(tuples,*args):
        for i in range(len(tuples)):
            for arg in args:
                if tuples[i][0] == arg.__name__:
                    setchild(tuples[i],arg)

    def setchild(atuple,obj):
        array = np.array(atuple[3:]).astype('float64')
        setattr(obj,atuple[1]+'_unit',atuple[2])
        setattr(obj,atuple[1],array)

    def writing(self):
            
        with open(inputfile1) as rewrittenfile:
            with open(outputfile,"w") as writtenfile:
                for i,date in enumerate(rdate):
                    skiptoline(rewrittenfile,'DATES',writtenfile)
                    skiptoline(rewrittenfile,rdate[i],writtenfile)
                    skiptoline(rewrittenfile,'WCONHIST',writtenfile)
                    while True:
                        wline = next(rewrittenfile)
                        if wline.split('/')[0].strip().split(' ')[0] == '\'BHR_76\'':
                            writtenfile.write(rcopy[i])
                            break
                        else:
                            writtenfile.write(wline)
                while True:
                    try:
                        copiedline = next(rewrittenfile)
                        writtenfile.write(copiedline)
                    except:
                        break

class Window(QtWidgets.QMainWindow):

    #itemEdited = QtCore.pyqtSignal(item,column)
    
    def __init__(self,filepath):
        
        super(Window,self).__init__()

        self.resize(1200,600)
        
        filepath = "C:\\Users\\Cavid\\Documents\\bhospy\\interfaces\\sample.csv"
        
        self.filepath = filepath
        self.treeView = data_table(parent=self)
        self.treeView.loadCSV(self.filepath)

        self.treeView.autoColumnWidth()

        # self.fileView = FileViewer(parent=self)

        # print(dir(self.treeView))
        
        self.centralwidget = QtWidgets.QWidget(self)
        self.verticalLayout = QtWidgets.QVBoxLayout(self.centralwidget)
        self.verticalLayout.addWidget(self.treeView)
        # self.verticalLayout.addWidget(self.fileView)
        self.setCentralWidget(self.centralwidget)
        
        self.editedFlag = True

        self.show()   

    def closeEvent(self,event):
        
        if self.treeView.editedFlag:
            choice = QtWidgets.QMessageBox.question(self,'Quit',
                "Do you want to save changes?",QtWidgets.QMessageBox.Yes | 
                QtWidgets.QMessageBox.No)

            if choice == QtWidgets.QMessageBox.Yes:
                self.treeView.writeCSV(self.filepath)

        event.accept()

class data_table(QtWidgets.QTreeView):

    def __init__(self,parent=None):
        super(data_table,self).__init__(parent)

        #print(dir(self))

        self.setSortingEnabled(True)

        self.model = QtGui.QStandardItemModel()
        self.setModel(self.model)

        #self.setColumnWidth(0,800)

        self.editedFlag = False

        self.model.itemChanged.connect(self.changeFlag)

    def changeFlag(self):
        self.editedFlag = True

    def mouseDoubleClickEvent(self,event):

        index = self.indexAt(event.pos())
        #print(index.row(),index.column())

        #item = self.model.item(index.row(),index.column())
        #print(item.data(QtCore.Qt.DisplayRole))
        #item.setEditable(True)

        if index.row()>-1 and index.column()>-1:
            self.edit(index)

    def loadCSV(self,filepath):

        csvcontent = list(csv.reader(open(filepath)))

        #self.treeView.setHeaderLabels(csvcontent[0])

        self.model.setHorizontalHeaderLabels(csvcontent[0])

        for line in csvcontent[1:]:
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

        self.root.title("BEOC-Subsurface")
        
        self.root.configure(background="white")

        menubar = tk.Menu(self.root)
        
        self.root.config(menu=menubar)

        fileMenu = tk.Menu(menubar,tearoff="Off")

        fileMenu.add_command(label="Open",command=self.set_path)

        menubar.add_cascade(label="File",menu=fileMenu)

        self.frame = tk.Frame(self.root,width=300,height=200)

        tk.Grid.rowconfigure(self.frame,0,weight=1)

        tk.Grid.columnconfigure(self.frame,0,weight=1)
        tk.Grid.columnconfigure(self.frame,1,weight=1)

        self.frame.configure(background="white")

        self.fileviewer = FileViewer(self.frame)

        self.listbox = tk.Listbox(self.frame,width=10,height=30)
        self.listbox.grid(row=0,column=0,sticky=tk.NSEW)

        self.listbox.bind('<Double-1>',self.get_listentry)

        self.button = tk.Button(self.frame,text="Plot Graph",command=lambda: self.get_sheet(self.listbox))
        self.button.grid(row=1,column=0)

        self.figure = plt.Figure()
        
        self.plot = FigureCanvasTkAgg(self.figure,self.frame)

        self.plot_widget = self.plot.get_tk_widget()

        self.plot_widget.grid(row=0,column=1,sticky=tk.NSEW) #width=250,height=30
        
        self.status = tk.Listbox(self.frame,width=250,height=5)
        self.status.grid(row=1,column=1,sticky=tk.EW)

        self.frame.pack(side=tk.LEFT,expand=1,fill=tk.BOTH)
        
    def set_path(self):

        self.filepath = filedialog.askopenfilename(
            title = "Select a File",
            initialdir = os.getcwd(),
            filetypes = (("Excel files","*.xl*"),("All files","*.*")))

        if not self.filepath:
            return

        self.inputfile = self.filepath
        
        self.wb = openpyxl.load_workbook(self.inputfile)

        status = "Imported \""+self.filepath+"\"."

        for sheet in self.wb.sheetnames:
            self.listbox.insert(tk.END,sheet)
        
        self.status.insert(tk.END,status)
        self.status.see(tk.END)

    def get_listentry(self,event):
        
        self.get_sheet(event.widget)

    def get_sheet(self,item):

        if not item.curselection():
            return

        if hasattr(self,"axes"):
            for axis in self.axes:
                axis.remove()

        self.template()

        class data: pass
                
        data.ws = self.wb[self.listbox.get(item.curselection())]
        
        data.date = list(data.ws.iter_cols(min_row=2,max_col=1,values_only=True))[0]
        
        Head = list(data.ws.iter_rows(max_row=1,min_col=2,values_only=True))[0]
        Body = list(data.ws.iter_cols(min_row=2,min_col=2,values_only=True))

        for i,head in enumerate(Head):
            string = head.split(":")[1].split(",")[0].strip().replace(" ","_")
            string = string.replace("(","").replace(")","").replace(".","")
            setattr(data,string,np.array(Body[i]))
        
        try: self.axes[0].plot(data.date,data.Oil_Rate,c='k')
        except: pass
        try: self.axes[0].plot(data.date,data.Oil_Rate_H,'--',c='g')
        except: pass
        try: self.axes[1].plot(data.date,data.Oil_Total/1000,c='k')
        except: pass
        try: self.axes[1].plot(data.date,data.Oil_Total_H/1000,'--',c='g')
        except: pass
        
        try: self.axes[2].plot(data.date,data.Gas_Rate/1000,c='k')
        except: pass
        try: self.axes[2].plot(data.date,data.Gas_Rate_H/1000,'--',c='r')
        except: pass
        try: self.axes[3].plot(data.date,data.Gas_Total/1000000,c='k')
        except: pass
        try: self.axes[3].plot(data.date,data.Gas_Total_H/1000000,'--',c='r')
        except: pass
        
        try: self.axes[4].plot(data.date,data.Water_Rate,c='k')
        except: pass
        try: self.axes[4].plot(data.date,data.Water_Rate_H,'--',c='b')
        except: pass
        try: self.axes[5].plot(data.date,data.Water_Total/1000,c='k')
        except: pass
        try: self.axes[5].plot(data.date,data.Water_Total_H/1000,'--',c='b')
        except: pass
        
        try: self.axes[6].plot(data.date,data.Bottom_Hole_Pressure,c='k')
        except: self.axes[6].plot(data.date,data.Avg_Pressure,c='k')
        try: self.axes[6].plot(data.date,data.Bottom_Hole_Pressure_H,'--',c='m')
        except: pass

        self.figure.set_tight_layout(True)
        
        self.plot.draw()

    def template(self):

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

if __name__ == "__main__":

    """database manager"""

    dbpath = r"C:\Users\Cavid\Documents\bhospy\interfaces\instructors.db"

    DB = database_manager(dbpath)

    instructor_table = """ CREATE TABLE IF NOT EXISTS instructors (
                                        id integer PRIMARY KEY,
                                        first_name text NOT NULL,
                                        last_name text NOT NULL,
                                        patronym text NOT NULL,
                                        position text NOT NULL,
                                        status text NOT NULL,
                                        email text NOT NULL UNIQUE,
                                        phone integer NOT NULL UNIQUE);"""

    DB.create_table(instructor_table)

    instructor_table_insert = """ INSERT INTO instructors(
                                        id,
                                        first_name,
                                        last_name,
                                        patronym,
                                        position,
                                        status,
                                        email,
                                        phone)
                                        VALUES(?,?,?,?,?,?,?,?)"""

    instructor_7 = (7,"Javid","Shiriyev","Farhad",
                "Senior Lecturer","Hour Based Teaching",
                "cavid.shiriyev@bhos.edu.az","+994508353992")

    DB.insert_table(instructor_table_insert,instructor_7)
    DB.cursor.close()
    DB.conn.close()

    """database interface"""
    
    # window = tk.Tk()
    # gui = database_constructor(window)
    # window.mainloop()

    """data plot"""
    
    # window = tk.Tk()
    # gui = plot_production(window)
    # window.mainloop()

    """input editor"""

    # app = QtWidgets.QApplication(sys.argv)
    # # window = Window(args.filepath)
    # window = Window(os.curdir)
    # sys.exit(app.exec_())

    """task 0"""
    
    # inputfile0 = 'C:\\Users\\javid.s\\Desktop\\BHR-076.txt'
    # inputfile1 = 'C:\\Users\\javid.s\\Desktop\\BHR_VI_SCHEDULE.INC'

    # outputfile = 'C:\\Users\\javid.s\\Desktop\\BHR_VI_SCHEDULE_V2.INC'

    """input reader"""

    # class reservoir: pass
    # class well: pass
    # class time: pass

    # sheets = {
    #    "num_cols": 5,
    #    "dataTypes": "col"
    #    }

    # setup("sample.csv",sheets,reservoir)

    # print(reservoir.porosity)

    # sheets = {
    #    "names": ["build_up_test","parameters"],
    #    "num_cols": [3,4],
    #    "dataTypes": ["col","row"]
    #    }

    # readxlsx('sample.xlsx',sheets,time,well)
