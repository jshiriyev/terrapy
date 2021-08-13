from dateutil.parser import parse

import os
import re

from matplotlib import pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg

import numpy as np
import openpyxl

import sqlite3
from sqlite3 import Error as sqlError

import tkinter as tk

from tkinter import ttk
from tkinter import filedialog



class manager():

    special_extensions = [".db",".xlsx"]

    def __init__(self,filepath=None,skiplines=1,headerline=None,comment="--",endline="/",endfile="END",**kwargs):

        if filepath is None:
            return

        self.filepath = filepath

        self.skiplines = skiplines

        if headerline is None:
            self.headerline = skiplines-1
        elif headerline<skiplines:
            self.headerline = headerline
        else:
            self.headerline = skiplines-1

        self.comment = comment
        self.endline = endline
        self.endfile = endfile

        self.filename = os.path.split(self.filepath)[1]
        self.extension = os.path.splitext(self.filepath)[1]

        if any([self.extension==extension for extension in self.special_extensions]):
            self.read_special(**kwargs)
        else:
            self.read_plain()

        self.title = []

        for _ in range(self.skiplines):
            self.title.append(self._running.pop(0))

        num_cols = len(self._running[0])

        if self.skiplines==0:
            self._headers = ["Column #"+str(index) for index in range(num_cols)]
        elif skiplines!=0:
            self._headers = self.title[self.headerline]

        self.headers = self._headers

        nparray = np.array(self._running).T

        self._running = [np.asarray(column) for column in nparray]

        self.running = [np.asarray(column) for column in self._running]

    def read_plain(self):

        # While looping inside the file it does not read lines:
        # - starting with comment phrase, e.g., comment = "--"
        # - after the end of line phrase, e.g., endline = "/"
        # - after the end of file keyword e.g., endfile = "END"

        self._running = []

        flagContinueLoopFile = True

        with open(self.filepath,"r") as text:

            while flagContinueLoopFile:

                try:
                    line = next(text)
                except:
                    break

                line = line.split('\n')[0].strip()

                if self.endline is not None:
                    line = line.strip(self.endline)

                if line=="":
                    continue

                if self.comment is not None:
                    if line[:len(self.comment)] == self.comment:
                        continue

                if self.endfile is not None:
                    if line[:len(self.endfile)] == self.endfile:
                        break

                self._running.append([line])

    def read_special(self,sheetname=None,min_row=1,min_col=1,max_row=None,max_col=None):

        if self.extension == ".db":

            self.conn = None

            try:
                self.conn = sqlite3.connect(self.filepath)
            except Error:
                print(Error)
            return

        if self.extension == ".xlsx":

            wb = openpyxl.load_workbook(self.filepath,read_only=True)

            lines = wb[self.sheetname].iter_rows(min_row=min_row,
                max_row=max_row,min_col=min_col,max_col=max_col,values_only=True)

            self._running = [list(line) for line in lines]

            wb._archive.close()
            return

    def set_subheaders(self,header_index,match=None,title="sub-headers"):

        nparray = np.array(self._running[header_index])

        if match is not None:
            match_col,match_keys = match
            match_keys = np.array(match_keys).reshape((-1,1))
            match_index = np.any(self._running[match_col]==match_keys,axis=0)

        else:
            match_letter = np.empty(nparray.shape,dtype=bool)
            for index,string in enumerate(nparray):
                if any([character.isdigit() for character in string]):
                    match_letter[index] = False
                else:
                    match_letter[index] = True

            match_upper = np.char.isupper(nparray)
            match_index = np.logical_and(match_letter,match_upper)

        firstocc = np.argmax(match_index)

        lower = np.where(match_index)[0]
        upper = np.append(lower[1:],nparray.size)

        repeat_count = upper-lower-1

        match = nparray[match_index]

        nparray[firstocc:][~match_index[firstocc:]] = np.repeat(match,repeat_count)

        self._headers.insert(header_index,title)
        self._running.insert(header_index,np.asarray(nparray))

        for index,column in enumerate(self._running):
            self._running[index] = np.array(self._running[index][firstocc:][~match_index[firstocc:]])

        self.headers = self._headers
        self.running = [np.asarray(column) for column in self._running]

    def columntotext(self,header_name_new,header_indices,deliminator=" "):

        col_concatn = np.empty((len(header_indices),self._running[0].size),dtype='U25')
        col_indices = np.empty(len(header_indices))

        for index in enumerate(header_indices):
            col_concatn[index] = self._running[index]

        for index in header_indices:
            self._headers.pop(index)
            self._running.pop(index)

        self._headers.insert(col_indices.min(),header_name_new)
        self._running.insert(col_indices.min(),np.array([deliminator.join(row) for row in col_concatn.T]))

        self.headers = self._headers
        self.running = [np.asarray(column) for column in self._running]

    def texttocolumn(self,header_index,deliminator,max_split):

        header_string = self._headers[header_index]
        header_string = re.sub(deliminator+'+',deliminator,header_string)

        headers = header_string.split(deliminator)

        for index in range(max_split):
            if len(headers)<index+1:
                headers.append("col ##"+string(header_index+index))

        column_to_split = np.asarray(self._running[header_index])

        running = []

        for index,string in enumerate(column_to_split):

            string = re.sub(deliminator+'+',deliminator,string)

            row = np.char.split(string,deliminator).tolist()

            while len(row)<max_split:
                row.append("")

            running.append(row)

        running = np.array(running,dtype=str).T

        self._headers.pop(header_index)
        self._running.pop(header_index)

        for header,column in zip(headers,running):
            self._headers.insert(header_index,header)
            self._running.insert(header_index,column)
            header_index += 1

        # line = re.sub(r"[^\w]","",line)
        # line = "_"+line if line[0].isnumeric() else line

        self.headers = self._headers
        self.running = [np.asarray(column) for column in self._running]
        
    def astype(self,header_indices=None,headers=None,dtypes=None):

        if header_indices is None:

            header_indices = []

            for header in headers:
                header_indices.append(self._headers.index(header))

        for index,header_index in enumerate(header_indices):

            column = np.asarray(self.running[header_index])

            if dtypes[index]==np.datetime64:

                dates_list = []

                for string in column:

                    dates_list.append(parse(string))
                    # dates_list.append(datetime.datetime.strptime(string,format))

                column = np.array(dates_list)

            self.running[header_index] = column.astype(dtypes[index])

    def set_rows(self,row,row_indices=None):
        
        if row_indices is None:
            for index,column in enumerate(self.running):
                self.running[index] = np.append(column,row[index])
        else:
            for index, _ in enumerate(self.running):
                self.running[index][row_indices] = row[index]

    def del_rows(self,row_indices):

        for index,col in enumerate(self.running):
            self.running[index] = np.delete(col,row_indices)

    def get_columns(self,header_indices=None,headers=None):

        indicesToKeep = []

        for header_read_from_file in self._headers:
            try:
                if any([header_read_from_file.strip() == header for header in headers]):
                    indicesToKeep.append(self._headers.index(header_read_from_file))
            except:
                continue

        self.headers = [self._headers[index] for index in indicesToKeep]
        self.running = [[] for _ in range(indicesToKeep)]

        for index in indicesToKeep:
            self.running[index] = np.asarray(self._running[index])

    def get_rows(self,row_indices):

        rows = []
        
        for index,column in enumerate(self.running):
            # print(type(column[row_indices]))
            # print(column[row_indices][:10])
            rows.append(column[row_indices].astype(str))

        return np.asarray(rows).T

    def filter_columns(self,keywords,header_index=None,header=None,inplace=False):

        if header_index is None:
            header_index = self._headers.index(header)

        keywords = np.array(keywords).reshape((-1,1))

        match_index = np.any(self._running[header_index]==keywords,axis=0)

        self.running = [[] for _ in range(len(self._running))]

        if not inplace:
            for index,column in enumerate(self._running):
                self.running[index] = np.asarray(column[match_index])
        else:
            for index,column in enumerate(self._running):
                self._running[index] = column[match_index]
                self.running[index] = np.asarray(self._running[index])

        # listA = self.get_column(header.lower())

        # idx = list(range(len(listA)))

        # zipped_lists = zip(listA,idx)
        # sorted_pairs = sorted(zipped_lists)

        # tuples = zip(*sorted_pairs)

        # listA, idx = [ list(tuple) for tuple in  tuples]

        # for header in self._headers:
        #     self.toarray(header)
        #     setattr(self,header,getattr(self,header)[idx])

    def filter_rows(self,keyword,header_index=None,header=None,inplace=False):

        if header_index is None:
            header_index = self._headers.index(header)

        column = self.running[header_index]

        splitted = np.char.split(column,"'",maxsplit=2)

        match_index = np.empty(column.shape,dtype=bool)

        for index,row in enumerate(splitted):
            if row[1]==keyword:
                match_index[index] = True
            else:
                match_index[index] = False

        self.running = [[] for _ in range(len(self._running))]

        if not inplace:
            for index,column in enumerate(self._running):
                self.running[index] = np.asarray(column[match_index])
        else:
            for index,column in enumerate(self._running):
                self._running[index] = column[match_index]
                self.running[index] = np.asarray(self._running[index])

    def write(self,filepath,sheet_title=None):

        running = np.array(self.running,dtype='U25').T

        running = np.insert(running,0,self._headers).reshape((-1,len(self.running)))

        extension = os.path.splitext(filepath)[1]

        if extension == ".bhos":
            with open(filepath,"w",encoding='utf-8') as writtenfile:
                for line in running:
                    writtenfile.write("\t".join(line)+"\t/\n")
                writtenfile.write("/\n")
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

   

class table(manager):

    def __init__(self,headers=["GETALL"],filepath=None,**kwargs):

        super().__init__(filepath,**kwargs)

        if filepath is None:
            self.headers = headers
            self.running = [np.array([]) for _ in self.headers]
        elif headers[0]!="GETALL":
            self.trim_columns_by_headers(headers)

    def draw(self,window,func=None):

        self.root = window

        self.scrollbar = tk.Scrollbar(self.root)

        self.columns = ["#"+str(idx) for idx,_ in enumerate(self.headers,start=1)]

        self.tree = ttk.Treeview(self.root,columns=self.columns,show="headings",selectmode="browse",yscrollcommand=self.scrollbar.set)

        self.sortReverseFlag = [False for col in self.columns]

        for idx,(column,header) in enumerate(zip(self.columns,self.headers)):
            if idx<len(self.headers)-1 :
                self.tree.column(column,anchor=tk.W,width=100)
            elif idx==len(self.headers)-1:
                self.tree.column(column,anchor=tk.W)
            self.tree.heading(column,text=header,anchor=tk.W,command=lambda x = idx: self.sort_column(x))

        self.tree.pack(side=tk.LEFT,expand=1,fill=tk.BOTH)

        self.scrollbar.pack(side=tk.LEFT,fill=tk.Y)

        self.scrollbar.config(command=self.tree.yview)

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

        self.tablesize = self.running[0].size

        for index,row in enumerate(self.get_rows(list(range(self.tablesize)))):
            self.tree.insert(parent="",index="end",iid=index,values=tuple(row.tolist()))

        self.editedFlag = False

        self.added = []
        self.edited = []
        self.deleted = []

    def addItem(self):

        if hasattr(self,"topAddItem"):
            if self.topAddItem.winfo_exists():
                return

        self.topAddItem = tk.Toplevel()

        for idx,(header,explicit) in enumerate(zip(self.headers,self.headers)):
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

        self.tree.insert(parent="",index="end",iid=self.tablesize,values=values)

        self.added.append(values)

        self.tablesize += 1

        self.editedFlag = True

        self.topAddItem.destroy()

    def editItem(self):

        if not self.tree.selection():
            return
        else:
            item = self.tree.selection()[0]

        values = self.tree.item(item)['values']

        self.topEditItem = tk.Toplevel()

        for idx,(header,explicit) in enumerate(zip(self.headers,self.headers)):
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

        self.tree.item(item,values=values)

        self.edited.append([int(item),values]) # index and new values

        self.editedFlag = True

        self.topEditItem.destroy()

    def deleteItem(self):

        if not self.tree.selection():
            return
        else:
            item = self.tree.selection()[0]

        self.deleted.append(int(item))

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

        for values in self.added:
            self.set_row(values)

        self.added = []

        for index,values in self.edited:
            self.set_row(values,index)

        self.edited = []

        self.del_row(self.deleted)

        self.deleted = []

        self.tablesize = self.running[0].size

        self.editedFlag = False

        if func is None:
            for index in range(self.tablesize):
                print(self.get_row(index))
            self.tree.delete(*self.tree.get_children())
            for index in range(self.tablesize):
                self.tree.insert(parent="",index="end",iid=index,values=self.get_row(index))
        else:
            func()
            self.root.destroy()

    def sort_column(self,index):

        column = self.columns[index]
        reverseFlag = self.sortReverseFlag[index]

        col_and_idx = [(self.tree.set(k,column), k) for k in self.tree.get_children('')]

        col_and_idx.sort(reverse=reverseFlag)

        for idx, ( _ ,k) in enumerate(col_and_idx):
            self.tree.move(k,'',idx)

        self.sortReverseFlag[index] = not reverseFlag



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

    gui = table("instructors.csv")
    gui = table(headers=["Full Name","Position","Contact"])

    gui.draw(window)

    window.mainloop()
