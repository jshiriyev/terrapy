import calendar

from datetime import datetime
from dateutil.parser import parse
from dateutil.relativedelta import relativedelta

import inspect

import os
import re

import tkinter as tk

from tkinter import ttk
from tkinter import filedialog

from ttkwidgets.autocomplete import AutocompleteEntryListbox

import warnings

from matplotlib import pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg

import numpy as np

if __name__ == "__main__":
    import setup

temp0 = {
    "name": "Standard",
    "subplots": [1,1],
    "title": [""],
    "twinx": [False],
    "xlabel": ["x-axis"],
    "ylabel": ["y-axis"],
    "xticks": [None],
    "yticks": [None],
    "grid": [True],
    #
    "xaxes": [[1,1,1]],
    "yaxes": [[2,3,4]],
    "colors": [["k","b","r"]],
    "legends": [True],
}

temp1 = {
    "name": "Standard-dual horizontal stack",
    "subplots": [1,2],
    "twinx": [False,False],
    "title": ["Left","Right"],
    "xlabel": ["x-axis","x-axis"],
    "ylabel": ["y-axis","y-axis"],
    "xticks": [None,None],
    "yticks": [None,None],
    "grid": [True,True],
    #
    "xaxes": [[1],[1,1]],
    "yaxes": [[2],[3,4]],
    "colors": [["k"],["b","r"]],
    "legends": [True,True],
}

temp2 = {
    "name": "Standard-dual vertical stack",
    "subplots": [2,1],
    "twinx": [False,False],
    "title": ["Top","Bottom"],
    "xlabel": ["x-axis","x-axis"],
    "ylabel": ["y-axis","y-axis"],
    "xticks": [None,None],
    "yticks": [None,None],
    "grid": [True,True],
    #
    "xaxes": [[1],[1,1]],
    "yaxes": [[2],[3,4]],
    "colors": [["k"],["b","r"]],
    "legends": [True,True],
}

temp3 = {
    "name": "Standard-quadruple",
    "subplots": [2,2],
    "twinx": [False,False,False,False],
    "title": ["NW","NE","SW","SE"],
    "xlabel": ["x-axis","x-axis","x-axis","x-axis"],
    "ylabel": ["y-axis","y-axis-2","y-axis","y-axis-2","y-axis","y-axis-2","y-axis"],
    "xticks": [None,None,None,None],
    "yticks": [None,None,None,None,None,None,None],
    "grid": [True,True,True,True],
    #
    "xaxes": [[1],[1],[1],[]],
    "yaxes": [[2],[3],[4],[]],
    "colors": [["k"],["b"],["r"],[]],
    "legends": [True,True,True,False],
}

func_integer = lambda x: True if x.isdigit() or x == "" else False

class dataset():

    special_extensions = [".db",".xlsx",".las"]

    templates = (temp0,temp1,temp2,temp3)

    def __init__(self,window=None,headers=None,filepath=None,skiplines=0,headerline=None,comment=None,endline=None,endfile=None,**kwargs):

        # There are two visualization options:
        #   case 1: tabulating
        #   case 2: plotting allowing also templates

        # There are two uses of dataset, case 1 or case 2:
        #   case 1: headers
        #   case 2: filepath,skiplines,headerline
        #    - reading plain text: comment,endline,endfile
        #    - reading scpecial extensions: **kwargs

        if window is not None:
            self.root = window

        self.dirname = os.path.dirname(__file__)

        if headers is not None:
            self._headers = headers
            self._running = [np.array([])]*len(self._headers)

        elif filepath is not None:
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
                self.read()

        else:
            return

        self.headers = self._headers
        self.running = [np.asarray(column) for column in self._running]

    def read(self):

        # While looping inside the file it does not read lines:
        # - starting with comment phrase, e.g., comment = "--"
        # - after the end of line phrase, e.g., endline = "/"
        # - after the end of file keyword e.g., endfile = "END"

        self._running = []

        with open(self.filepath,"r") as text:

            for line in text:

                line = line.split('\n')[0].strip()

                if self.endline is not None:
                    line = line.strip(self.endline)

                line = line.strip()
                line = line.strip("\t")
                line = line.strip()

                if line=="":
                    continue

                if self.comment is not None:
                    if line[:len(self.comment)] == self.comment:
                        continue

                if self.endfile is not None:
                    if line[:len(self.endfile)] == self.endfile:
                        break

                self._running.append([line])

        self.title = []

        for _ in range(self.skiplines):
            self.title.append(self._running.pop(0))

        num_cols = len(self._running[0])

        if self.skiplines==0:
            self._headers = ["Column #"+str(index) for index in range(num_cols)]
        elif self.skiplines!=0:
            self._headers = self.title[self.headerline]

        nparray = np.array(self._running).T

        self._running = [np.asarray(column) for column in nparray]

    def read_special(self,sheetname=None,min_row=1,min_col=1,max_row=None,max_col=None):

        if self.extension == ".xlsx":

            import openpyxl

            wb = openpyxl.load_workbook(self.filepath,read_only=True)

            rows = wb[sheetname].iter_rows(min_row=min_row,min_col=min_col,
                max_row=min_row+self.skiplines-1,max_col=max_col,values_only=True)

            rows = list(rows)

            self._headers = list(rows[self.headerline-1])

            if self.headerline<self.skiplines:

                for index,(header,header_lower) in enumerate(zip(self._headers,rows[self.skiplines-1])):
                    if header_lower is not None:
                        self._headers[index] = header_lower.strip()
                    elif header is not None:
                        self._headers[index] = header.strip()
                    else:
                        self._headers[index] = None

            columns = wb[sheetname].iter_rows(min_row=min_row+self.skiplines,min_col=min_col,
                max_row=max_row,max_col=max_col,values_only=True)

            nparray = np.array(list(columns)).T

            self._running = [np.asarray(column) for column in nparray]

            wb._archive.close()

        elif self.extension == ".las":

            import lasio

            las = lasio.read(self.filepath)

            self._headers = las.keys()

            self._running = [np.asarray(column) for column in las.data.transpose()]

    def set_subheaders(self,header_index=None,header=None,regex=None,regex_builtin="INC_HEADERS",title="SUB-HEADERS"):

        nparray = np.array(self._running[header_index])

        if regex is None and regex_builtin=="INC_HEADERS":
            regex = r'^[A-Z]+$'                         #for strings with only capital letters no digits
        elif regex is None and regex_builtin=="INC_DATES":
            regex = r'^\d{1,2} [A-Za-z]{3} \d{2}\d{2}?$'   #for strings with [1 or 2 digits][space][3 capital letters][space][2 or 4 digits], e.g. DATES

        vmatch = np.vectorize(lambda x: bool(re.compile(regex).match(x)))

        match_index = vmatch(nparray)

        firstocc = np.argmax(match_index)

        lower = np.where(match_index)[0]
        upper = np.append(lower[1:],nparray.size)

        repeat_count = upper-lower-1

        match_content = nparray[match_index]

        nparray[firstocc:][~match_index[firstocc:]] = np.repeat(match_content,repeat_count)

        self._headers.insert(header_index,title)
        self._running.insert(header_index,np.asarray(nparray))

        for index,column in enumerate(self._running):
            self._running[index] = np.array(self._running[index][firstocc:][~match_index[firstocc:]])

        self.headers = self._headers
        self.running = [np.asarray(column) for column in self._running]

    def texttocolumn(self,header_index=None,header=None,deliminator=None,maxsplit=None):

        if header_index is None:
            header_index = self._headers.index(header)

        header_string = self._headers[header_index]
        # header_string = re.sub(deliminator+'+',deliminator,header_string)

        headers = header_string.split(deliminator)

        if maxsplit is not None:
            for index in range(maxsplit):
                if len(headers)<index+1:
                    headers.append("col ##"+string(header_index+index))

        column_to_split = np.asarray(self._running[header_index])

        running = []

        for index,string in enumerate(column_to_split):

            # string = re.sub(deliminator+'+',deliminator,string)
            row = np.char.split(string,deliminator).tolist()

            if maxsplit is not None:
                while len(row)<maxsplit:
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
        # vmatch = np.vectorize(lambda x: bool(re.compile('[Ab]').match(x)))
        
        self.headers = self._headers
        self.running = [np.asarray(column) for column in self._running]

    def columntotext(self,header_new,header_indices=None,headers=None,string=None):

        if header_indices is None:
            header_indices = [self._headers.index(header) for header in headers]

        if string is None:
            string = ("{} "*len(header_indices)).strip()

        vprint = np.vectorize(lambda *args: string.format(*args))

        column_new = [np.asarray(self._running[index]) for index in header_indices]

        column_new = vprint(*column_new)

        self._headers.append(header_new)
        self._running.append(column_new)

        self.headers = self._headers
        self.running = [np.asarray(column) for column in self._running]

    def astype(self,header_index=None,header=None,dtype=None,datestring=False,shiftmonths=0):

        if header_index is None:
            header_index = self._headers.index(header)

        if datestring:

            def shifting(string):
                date = parse(string)+relativedelta(months=shiftmonths)
                days = calendar.monthrange(date.year,date.month)[1]
                return datetime(date.year,date.month,days)

            if dtype is None:
                if shiftmonths != 0:
                    vdate = np.vectorize(lambda x: shifting(x))
                else:
                    vdate = np.vectorize(lambda x: parse(x))
            else:
                if shiftmonths != 0:
                    vdate = np.vectorize(lambda x: dtype(shifting(x)))
                else:
                    vdate = np.vectorize(lambda x: dtype(parse(x)))
            
        else:
            vdate = np.vectorize(lambda x: dtype(x))
            
        self._running[header_index] = vdate(self._running[header_index])

        self.running[header_index] = np.asarray(self._running[header_index])

    def upper(self,header_index=None,header=None):

        if header_index is None:
            header_index = self._headers.index(header)

        self._running[header_index] = np.char.upper(self._running[header_index])

    def set_column(self,column,header_index=None,header_new=None):
        
        if header_index is None:
            if header_new is None:
                header_new = "Col ##"+str(len(self._headers))

            self._headers.append(header_new)
            self._running.append(column)

            self.headers = self._headers
            self.running.append(np.asarray(self._running[-1]))
        else:
            self._running[header_index] = column

            self.running[header_index] = np.asarray(self._running[header_index])

    def set_rows(self,row,row_indices=None):
        
        if row_indices is None:
            for index,column in enumerate(self._running):
                self._running[index] = np.append(column,row[index])
        else:
            for index, _ in enumerate(self._running):
                self._running[index][row_indices] = row[index]

        self.running = [np.asarray(column) for column in self._running]

    def get_rows(self,row_indices):

        if type(row_indices)==int:
            row_indices = [row_indices]

        rows = [[column[index] for column in self._running] for index in row_indices]
        
        return rows

    def get_columns(self,header_indices=None,headers=None,inplace=False):

        if header_indices is None:
            header_indices = [self._headers.index(header) for header in headers]

        if inplace:
            self._headers = [self._headers[index] for index in header_indices]
            self._running = [self._running[index] for index in header_indices]

            self.headers = self._headers
            self.running = [np.asarray(column) for column in self._running]

        else:
            self.headers = [self._headers[index] for index in header_indices]
            self.running = [np.asarray(self._running[index]) for index in header_indices]

    def del_rows(self,row_indices,inplace=False):

        all_rows = np.array([np.arange(self._running[0].size)])

        row_indices = np.array(row_indices).reshape((-1,1))

        comp_mat = all_rows==row_indices

        keep_index = ~np.any(comp_mat,axis=0)

        if inplace:
            self._running = [column[keep_index] for column in self._running]
            self.running = [np.asarray(column) for column in self._running]
        else:
            self.running = [np.asarray(column[keep_index]) for column in self._running]

    def sort(self,header_indices=None,headers=None,reverse=False,inplace=False,returnFlag=False):

        if header_indices is None:
            header_indices = [self.headers.index(header) for header in headers]

        columns = [self._running[index] for index in header_indices]

        columns.reverse()

        sort_index = np.lexsort(columns)

        if reverse:
            sort_index = np.flip(sort_index)

        if inplace:
            self._running = [column[sort_index] for column in self._running]
            self.running = [np.asarray(column) for column in self._running]
        else:
            self.running = [np.asarray(column[sort_index]) for column in self._running]

        if returnFlag:
            return sort_index

    def filter(self,header_index=None,header=None,keywords=None,regex=None,inplace=False):

        if header_index is None:
            header_index = self._headers.index(header)

        if keywords is not None:
            match_array = np.array(keywords).reshape((-1,1))
            match_index = np.any(self._running[header_index]==match_array,axis=0)
        else:
            match_vectr = np.vectorize(lambda x:bool(re.compile(regex).match(x)))
            match_index = match_vectr(self._running[header_index])

        if inplace:
            self._running = [column[match_index] for column in self._running]
            self.running = [np.asarray(column) for column in self._running]
        else:
            self.running = [np.asarray(column[match_index]) for column in self._running]

    def set_gui(self):

        self.validate_integer = (self.root.register(func_integer),'%P')

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

        self.plotbox = self.canvas.get_tk_widget()

        self.pane_EW.add(self.plotbox,weight=1)

        self.pane_EW.pack(expand=1,fill=tk.BOTH)

        # configuration of top left pane
        self.pane_ns = ttk.PanedWindow(self.frame_side,orient=tk.VERTICAL,width=300)

        self.itembox = AutocompleteEntryListbox(self.frame_side,height=250,padding=0)

        self.itembox.content = self.names.tolist()
        self.itembox.config(completevalues=self.itembox.content,allow_other_values=True)

        self.itembox.listbox.bind('<<ListboxSelect>>',lambda event: self.set_lines(event))

        self.pane_ns.add(self.itembox,weight=1)

        self.tempbox = ttk.Frame(self.frame_side,height=200)

        self.tempbox.rowconfigure(0,weight=0)
        self.tempbox.rowconfigure(1,weight=1)

        self.tempbox.columnconfigure(0,weight=1)
        self.tempbox.columnconfigure(1,weight=0)
        self.tempbox.columnconfigure(2,weight=0)

        self.tempbox.label = ttk.Label(self.tempbox,text="Templates")
        self.tempbox.label.grid(row=0,column=0,sticky=tk.EW)

        self.tempbox.iconadd = tk.PhotoImage(file=os.path.join(self.dirname,"graphics","Add","Add-9.png"))
        self.tempbox.iconedit = tk.PhotoImage(file=os.path.join(self.dirname,"graphics","Edit","Edit-9.png"))
        self.tempbox.icondel = tk.PhotoImage(file=os.path.join(self.dirname,"graphics","Delete","Delete-9.png"))

        self.tempbox.buttonadd = ttk.Button(self.tempbox,image=self.tempbox.iconadd,command=self.add_temp)
        self.tempbox.buttonadd.grid(row=0,column=1)

        self.tempbox.buttonedit = ttk.Button(self.tempbox,image=self.tempbox.iconedit,command=self.edit_temp)
        self.tempbox.buttonedit.grid(row=0,column=2)

        self.tempbox.buttondel = ttk.Button(self.tempbox,image=self.tempbox.icondel,command=self.del_temp)
        self.tempbox.buttondel.grid(row=0,column=3)

        self.tempbox.listbox = tk.Listbox(self.tempbox,exportselection=False)
        self.tempbox.listbox.grid(row=1,column=0,columnspan=4,sticky=tk.NSEW)

        for template in self.templates:
            self.tempbox.listbox.insert(tk.END,template.get("name"))

        self.curtemp = {}

        self.tempbox.listbox.bind('<<ListboxSelect>>',lambda event: self.set_axes(event))

        self.pane_ns.add(self.tempbox,weight=1)

        self.pane_ns.pack(expand=1,fill=tk.BOTH)

    def set_axes(self,event):

        if not self.tempbox.listbox.curselection():
            return

        if self.curtemp == self.templates[self.tempbox.listbox.curselection()[0]]:
            return
        
        self.curtemp = self.templates[self.tempbox.listbox.curselection()[0]]

        axisx = self.curtemp.get("subplots")[0]
        axisy = self.curtemp.get("subplots")[1]

        twinx = self.curtemp.get("twinx")

        self.curtemp["flagMainAxes"] = []

        for flagTwinAxis in twinx:
            self.curtemp["flagMainAxes"].append(True)
            if flagTwinAxis: self.curtemp["flagMainAxes"].append(False)

        if hasattr(self,"axes"):
            [self.figure.delaxes(axis) for axis in self.axes]

        self.axes = []

        for index,flagMainAxis in enumerate(self.curtemp.get("flagMainAxes")):

            index_main = sum(self.curtemp.get("flagMainAxes")[:index+1])-1

            if flagMainAxis:
                axis = self.figure.add_subplot(axisx,axisy,index_main+1)
            else:
                axis = self.axes[-1].twinx()
                
            if flagMainAxis and self.curtemp.get("title")[index_main] is not None:
                axis.set_title(self.curtemp.get("title")[index_main])

            if flagMainAxis and self.curtemp.get("xlabel")[index_main] is not None:
                axis.set_xlabel(self.curtemp.get("xlabel")[index_main])

            if self.curtemp.get("ylabel")[index] is not None:
                axis.set_ylabel(self.curtemp.get("ylabel")[index])

            if flagMainAxis and self.curtemp.get("xticks")[index_main] is not None:
                axis.set_xticks(self.curtemp.get("xticks")[index_main])

            if self.curtemp.get("yticks")[index] is not None:
                axis.set_yticks(self.curtemp.get("yticks")[index])

            if flagMainAxis and self.curtemp.get("grid")[index_main] is not None:
                axis.grid(self.curtemp.get("grid")[index_main])

            self.axes.append(axis)

            # for tick in axis0.get_xticklabels():
            #     tick.set_rotation(45)

        status = "{} template has been selected.".format(self.curtemp.get("name"))

        self.footer.insert(tk.END,status)
        self.footer.see(tk.END)

        self.figure.set_tight_layout(True)

        self.canvas.draw()

    def set_lines(self,event):

        if not self.itembox.listbox.curselection():
            return

        if not hasattr(self,"axes"):
            status = "No template has been selected."
            self.footer.insert(tk.END,status)
            self.footer.see(tk.END)
            return

        self.filter(0,keywords=[self.names[self.itembox.listbox.curselection()[0]]],inplace=False)

        if hasattr(self,"lines"):
            [line.remove() for line in self.lines]
                
        self.lines = []

        for index,axis in enumerate(self.axes):
            xaxes = self.curtemp.get("xaxes")[index]
            yaxes = self.curtemp.get("yaxes")[index]
            colors = self.curtemp.get("colors")[index]
            for xaxis,yaxis,color in zip(xaxes,yaxes,colors):
                line = axis.plot(self.running[xaxis],self.running[yaxis],c=color,label=self.headers[yaxis])[0]
                self.lines.append(line)
            if self.curtemp.get("legends")[index]:
                axis.legend()
            axis.relim()
            axis.autoscale_view()
            axis.set_ylim(bottom=0,top=None,auto=True)

        self.figure.set_tight_layout(True)

        self.canvas.draw()

    def add_temp(self):

        self.set_temptop()

    def edit_temp(self):

        if not self.tempbox.listbox.curselection(): return
        
        name = self.tempbox.listbox.get(self.tempbox.listbox.curselection())

        item = self.tempbox.get("names").index(name)

        self.set_temptop(item=item)

    def del_temp(self):

        if not self.tempbox.listbox.curselection(): return

        name = self.tempbox.listbox.get(self.tempbox.listbox.curselection())

        item = self.curtemp.get("name").index(name)
        
        self.tempbox.listbox.delete(item)

        self.curtemp.get("name").pop(item)
        # self.curtemp.get("xnumgrid").pop(item)
        # self.curtemp.get("ynumgrid").pop(item)

    def set_temptop(self,item=None):

        if hasattr(self,"temptop"):
            if self.temptop.winfo_exists(): return

        self.temptop = tk.Toplevel()

        self.temptop.geometry("700x400")

        self.temptop.resizable(0,0)

        self.style = ttk.Style(self.temptop)

        self.style.configure("TNotebook.Tab",width=20,anchor=tk.CENTER)

        self.topTemp = ttk.Notebook(self.temptop)

        self.topTempAxis = tk.Frame(self.topTemp)

        self.topTempAxisFrame0 = tk.Frame(self.topTempAxis,borderwidth=2,relief=tk.GROOVE)

        self.topTempAxisFrame0.tempnameLabel = ttk.Label(self.topTempAxisFrame0,text="Template Name")
        self.topTempAxisFrame0.tempnameLabel.grid(row=0,column=0,padx=(10,10),pady=(20,2))

        self.topTempAxisFrame0.tempname = ttk.Entry(self.topTempAxisFrame0,width=30)
        self.topTempAxisFrame0.tempname.grid(row=0,column=1,padx=(10,20),pady=(20,2),sticky=tk.EW)

        self.topTempAxisFrame0.tempname.focus()

        self.topTempAxisFrame0.legendLabel = ttk.Label(self.topTempAxisFrame0,text="Legend Position")
        self.topTempAxisFrame0.legendLabel.grid(row=1,column=0,padx=(10,10),pady=(2,2))

        self.topTempAxisFrame0.legend = ttk.Entry(self.topTempAxisFrame0,width=30)
        self.topTempAxisFrame0.legend.grid(row=1,column=1,padx=(10,20),pady=(2,2),sticky=tk.EW)

        self.topTempAxisFrame0.pack(side=tk.LEFT,expand=0,fill=tk.Y)

        self.topTempAxisFrame1 = tk.Frame(self.topTempAxis)

        self.topTempAxisFrame1.xgridlabel = ttk.Label(self.topTempAxisFrame1,text="Grids in Y")
        self.topTempAxisFrame1.xgridlabel.grid(row=0,column=0,sticky=tk.EW,padx=(10,10),pady=(20,2))

        self.topTempAxisFrame1.xnumgrid = ttk.Entry(self.topTempAxisFrame1,width=10,validate="key",validatecommand=self.validate_integer)
        self.topTempAxisFrame1.xnumgrid.grid(row=0,column=1,sticky=tk.EW,padx=(10,2),pady=(20,2))

        self.topTempAxisFrame1.ygridlabel = ttk.Label(self.topTempAxisFrame1,text="Grids in X")
        self.topTempAxisFrame1.ygridlabel.grid(row=1,column=0,sticky=tk.EW,padx=(10,10),pady=(2,2))

        self.topTempAxisFrame1.ynumgrid = ttk.Entry(self.topTempAxisFrame1,width=10,validate="key",validatecommand=self.validate_integer)
        self.topTempAxisFrame1.ynumgrid.grid(row=1,column=1,sticky=tk.EW,padx=(10,2),pady=(2,2))

        self.topTempAxisFrame1.pack(side=tk.LEFT,expand=1,fill=tk.BOTH)
        
        self.topTemp.add(self.topTempAxis,text="Template Options",compound=tk.CENTER)

        self.topTempLine = tk.Frame(self.topTemp)

        self.topTemp.add(self.topTempLine,text="Line Options",compound=tk.CENTER)

        self.topTemp.pack(side=tk.TOP,expand=1,fill=tk.BOTH,padx=(0,1))

        if item is not None:

            tempname = self.curtemp.get("name")[item]
            # xnumgrid = self.curtemp.get("xnumgrid")[item]
            # ynumgrid = self.curtemp.get("ynumgrid")[item]

            self.topTempAxisFrame.tempname.insert(0,tempname)
            self.topTempAxisFrame.xnumgrid.insert(0,xnumgrid)
            self.topTempAxisFrame.ynumgrid.insert(0,ynumgrid)

        buttonname = "Add Template" if item is None else "Edit Template"

        self.temptop.button = ttk.Button(self.temptop,text=buttonname,width=20,command=lambda: self.topTempButtonApply(item))
        self.temptop.button.pack(side=tk.TOP,anchor=tk.E,padx=(0,1),pady=(1,1))

        self.temptop.button.bind('<Return>',lambda event: self.topTempButtonApply(item,event))

        self.temptop.mainloop()

    def topTempButtonApply(self,item=None,event=None):

        if event is not None and event.widget!=self.temptop.button:
            return

        if item is not None:
            names = [name for index,name in enumerate(self.curtemp.get("name")) if index!=item]
        else:
            names = self.curtemp.get("name")

        name = self.topTempAxisFrame0.tempname.get()

        if name in names:
            tk.messagebox.showerror("Error","You have a template with the same name!",parent=self.temptop)
            return
        elif name.strip()=="":
            tk.messagebox.showerror("Error","You have not named the template!",parent=self.temptop)
            return

        if item is None:
            item = len(self.temps.get("names"))
        else:
            self.tempbox.listbox.delete(item)
            self.curtemp.get("name").pop(item)
            # self.curtemp.get("xnumgrid").pop(item)
            # self.curtemp.get("ynumgrid").pop(item)
        
        self.tempbox.listbox.insert(item,name)

        self.curtemp.get("name").insert(item,name)

        try:
            xnumgrid = int(self.topTempAxisFrame1.xnumgrid.get())
        except ValueError:
            xnumgrid = 1

        # self.curtemp.get("xnumgrid").insert(item,xnumgrid)

        try:
            ynumgrid = int(self.topTempAxisFrame1.ynumgrid.get())
        except ValueError:
            ynumgrid = 1

        # self.curtemp.get("ynumgrid").insert(item,ynumgrid)

        self.temptop.destroy()

    def write(self,filepath,header_indices=None,headers=None,string=None,**kwargs):

        if header_indices is None:
            header_indices = [self._headers.index(header) for header in headers]

        if string is None:
            string = ("{}\t"*len(header_indices))[:-1]+"\n"

        vprint = np.vectorize(lambda *args: string.format(*args))

        columns = [np.asarray(self._running[index]) for index in header_indices]

        with open(filepath,"w",encoding='utf-8') as wfile:
            for line in vprint(*columns):
                wfile.write(line)

def writexlsx(filepath,**kwargs):

    wb = openpyxl.Workbook()

    sheet = wb.active

    if sheet_title is not None:
        sheet.title = sheet_title

    for line in running:
        sheet.append(line)

    wb.save(filepath)

def writevtk(frac,time,solution):

    pass

    # # deleteing files in results file
    
    # delete 'results\*.fig'
    # delete 'results\*.vtk'
    # delete 'results\*.out'
    
    # # conversion to field units
    
    # T = time.tau/setup.convFactorDetermine('time');
    
    # Pf = sol.pressure/setup.convFactorDetermine('pressure');
    # # Qf = sol.fracflux/setup.convFactorDetermine('velocity');
    
    # Pw = sol.wellpressure/setup.convFactorDetermine('pressure');
    # Qw = sol.wellflowrate/setup.convFactorDetermine('flowrate');
    
    # # writing time values of well pressure and flowrate
    
    # fid = fopen('results\solution.out','w');
    
    # fprintf(fid,'FRACTURE FLOW ANALYTICAL SOLUTION\r\n');
    # fprintf(fid,'WELL PRESSURE AND FLOW-RATE\r\n');
    # fprintf(fid,'\r\n%-10s\t%-10s\t%-10s\r\n','Time','Pressure','Flow-Rate');
    # fprintf(fid,'%-10s\t%-10s\t%-10s\r\n','[days]','[psi]','[bbl/day]');
    
    # fclose(fid);
    
    # dlmwrite('results\solution.out',[T,Pw',Qw'],'-append',...
    #          'delimiter','\t','precision','%-10.3f');
    
    # # writing time values of fracture pressure
    
    # for j = 1:time.numTimeStep
    
    #     fid = fopen(['results\fracPressure',num2str(j),'.vtk'],'w');

    #     fprintf(fid,'# vtk DataFile Version 1.0\r\n');
    #     fprintf(fid,'FRACTURE FLOW ANALYTICAL SOLUTION\r\n');
    #     fprintf(fid,'ASCII\r\n');

    #     fprintf(fid,'\r\nDATASET UNSTRUCTURED_GRID\r\n');

    #     fprintf(fid,'\r\nPOINTS %d FLOAT\r\n',frac.numAnode*2);

    #     for i = 1:frac.numAnode
    #         fprintf(fid,'%f %f %f\r\n',frac.nodeCoord(i,:));
    #     end

    #     for i = 1:frac.numAnode
    #         fprintf(fid,'%f %f %f\r\n',[frac.nodeCoord(i,1:2),0]);
    #     end

    #     fprintf(fid,'\r\nCELLS %d %d\r\n',frac.numAfrac,5*frac.numAfrac);

    #     for i = 1:frac.numAfrac
    #         fprintf(fid,'%d %d %d %d %d\r\n',[4,frac.map(i,:)-1,frac.map(i,:)+frac.numAnode-1]);
    #     end

    #     fprintf(fid,'\r\nCELL_TYPES %d\r\n',frac.numAfrac);

    #     for i = 1:frac.numAfrac
    #         fprintf(fid,'%d\r\n',8);
    #     end

    #     fprintf(fid,'\r\nCELL_DATA %d\r\n',frac.numAfrac);
    #     fprintf(fid,'SCALARS pressure float\r\n');
    #     fprintf(fid,'LOOKUP_TABLE default\r\n');

    #     for i = 1:frac.numAfrac
    #         fprintf(fid,'%f\r\n',Pf(i,j));
    #     end

    #     fclose(fid);

def cyrilictolatin(string):

    """best it can be done with regular expressions"""
    
    string = string.replace("а","a")
    string = string.replace("б","b")
    string = string.replace("ж","c")
    string = string.replace("ч","ç")
    string = string.replace("д","d")
    string = string.replace("е","e")
    string = string.replace("я","ə")
    string = string.replace("ф","f")
    string = string.replace("э","g")
    string = string.replace("ь","ğ")
    string = string.replace("щ","h")
    string = string.replace("х","x")
    string = string.replace("ы","ı")
    string = string.replace("и","i")
    string = string.replace("ъ","j")
    string = string.replace("к","k")
    string = string.replace("г","q")
    string = string.replace("л","l")
    string = string.replace("м","m")
    string = string.replace("н","n")
    string = string.replace("о","o")
    string = string.replace("ю","ö")
    string = string.replace("п","p")
    string = string.replace("р","r")
    string = string.replace("с","s")
    string = string.replace("ш","ş")
    string = string.replace("т","t")
    string = string.replace("у","u")
    string = string.replace("ц","ü")
    string = string.replace("в","v")
    string = string.replace("й","y")
    string = string.replace("з","z")

    string = string.replace("А","A")
    string = string.replace("Б","B")
    string = string.replace("Ҹ","C")
    string = string.replace("Ч","Ç")
    string = string.replace("Д","D")
    string = string.replace("Е","E")
    string = string.replace("Я","Ə")
    string = string.replace("Ф","F")
    string = string.replace("Ҝ","G")
    string = string.replace("Ғ","Ğ")
    string = string.replace("Щ","H")
    string = string.replace("Х","X")
    string = string.replace("Ы","I")
    string = string.replace("И","İ")
    ##string = string.replace("я","J")
    string = string.replace("К","K")
    string = string.replace("Г","Q")
    ##string = string.replace("я","L")
    ##string = string.replace("я","M")
    string = string.replace("Н","N")
    ##string = string.replace("я","O")
    ##string = string.replace("я","Ö")
    string = string.replace("П","P")
    string = string.replace("Р","R")
    string = string.replace("С","S")
    string = string.replace("Ш","Ş")
    ##string = string.replace("я","T")
    ##string = string.replace("я","U")
    ##string = string.replace("я","Ü")
    string = string.replace("В","V")
    string = string.replace("Й","Y")
    string = string.replace("З","Z")

    return string

if __name__ == "__main__":

    import interfaces.tests

    window = tk.Tk()

    data = dataset(window=window,filepath=os.path.join(os.path.dirname(__file__),"tests","datatest"),skiplines=1)

    data.texttocolumn(0,deliminator="\t")

    data.astype(1,dtype=np.float64)
    data.astype(2,dtype=np.float64)
    data.astype(3,dtype=np.float64)
    data.astype(4,dtype=np.float64)

    data.names = np.unique(data.running[0])

    data.set_gui()

    window.mainloop()
