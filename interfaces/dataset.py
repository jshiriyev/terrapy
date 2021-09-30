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
    "legends": [True],
    "xticks": [None],
    "yticks": [None],
    "grid": [True],
    #
    "sublines": [[3]],
    "xaxes": [[1,1,1]],
    "yaxes": [[2,3,4]],
    "drawstyles": [[0,0,0]],
    "linestyles": [[0,1,0]],
    "linecolors": [[6,0,2]],
    }

temp1 = {
    "name": "Standard-dual horizontal stack",
    "subplots": [1,2],
    "twinx": [False,False],
    "title": ["Left","Right"],
    "xlabel": ["x-axis","x-axis"],
    "ylabel": ["y-axis","y-axis"],
    "legends": [True,True],
    "xticks": [None,None],
    "yticks": [None,None],
    "grid": [True,True],
    #
    "sublines": [[1],[2]],
    "xaxes": [[1],[1,1]],
    "yaxes": [[2],[3,4]],
    "drawstyles": [[0],[0,0]],
    "linestyles": [[0],[None,None]],
    "linecolors": [[6],[0,2]],
    }

temp2 = {
    "name": "Standard-dual vertical stack",
    "subplots": [2,1],
    "twinx": [False,False],
    "title": ["Top","Bottom"],
    "xlabel": ["x-axis","x-axis"],
    "ylabel": ["y-axis","y-axis"],
    "legends": [True,True],
    "xticks": [None,None],
    "yticks": [None,None],
    "grid": [True,True],
    #
    "sublines": [[1],[2]],
    "xaxes": [[1],[1,1]],
    "yaxes": [[2],[3,4]],
    "drawstyles": [[4],[0,0]],
    "linestyles": [[0],[]],
    "linecolors": [[6],[0,2]],
    }

temp3 = {
    "name": "Standard-quadruple",
    "subplots": [2,2],
    "twinx": [False,False,False,False],
    "title": ["NW","NE","SW","SE"],
    "xlabel": ["x-axis","x-axis","x-axis","x-axis"],
    "ylabel": ["y-axis","y-axis","y-axis","y-axis"],
    "legends": [True,True,True,False],
    "xticks": [None,None,None,None],
    "yticks": [None,None,None,None],
    "grid": [True,True,True,True],
    #
    "sublines": [[1],[1],[1],[0]],
    "xaxes": [[1],[1],[1],[]],
    "yaxes": [[2],[3],[4],[]],
    "drawstyles": [[0],[0],[0],[0]],
    "linestyles": [[0],[0],[0],[]],
    "linecolors": [[6],[0],[2],[]],
    }

class dataset():

    special_extensions = (
        ".db",".xlsx",".las",
        )

    legendpos = (
        "best","right",
        "upper left","upper center","upper right",
        "lower left","lower center","lower right",
        "center left","center","center right",
        )

    drawstyles = (
        'default','steps','steps-pre','steps-mid','steps-post',
        )

    linestyles = (
        "({}) solid".format('-'),
        "({}) dashed".format('--'),
        "({}) dash dot".format('-.'),
        "({}) dotted".format(':'),
        "({}) point marker".format('.'),
        "({}) pixel marker".format(','),
        "({}) circle marker".format('o'),
        "({}) triangle_down marker".format('v'),
        "({}) triangle_up marker".format('^'),
        "({}) triangle_left marker".format('<'),
        "({}) triangle_right marker".format('>'),
        "({}) tri_down marker".format('1'),
        "({}) tri_up marker".format('2'),
        "({}) tri_left marker".format('3'),
        "({}) tri_right marker".format('4'),
        "({}) square marker".format('s'),
        "({}) pentagon marker".format('p'),
        "({}) star marker".format('*'),
        "({}) hexagon1 marker".format('h'),
        "({}) hexagon2 marker".format('H'),
        "({}) plus marker".format('+'),
        "({}) x marker".format('x'),
        "({}) diamond marker".format('D'),
        "({}) thin_diamond marker".format('d'),
        "({}) vline marker".format('|'),
        "({}) hline marker".format('_'),
        )

    linecolors = (
        "b: blue",
        "g: green",
        "r: red",
        "c: cyan",
        "m: magenta",
        "y: yellow",
        "k: black",
        "w: white",
        )

    templates = (
        temp0,temp1,temp2,temp3,
        )

    def __init__(self,window=None,headers=None,filepath=None,skiplines=0,headerline=None,comment=None,endline=None,endfile=None,**kwargs):

        # There are two visualization options:
        #   case 1: tabulating
        #   case 2: plotting allowing also templates

        # There are two uses of dataset, case 1 or case 2:
        #   case 1: headers
        #   case 2: filepath,skiplines,headerline
        #    - reading plain text: comment,endline,endfile
        #    - reading scpecial extensions: **kwargs
        #   case 3: no input

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
            self._headers = []
            self._running = []

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

        if header_new is None:
            header_new = "Col ##"+str(len(self._headers))
        
        if header_index is None or header_index==-1:
            self._headers.append(header_new)
            self._running.append(column)
        else:
            self._running[header_index] = column

        self.headers = self._headers
        self.running = [np.asarray(column) for column in self._running]

    def set_rows(self,rows,row_indices=None):
        
        for row in rows:

            if row_indices is None:
                for index,column in enumerate(self._running):
                    self._running[index] = np.append(column,row[index])
            else:
                for index, _ in enumerate(self._running):
                    self._running[index][row_indices] = row[index]

            self.running = [np.asarray(column) for column in self._running]

    def get_rows(self,row_indices=None):

        if row_indices is None:
            row_indices = range(self._running[0].size)
        elif type(row_indices)==int:
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

    def write(self,filepath,fstring=None,**kwargs):

        header_fstring = ("{}\t"*len(self._headers))[:-1]+"\n"

        if fstring is None:
            running_fstring = ("{}\t"*len(self._headers))[:-1]+"\n"
        else:
            running_fstring = fstring

        vprint = np.vectorize(lambda *args: running_fstring.format(*args))

        with open(filepath,"w",encoding='utf-8') as wfile:
            wfile.write(header_fstring.format(*self._headers))
            for line in vprint(*self._running):
                wfile.write(line)

    def set_graphtop(self):

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

        self.tempbox.label = ttk.Label(self.tempbox,text="Graph Templates")
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

        naxrows,naxcols = self.curtemp.get("subplots")

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
                axis = self.figure.add_subplot(naxrows,naxcols,index_main+1)
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
            drawstyles = self.curtemp.get("drawstyles")[index]
            linestyles = self.curtemp.get("linestyles")[index]
            linecolors = self.curtemp.get("linecolors")[index]
            for xaxis,yaxis,dstyle,lstyle,color in zip(xaxes,yaxes,drawstyles,linestyles,linecolors):
                line = axis.plot(
                    self.running[xaxis],
                    self.running[yaxis],
                    drawstyle=dstyle,
                    linestyle=lstyle,
                    c=color,
                    label=self.headers[yaxis])[0]
                self.lines.append(line)
            if self.curtemp.get("legends")[index]:
                axis.legend()
            axis.relim()
            axis.autoscale_view()
            axis.set_ylim(bottom=0,top=None,auto=True)

        self.figure.set_tight_layout(True)

        self.canvas.draw()

    def get_temptoset(self,manipulation="add"):

        if manipulation=="add":

            self.curtemp = {# when creating a template
                "name": "",
                "subplots": [1,1],
                "title": [""],
                "twinx": [False],
                "xlabel": [""],
                "ylabel": [""],
                "legends": [False],
                "xticks": [None],
                "yticks": [None],
                "grid": [False],
                #
                "sublines": [[0]],
                "xaxes": [[]],
                "yaxes": [[]],
                "drawstyles": [[]],
                "linestyles": [[]],
                "linecolors": [[]],
                }

            self.set_temptop() # when adding a new one

        elif manipulation=="edit":

            if not self.tempbox.listbox.curselection(): return # when editing

            self.curtemp = self.templates[self.tempbox.listbox.curselection()[0]] # when editing
            self.set_temptop(tempid=self.tempbox.listbox.curselection()[0]) # editing the existing one
            
        elif manipulation=="delete"
            # deleting a template

            if not self.tempbox.listbox.curselection(): return

            name = self.tempbox.listbox.get(self.tempbox.listbox.curselection())

            item = self.curtemp.get("name").index(name)
            
            self.tempbox.listbox.delete(item)

            self.curtemp.get("name").pop(item)
            # self.curtemp.get("naxrows").pop(item)
            # self.curtemp.get("naxcols").pop(item)

    def set_temptop(self,manipulation):

        if hasattr(self,"temptop"):
            if self.temptop.winfo_exists(): return

        if tempid is not None:
            curtemp = self.templates[tempid]
        else:
            curtemp = {"subplots": [1,1]}
            curtemp["sublines"] = [[1,0]]

        self.temptop = tk.Toplevel()

        self.temptop.title("Template Editor")

        self.temptop.geometry("700x450")

        self.temptop.resizable(0,0)

        self.style = ttk.Style(self.temptop)

        self.style.configure("TNotebook.Tab",width=15,anchor=tk.CENTER)

        self.tempedit = ttk.Notebook(self.temptop)

        # General Properties

        self.tempeditgeneral = tk.Frame(self.tempedit)

        self.tempeditgeneral0 = tk.Frame(self.tempeditgeneral,borderwidth=2,relief=tk.GROOVE)

        self.tempeditgeneral0.templabel = ttk.Label(self.tempeditgeneral0,text="Settings")
        self.tempeditgeneral0.templabel.grid(row=0,column=0,columnspan=2,sticky=tk.W,padx=(10,10),pady=(2,2))

        self.tempeditgeneral0.tempnamelabel = ttk.Label(self.tempeditgeneral0,text="Template Name")
        self.tempeditgeneral0.tempnamelabel.grid(row=1,column=0,sticky=tk.E,padx=(10,10),pady=(2,2))

        self.tempeditgeneral0.tempname = ttk.Entry(self.tempeditgeneral0,width=30)
        self.tempeditgeneral0.tempname.grid(row=1,column=1,padx=(0,20),pady=(2,2),sticky=tk.EW)

        self.tempeditgeneral0.tempname.focus()

        self.tempeditgeneral0.legendLabel = ttk.Label(self.tempeditgeneral0,text="Legend Position")
        self.tempeditgeneral0.legendLabel.grid(row=2,column=0,sticky=tk.E,padx=(10,10),pady=(2,2))

        self.tempeditgeneral0.legend = ttk.Entry(self.tempeditgeneral0,width=30)
        self.tempeditgeneral0.legend.grid(row=2,column=1,padx=(0,20),pady=(2,2),sticky=tk.EW)

        self.tempeditgeneral0.pack(side=tk.LEFT,expand=0,fill=tk.Y)

        self.tempeditgeneral1 = tk.Frame(self.tempeditgeneral,borderwidth=2)

        self.tempeditgeneral1.naxlabel = ttk.Label(self.tempeditgeneral1,text="Number of Axes")
        self.tempeditgeneral1.naxlabel.grid(row=0,column=0,columnspan=2,sticky=tk.EW,padx=(10,2),pady=(2,2))

        self.tempeditgeneral1.naxrowslabel = ttk.Label(self.tempeditgeneral1,text="Rows")
        self.tempeditgeneral1.naxrowslabel.grid(row=1,column=0,sticky=tk.E,padx=(10,10),pady=(2,2))

        self.tempeditgeneral1.naxval0 = tk.StringVar(self.root)
        self.tempeditgeneral1.naxrows = ttk.Spinbox(self.tempeditgeneral1,textvariable=self.tempeditgeneral1.naxval0,from_=1,to=5,command=lambda:self.set_temptopdict("rows"))
        self.tempeditgeneral1.naxrows.grid(row=1,column=1,sticky=tk.EW,padx=(0,2),pady=(2,2))

        self.tempeditgeneral1.naxcolslabel = ttk.Label(self.tempeditgeneral1,text="Columns")
        self.tempeditgeneral1.naxcolslabel.grid(row=2,column=0,sticky=tk.E,padx=(10,10),pady=(2,2))

        self.tempeditgeneral1.naxval1 = tk.StringVar(self.root)
        self.tempeditgeneral1.naxcols = ttk.Spinbox(self.tempeditgeneral1,textvariable=self.tempeditgeneral1.naxval1,from_=1,to=5,command=lambda:self.set_temptopdict("columns"))
        self.tempeditgeneral1.naxcols.grid(row=2,column=1,sticky=tk.EW,padx=(0,2),pady=(2,2))

        self.tempeditgeneral1.pack(side=tk.LEFT,expand=1,fill=tk.BOTH)
        
        self.tempedit.add(self.tempeditgeneral,text="General",compound=tk.CENTER)

        # Axes Properties

        self.tempeditaxes = tk.Frame(self.tempedit)

        self.tempeditaxes0 = tk.Frame(self.tempeditaxes,borderwidth=2,relief=tk.GROOVE)

        self.tempeditaxes0.axislabel = ttk.Label(self.tempeditaxes0,text="Axis List")
        self.tempeditaxes0.axislabel.pack(side=tk.TOP,fill=tk.X)

        self.tempeditaxes0.listbox = tk.Listbox(self.tempeditaxes0)

        self.tempeditaxes0.listbox.pack(side=tk.TOP,expand=1,fill=tk.BOTH)

        self.tempeditaxes0.pack(side=tk.LEFT,expand=0,fill=tk.Y)

        self.tempeditaxes1 = tk.Frame(self.tempeditaxes,borderwidth=2,relief=tk.GROOVE)

        self.tempeditaxes1.entry00 = tk.IntVar(self.root)
        self.tempeditaxes1.check00 = ttk.Checkbutton(self.tempeditaxes1,text="Draw X Twin",variable=self.tempeditaxes1.entry00,command=lambda:self.set_temptopdict("entry00"))
        self.tempeditaxes1.check00.grid(row=0,column=0,sticky=tk.EW,padx=(10,),pady=(4,))

        self.tempeditaxes1.label01 = ttk.Label(self.tempeditaxes1,text="Title")
        self.tempeditaxes1.label01.grid(row=1,column=0,sticky=tk.EW,padx=(30,),pady=(4,))
        self.tempeditaxes1.entry01 = ttk.Entry(self.tempeditaxes1)
        self.tempeditaxes1.entry01.grid(row=1,column=1,sticky=tk.EW,padx=(0,10),pady=(4,))

        self.tempeditaxes1.label02 = ttk.Label(self.tempeditaxes1,text="X Label")
        self.tempeditaxes1.label02.grid(row=2,column=0,sticky=tk.EW,padx=(30,),pady=(4,))
        self.tempeditaxes1.entry02 = ttk.Entry(self.tempeditaxes1)
        self.tempeditaxes1.entry02.grid(row=2,column=1,sticky=tk.EW,padx=(0,10),pady=(4,))

        self.tempeditaxes1.label03 = ttk.Label(self.tempeditaxes1,text="Y-1 Label")
        self.tempeditaxes1.label03.grid(row=3,column=0,sticky=tk.EW,padx=(30,),pady=(4,))
        self.tempeditaxes1.entry03 = ttk.Entry(self.tempeditaxes1,state=tk.NORMAL)
        self.tempeditaxes1.entry03.grid(row=3,column=1,sticky=tk.EW,padx=(0,10),pady=(4,))

        self.tempeditaxes1.label04 = ttk.Label(self.tempeditaxes1,text="Y-2 Label",state=tk.DISABLED)
        self.tempeditaxes1.label04.grid(row=4,column=0,sticky=tk.EW,padx=(30,),pady=(4,))
        self.tempeditaxes1.entry04 = ttk.Entry(self.tempeditaxes1,state=tk.DISABLED)
        self.tempeditaxes1.entry04.grid(row=4,column=1,sticky=tk.EW,padx=(0,10),pady=(4,))

        self.tempeditaxes1.entry05 = tk.StringVar(self.root)
        self.tempeditaxes1.label05 = ttk.Label(self.tempeditaxes1,text="Y-1 Lines")
        self.tempeditaxes1.label05.grid(row=5,column=0,sticky=tk.EW,padx=(30,),pady=(4,))
        self.tempeditaxes1.spinb05 = ttk.Spinbox(self.tempeditaxes1,to=20,textvariable=self.tempeditaxes1.entry05,command=lambda:self.set_temptopdict("entry05"))
        self.tempeditaxes1.spinb05.grid(row=5,column=1,sticky=tk.EW,padx=(0,10),pady=(4,))

        self.tempeditaxes1.entry06 = tk.StringVar(self.root)
        self.tempeditaxes1.label06 = ttk.Label(self.tempeditaxes1,text="Y-2 Lines",state=tk.DISABLED)
        self.tempeditaxes1.label06.grid(row=6,column=0,sticky=tk.EW,padx=(30,),pady=(4,))
        self.tempeditaxes1.spinb06 = ttk.Spinbox(self.tempeditaxes1,to=20,textvariable=self.tempeditaxes1.entry06,command=lambda:self.set_temptopdict("entry06"),state=tk.DISABLED)
        self.tempeditaxes1.spinb06.grid(row=6,column=1,sticky=tk.EW,padx=(0,10),pady=(4,))

        self.tempeditaxes1.entry07 = tk.IntVar(self.root)
        self.tempeditaxes1.check07 = ttk.Checkbutton(self.tempeditaxes1,text="Show Legends",variable=self.tempeditaxes1.entry07)
        self.tempeditaxes1.check07.grid(row=7,column=0,sticky=tk.EW,padx=(10,),pady=(4,))

        self.tempeditaxes1.entry08 = tk.IntVar(self.root)
        self.tempeditaxes1.check08 = ttk.Checkbutton(self.tempeditaxes1,text="Show X Ticks",variable=self.tempeditaxes1.entry08)
        self.tempeditaxes1.check08.grid(row=8,column=0,sticky=tk.EW,padx=(10,),pady=(4,))

        self.tempeditaxes1.entry09 = tk.IntVar(self.root)
        self.tempeditaxes1.check09 = ttk.Checkbutton(self.tempeditaxes1,text="Show Y-1 Ticks",variable=self.tempeditaxes1.entry09)
        self.tempeditaxes1.check09.grid(row=9,column=0,sticky=tk.EW,padx=(10,),pady=(4,))

        self.tempeditaxes1.entry10 = tk.IntVar(self.root)
        self.tempeditaxes1.check10 = ttk.Checkbutton(self.tempeditaxes1,text="Show Y-2 Ticks",variable=self.tempeditaxes1.entry10,state=tk.DISABLED)
        self.tempeditaxes1.check10.grid(row=10,column=0,sticky=tk.EW,padx=(10,),pady=(4,))

        self.tempeditaxes1.entry11 = tk.IntVar(self.root)
        self.tempeditaxes1.check11 = ttk.Checkbutton(self.tempeditaxes1,text="Show Grids",variable=self.tempeditaxes1.entry11)
        self.tempeditaxes1.check11.grid(row=11,column=0,sticky=tk.EW,padx=(10,),pady=(4,))

        self.tempeditaxes1.pack(side=tk.LEFT,expand=1,fill=tk.BOTH)

        self.tempedit.add(self.tempeditaxes,text="Axes",compound=tk.CENTER)

        # Line Properties

        self.tempeditlines = tk.Frame(self.tempedit)

        self.tempeditlines0 = tk.Frame(self.tempeditlines,borderwidth=2,relief=tk.GROOVE)

        self.tempeditlines0.axislabel = ttk.Label(self.tempeditlines0,text="Axis List")
        self.tempeditlines0.axislabel.pack(side=tk.TOP,fill=tk.X)

        self.tempeditlines0.listbox = tk.Listbox(self.tempeditlines0)

        self.tempeditlines0.listbox.pack(side=tk.TOP,expand=1,fill=tk.BOTH)

        self.tempeditlines0.pack(side=tk.LEFT,expand=0,fill=tk.Y)

        self.tempeditlines1 = tk.Frame(self.tempeditlines,borderwidth=2,relief=tk.GROOVE)

        self.tempeditlines1.line1label = ttk.Label(self.tempeditlines1,text="Y-1 Lines")
        self.tempeditlines1.line1label.pack(side=tk.TOP,fill=tk.X)

        self.tempeditlines1.listbox1 = tk.Listbox(self.tempeditlines1)

        self.tempeditlines1.listbox1.pack(side=tk.TOP,expand=1,fill=tk.BOTH)

        self.tempeditlines1.line2label = ttk.Label(self.tempeditlines1,text="Y-2 Lines")
        self.tempeditlines1.line2label.pack(side=tk.TOP,fill=tk.X)

        self.tempeditlines1.listbox2 = tk.Listbox(self.tempeditlines1)

        self.tempeditlines1.listbox2.pack(side=tk.TOP,expand=1,fill=tk.BOTH)

        self.tempeditlines1.pack(side=tk.LEFT,expand=0,fill=tk.Y)

        self.tempeditlines2 = tk.Frame(self.tempeditlines)

        self.tempeditlines2.label = ttk.Label(self.tempeditlines2,text="Line Details")
        self.tempeditlines2.label.grid(row=0,column=0,columnspan=2,sticky=tk.W,padx=(10,),pady=(4,))

        self.tempeditlines2.label0 = ttk.Label(self.tempeditlines2,text="X-axis")
        self.tempeditlines2.label0.grid(row=1,column=0,sticky=tk.E,padx=(10,),pady=(2,))

        self.tempeditlines2.label1 = ttk.Label(self.tempeditlines2,text="Y-axis")
        self.tempeditlines2.label1.grid(row=2,column=0,sticky=tk.E,padx=(10,),pady=(2,))

        self.tempeditlines2.label2 = ttk.Label(self.tempeditlines2,text="Draw Style")
        self.tempeditlines2.label2.grid(row=3,column=0,sticky=tk.E,padx=(10,),pady=(2,))

        self.tempeditlines2.label3 = ttk.Label(self.tempeditlines2,text="Line Style")
        self.tempeditlines2.label3.grid(row=4,column=0,sticky=tk.E,padx=(10,),pady=(2,))

        self.tempeditlines2.label4 = ttk.Label(self.tempeditlines2,text="Line Color")
        self.tempeditlines2.label4.grid(row=5,column=0,sticky=tk.E,padx=(10,),pady=(2,))

        self.tempeditlines2.val0 = tk.StringVar(self.tempeditlines2)
        self.tempeditlines2.val1 = tk.StringVar(self.tempeditlines2)
        self.tempeditlines2.val2 = tk.StringVar(self.tempeditlines2)
        self.tempeditlines2.val3 = tk.StringVar(self.tempeditlines2)
        self.tempeditlines2.val4 = tk.StringVar(self.tempeditlines2)

        self.tempeditlines2.menu0 = ttk.OptionMenu(
            self.tempeditlines2,self.tempeditlines2.val0,"Select Header",*self.headers)
        self.tempeditlines2.menu0.grid(row=1,column=1,sticky=tk.EW,padx=(10,),pady=(4,))

        self.tempeditlines2.menu1 = ttk.OptionMenu(
            self.tempeditlines2,self.tempeditlines2.val1,"Select Header",*self.headers)
        self.tempeditlines2.menu1.grid(row=2,column=1,sticky=tk.EW,padx=(10,),pady=(4,))

        self.tempeditlines2.menu2 = ttk.OptionMenu(
            self.tempeditlines2,self.tempeditlines2.val2,"Select Style",*self.drawstyles)
        self.tempeditlines2.menu2.grid(row=3,column=1,sticky=tk.EW,padx=(10,),pady=(4,))

        self.tempeditlines2.menu3 = ttk.OptionMenu(
            self.tempeditlines2,self.tempeditlines2.val3,"Select Style",*self.linestyles)
        self.tempeditlines2.menu3.grid(row=4,column=1,sticky=tk.EW,padx=(10,),pady=(4,))

        self.tempeditlines2.menu4 = ttk.OptionMenu(
            self.tempeditlines2,self.tempeditlines2.val4,"Select Color",*self.linecolors)
        self.tempeditlines2.menu4.grid(row=5,column=1,sticky=tk.EW,padx=(10,),pady=(4,))

        self.tempeditlines2.pack(side=tk.LEFT,expand=1,fill=tk.BOTH)

        self.tempedit.add(self.tempeditlines,text="Lines",compound=tk.CENTER)

        self.tempedit.pack(side=tk.TOP,expand=1,fill=tk.BOTH,padx=(0,1))

        buttonname = "Add Template" if tempid is None else "Edit Template"

        self.temptop.button = ttk.Button(self.temptop,text=buttonname,width=20,command=lambda: self.temptopapply(tempid))
        self.temptop.button.pack(side=tk.TOP,expand=1,anchor=tk.E,padx=(0,1),pady=(1,1))

        self.temptop.button.bind('<Return>',lambda event: self.temptopapply(tempid,event))

        self.temptop.mainloop()

    def set_temptopdict(self):
        pass
        # self.tempeditgeneral0.tempname.insert(0,curtemp.get("name"))
        # naxrows,naxcols = self.curtemp.get("subplots")

        # self.tempeditgeneral1.naxrows.insert(0,naxrows)
        # self.tempeditgeneral1.naxcols.insert(0,naxcols)

        # for index in range(naxrows*naxcols):
        #     self.tempeditaxes0.listbox.insert(tk.END,"Axis {}".format(index))
        #     self.tempeditlines0.listbox.insert(tk.END,"Axis {}".format(index))

        # self.tempeditaxes1.entry00.set(self.curtemp.get("twinx")[0])
        # self.tempeditaxes1.entry01.set(self.curtemp.get("title")[0])
        # self.tempeditaxes1.enrty02.set(self.curtemp.get("xlabel")[0])
        # self.tempeditaxes1.enrty03.set(self.curtemp.get("ylabel")[0])
        # self.tempeditaxes1.enrty04.set(self.curtemp.get("ylabel")[1])
        # self.tempeditaxes1.entry05.set(self.curtemp.get("sublines")[0])
        # self.tempeditaxes1.entry06.set(self.curtemp.get("sublines")[1])
        # self.tempeditaxes1.entry07.set(self.curtemp.get("legends")[0])
        # self.tempeditaxes1.entry08.set(self.curtemp.get("xticks")[0])
        # self.tempeditaxes1.entry09.set(self.curtemp.get("yticks")[0])
        # self.tempeditaxes1.entry10.set(self.curtemp.get("yticks")[1])
        # self.tempeditaxes1.entry11.set(self.curtemp.get("grids")[0])

        # for index in range(curtemp.get("sublines")[0][0]):
        #     self.tempeditlines1.listbox1.insert(tk.END,"Line {}".format(index))

        # for index in range(curtemp.get("sublines")[0][1]):
        #     self.tempeditlines1.listbox2.insert(tk.END,"Line {}".format(index))

        # self.tempeditlines2.val0.set(self.headers[self.curtemp.get("xaxes")[0][0]])
        # self.tempeditlines2.val1.set(self.headers[self.curtemp.get("yaxes")[0][0]])
        # self.tempeditlines2.val2.set(self.drawstyles[self.curtemp.get("drawstyles")[0][0]])
        # self.tempeditlines2.val3.set(self.linestyles[self.curtemp.get("linestyles")[0][0]])
        # self.tempeditlines2.val4.set(self.linecolors[self.curtemp.get("linecolors")[0][0]])

    def set_temptopedits(self,input_):

        if input_=="rows" or input_=="columns":
            numx = self.tempeditgeneral1.naxval0.get()
            numy = self.tempeditgeneral1.naxval1.get()
            numx = int(numx) if numx else 1
            numy = int(numy) if numy else 1
            numa = numx*numy
            if numa>len(self.tempeditaxes0.listbox.get(0,tk.END)):
                self.tempeditaxes0.listbox.insert(tk.END,"Axis {}".format(numa))
                self.tempeditlines0.listbox.insert(tk.END,"Axis {}".format(numa))
        elif input_=="entry00":
            if self.tempeditaxes1.entry00.get():
                self.tempeditaxes1.label04.config(state=tk.NORMAL)
                self.tempeditaxes1.entry04.config(state=tk.NORMAL)
                self.tempeditaxes1.label06.config(state=tk.NORMAL)
                self.tempeditaxes1.spinb06.config(state=tk.NORMAL)
                self.tempeditaxes1.check10.config(state=tk.NORMAL)
            else:
                self.tempeditaxes1.label04.config(state=tk.DISABLED)
                self.tempeditaxes1.entry04.config(state=tk.DISABLED)
                self.tempeditaxes1.label06.config(state=tk.DISABLED)
                self.tempeditaxes1.spinb06.config(state=tk.DISABLED)
                self.tempeditaxes1.check10.config(state=tk.DISABLED)
        elif input_=="entry05":
            num1 = int(self.tempeditaxes1.entry05.get())
            if num1>len(self.tempeditlines1.listbox1.get(0,tk.END)):
                self.tempeditlines1.listbox1.insert(tk.END,"Line {}".format(num1))
        elif input_=="entry06":
            num2 = int(self.tempeditaxes1.entry06.get())
            if num2>len(self.tempeditlines1.listbox2.get(0,tk.END)):
                self.tempeditlines1.listbox2.insert(tk.END,"Line {}".format(num2))

        # if x.isdigit() or x=="":
        #     # print(prop)
        #     return True
        # else:
        #     return False

    def set_template(self,tempid=None,event=None):

        if event is not None and event.widget!=self.temptop.button:
            return

        if tempid is not None:
            names = [name for index,name in enumerate(self.curtemp.get("name")) if index!=tempid]
        else:
            names = self.curtemp.get("name")

        name = self.tempeditgeneral0.tempname.get()

        if name in names:
            tk.messagebox.showerror("Error","You have a template with the same name!",parent=self.temptop)
            return
        elif name.strip()=="":
            tk.messagebox.showerror("Error","You have not named the template!",parent=self.temptop)
            return

        if tempid is None:
            tempid = len(self.temps.get("names"))
        else:
            self.tempbox.listbox.delete(tempid)
            self.curtemp.get("name").pop(tempid)
            # self.curtemp.get("naxrows").pop(tempid)
            # self.curtemp.get("naxcols").pop(tempid)
        
        self.tempbox.listbox.insert(tempid,name)

        self.curtemp.get("name").insert(tempid,name)

        try:
            naxrows = int(self.tempeditgeneral1.naxrows.get())
        except ValueError:
            naxrows = 1

        # self.curtemp.get("naxrows").insert(tempid,naxrows)

        try:
            naxcols = int(self.tempeditgeneral1.naxcols.get())
        except ValueError:
            naxcols = 1

        # self.curtemp.get("naxcols").insert(tempid,naxcols)

        self.temptop.destroy()

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

    aze_cyril_lower = [
        "а","б","ж","ч","д",
        "е","я","ф","э","ь",
        "щ","х","ы","и","ъ",
        "к","г","л","м","н",
        "о","ю","п","р","с",
        "ш","т","у","ц","в",
        "й","з"]

    aze_latin_lower = [
        "a","b","c","ç","d",
        "e","ə","f","g","ğ",
        "h","x","ı","i","j",
        "k","q","l","m","n",
        "o","ö","p","r","s",
        "ş","t","u","ü","v",
        "y","z"]

    aze_cyril_upper = [
        "А","Б","Ҹ","Ч","Д",
        "Е","Я","Ф","Ҝ","Ғ",
        "Щ","Х","Ы","И","Ъ",
        "К","Г","Л","М","Н",
        "О","Ю","П","Р","С",
        "Ш","Т","У","Ц","В",
        "Й","З"]

    aze_latin_upper = [
        "A","B","C","Ç","D",
        "E","Ə","F","G","Ğ",
        "H","X","I","İ","J",
        "K","Q","L","M","N",
        "O","Ö","P","R","S",
        "Ş","T","U","Ü","V",
        "Y","Z"]

    for cyril,latin in zip(aze_cyril_lower,aze_latin_lower):
        string.replace(cyril,latin)

    for cyril,latin in zip(aze_cyril_upper,aze_latin_upper):
        string.replace(cyril,latin)

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

    data.set_graphtop()

    window.mainloop()
