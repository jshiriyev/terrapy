import calendar
import datetime

from dateutil.parser import parse
from dateutil.relativedelta import relativedelta

import inspect

import os
import re

import warnings

import matplotlib.pyplot as plt

import numpy as np

if __name__ == "__main__":
    import setup

class dataset():

    special_extensions = [".db",".xlsx"] # should also add .las files

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
            return
        else:
            self.read()

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

        if self.extension == ".xlsx":

            import openpyxl

            wb = openpyxl.load_workbook(self.filepath,read_only=True)

            self._headers = wb[sheetname].iter_rows(min_row=self.headerline+1,min_col=min_col,
                max_row=self.headerline+1,max_col=max_col,values_only=True)

            columns = wb[sheetname].iter_cols(min_row=min_row+self.skiplines,min_col=min_col,
                max_row=max_row,max_col=max_col,values_only=True)

            self._running = [np.array(column) for column in columns]

            wb._archive.close()

        elif self.extension == ".las":

            import lasio

            las = lasio.read(self.filepath)

            self._headers = las.keys()

            self._running = [np.asarray(column) for column in las.data.transpose()]

        self.headers = self._headers
        self.running = [np.asarray(column) for column in self._running]

    def set_subheaders(self,header_index=None,header=None,regex=None,regex_builtin="INC_HEADERS",title="SUB-HEADERS"):

        nparray = np.array(self._running[header_index])

        if regex is None and regex_builtin=="INC_HEADERS":
            regex = r'^[A-Z]+$'                         #for strings with only capital letters no digits
        elif regex is None and regex_builtin=="INC_DATES":
            regex = r'^\d{1,2} [A-Z]{3} \d{2}\d{2}? $'   #for strings with [1 or 2 digits][space][3 capital letters][space][2 or 4 digits], e.g. DATES

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
        # vmatch = np.vectorize(lambda x:bool(re.compile('[Ab]').match(x)))
        
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
                return datetime.datetime(date.year,date.month,calendar.monthrange(date.year,date.month)[1])

            if shiftmonths != 0:
                vdate = np.vectorize(lambda x: shifting(x))
            else:
                vdate = np.vectorize(lambda x: parse(x))
            
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
            self.headers = self._headers
            self._running.append(column)
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
            self.headers = self._headers
            self._running = [self._running[index] for index in header_indices]
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

class schedule():

    def __init__(self,fprod,fcomp):

        self.fprod = fprod
        self.fcomp = fcomp

        self.prods = dataset(fprod,skiplines=1)
        self.comps = dataset(fcomp,skiplines=1)

        self.prods.texttocolumn(0,"\t",maxsplit=7)
        self.comps.texttocolumn(0,"\t",maxsplit=6)

        self.prodwellnames = set(self.prods.running[0])

        self.prods.astype(1,dtype=np.datetime64,datestring=True,shiftmonths=-1)
        self.prods.astype(2,dtype=np.int64)
        self.prods.astype(3,dtype=np.float64)
        self.prods.astype(4,dtype=np.float64)
        self.prods.astype(5,dtype=np.float64)
        self.prods.astype(6,dtype=np.float64)

        self.compwellnames = set(self.comps.running[0])

        self.comps.astype(1,dtype=np.datetime64,datestring=True)
        self.comps.astype(3,dtype=np.float64)
        self.comps.astype(4,dtype=np.float64)

    def getwell(self,wellname):

        self.prods.filter(0,keywords=[wellname],inplace=False)
        self.comps.filter(0,keywords=[wellname],inplace=False)

        class well: pass

        well.name = wellname

        well.proddates = self.prods.running[1]
        well.proddays = self.prods.running[2]
        well.prodoil = self.prods.running[3]
        well.prodwater = self.prods.running[4]
        well.prodgas = self.prods.running[5]
        well.produced = well.prodoil+well.prodwater+well.prodgas

        well.injwater = self.prods.running[6]
        well.injected = well.injwater

        well.shutdates = []

        well.compdates = self.comps.running[1]
        well.compevents = self.comps.running[2]

        well.compuppers = self.comps.running[3]
        well.complowers = self.comps.running[4]

        well.compopenintervals = []

        well.compopencounts = []

        for compevent,compupper,complower in zip(well.compevents,well.compuppers,well.complowers):

            if compevent=="PERF":
                well.compopenintervals.append([compupper,complower])
            elif compevent=="PLUG":
                well.compopenintervals.remove([compupper,complower])
                   
            well.compopencounts.append(len(well.compopenintervals))

        return well

    def testcross(self):

        for wellname in self.prodwellnames.difference(self.compwellnames):
            print("{:13s} has production but no completion data".format(wellname))

        for wellname in self.compwellnames.difference(self.prodwellnames):
            print("{:13s} has completion but no production data".format(wellname))

        for wellname in self.prodwellnames:

            well = self.getwel(wellname)

            if np.sum(well.compdates<well.proddates)==0:
                warnings.warn("{:13s} production has been defined before completion".format(well.name))

    def testproductions(self):

        for wellname in self.prodwellnames:

            well = self.getwel(wellname)

            if any(well.produced==0) and any(well.injected==0):
                warnings.warn("{:13s} zero production and injection has been observed".format(well.name))

    def testcompletions(self):
        # completion data should be sorted, first perforation of the interval and then plug
        # there must be more than two completion scenarios (perf and plug)
        # completion should also be sorted based on date
        pass

    def testwells(self):

        for wellname in self.prodwellnames:

            well = self.getwell(wellname)

            print("{:13s} is on progress...".format(well.name))

            flagNoPrevProd = True

            for index,(proddate,prodday) in enumerate(zip(proddates,proddays)):

                prodSTART = proddate+relativedelta(days=1)

                monthDays = calendar.monthrange(prodSTART.year,prodSTART.month)[1]

                prodEND = datetime.datetime(prodSTART.year,prodSTART.month,monthDays)

                if np.sum(compdates<prodSTART)==0:
                    compIndexSTART = 0
                else:
                    compIndexSTART = np.sum(compdates<prodSTART)-1

                compIndexEND = np.sum(compdates<=prodEND)

                compOPEN = well.compopencounts[compIndexSTART:compIndexEND]

                compEVENTS = well.compevents[compIndexSTART:compIndexEND]

                compDATES = well.compdates[compIndexSTART:compIndexEND]

                perfDATES = compDATES[compEVENTS=="PERF"]
                plugDATES = compDATES[compEVENTS=="PLUG"]

                try:
                    flagNoPostProd = True if well.proddates[index+1]-relativedelta(months=1)>prodEND else False
                except IndexError:
                    flagNoPostProd = True

                print("{:%d %b %Y} - {:%d %b %Y} {:13s}".format(prodSTART,prodEND,wellname))

                if np.sum(compdates<prodSTART)==0:
                    flagCompShutSTART = True
                else:
                    flagCompShutSTART = compOPEN[0]==0

                flagCompShutEND = compOPEN[-1]==0

                flagPlugPerf = any([openperforation==0 for openperforation in compOPEN[1:-1]])

                
                if flagCompShutSTART and flagCompShutEND:
                    proddates[index] = perfDATES[0]
                    prodopendays[index] = plugDATES[-1].day-perfDATES[0].day
                    shutdates.append(plugDATES[-1])
                    flagNoPrevProd = True
                    print(" - Peforated and Plugged")
                    print(" - Production is shut on {:%Y-%m-%d}.".format(shutdates[-1]))
                elif flagCompShutSTART and flagNoPostProd:
                    proddates[index] = perfDATES[0]
                    prodopendays[index] = prodEND.day-perfDATES[0].day
                    shutdates.append(prodEND)
                    flagNoPrevProd = True
                    print(" - Perforated and Open, no post production")
                    print(" - Production is shut on {:%Y-%m-%d}.".format(shutdates[-1]))
                elif flagCompShutSTART:
                    proddates[index] = perfDATES[0]
                    prodopendays[index] = prodEND.day-perfDATES[0].day
                    flagNoPrevProd = False
                    print(" - Perforated and Open")
                elif flagCompShutEND and plugDATES[-1].day>=prodday:
                    for plugDATE in plugDATES:
                        if plugDATE.day>=prodday: break
                    prodopendays[index] = plugDATE.day
                    shutdates.append(plugDATE)
                    flagNoPrevProd = True
                    print(" - Open and Plugged")
                    print(" - Production is shut on {:%Y-%m-%d}.".format(shutdates[-1]))
                elif flagPlugPerf and not flagNoPrevProd and plugDATES[-1].day>=prodday:
                    for plugDATE in plugDATES:
                        if plugDATE.day>=prodday: break
                    prodopendays[index] = plugDATE.day
                    shutdates.append(plugDATE)
                    flagNoPrevProd = True
                    print(" - Plugged and Perforated, no post production")
                    print(" - Production is shut on {:%Y-%m-%d}.".format(shutdates[-1]))
                elif flagPlugPerf and flagNoPrevProd and not flagNoPostProd and prodEND.day-perfDATES[1].day>=prodday:
                    for perfDATE in np.flip(perfDATES[1:]):
                        if prodEND.day-perfDATE.day>=prodday: break
                    proddates[index] = perfDATE
                    prodopendays[index] = prodEND.day-perfDATE.day
                    flagNoPrevProd = False
                    print(" - Plugged and Perforated, no previous production")
                elif flagPlugPerf and flagNoPrevProd and flagNoPostProd and prodEND.day-perfDATES[1].day>=prodday:
                    for perfDATE in np.flip(perfDATES[1:]):
                        if prodEND.day-perfDATE.day>=prodday: break
                    proddates[index] = perfDATE
                    prodopendays[index] = prodEND.day-perfDATE.day
                    shutdates.append(prodEND)
                    flagNoPrevProd = True
                    print(" - Plugged and Perforated, no previous production,")
                    print(" - Production is shut on {:%Y-%m-%d}.".format(shutdates[-1]))
                elif not flagNoPostProd:
                    prodopendays[index] = monthDays
                    flagNoPrevProd = False
                    print(" - No completion event")
                else:
                    prodopendays[index] = monthDays
                    shutdates.append(prodEND)
                    flagNoPrevProd = True
                    print(" - No completion event, no post production")
                    print(" - Production is shut on {:%Y-%m-%d}.".format(shutdates[-1]))
                
                print(" - Production efficiency is [{:2d} out of {:2d} days].".format(prodday,prodopendays[index]))

                if prodday/prodopendays[index]>1:
                    warnings.warn("{:%Y-%m-%d}: {:13s} efficiency is more than unit [{:2d} out of {:2d} days].".format(proddate,wellname,prodday,prodopendays[index]))

            wefacs = proddays/prodopendays

            shutdates = np.array(shutdates,dtype=datetime.datetime)

            proddates = np.append(proddates,shutdates)
            prodoil = np.append(prodoil,np.zeros(shutdates.shape))

            sortindex = np.argsort(proddates)

            proddates = proddates[sortindex]
            prodoil = prodoil[sortindex]

    def plotwell(self,wellname,fignum):

        well = self.getwell(wellname)

        fig = plt.figure(num=fignum,tight_layout=True)

        ax0 = fig.add_subplot()
        ax1 = ax0.twinx()

        ax0.scatter(well.proddates,well.prodoil)

        ax0.step(well.proddates,well.prodoil,'b',where='post')
        ax1.step(well.compdates,well.compopencounts,'r--',where='post')

        ax0.set_ylabel('Oil Production [m3/day]')
        ax1.set_ylabel('Open Perforation Intervals',rotation=270)

        ax0.yaxis.set_label_coords(-0.1,0.5)
        ax1.yaxis.set_label_coords(1.10,0.5)

        ax0.set_ylim(ymin=0,ymax=max(well.prodoil)*1.1)
        ax1.set_ylim(ymin=0,ymax=max(well.compopencounts)+0.5)

        ax1.set_yticks(range(0,max(well.compopencounts)+1))

        for tick in ax0.get_xticklabels():
            tick.set_rotation(45)

    def write(self,filepath):

        pass

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

    pass
