import calendar

from datetime import datetime
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
                return datetime(date.year,date.month,calendar.monthrange(date.year,date.month)[1])

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

class schedule(dataset):

    def __init__(self,fprod,fcomp):

        super().__init__(None)

        # KEYWORDS = [DATES,COMPDATMD,COMPORD,WCONHIST,WCONINJH,WEFAC,WELOPEN]

        self.strdates       = " {} / "#.format(date)

        self.strwspec       = " '{}'\t1*\t2* / "
        self.strcompdat     = " '{}'\t1*\t{}\t{}\tMD\t{}\t2*\t0.14 / "#.format(wellname,top,bottom,status)
        self.strcompord     = " '{}'\tINPUT\t/ "#.format(wellname)
        self.strprodhist    = " '{}'\tOPEN\tORAT\t{}\t{}\t{} / "#.format(wellname,oilrate,waterrate,gasrate)
        self.strinjhist     = " '{}'\tWATER\tOPEN\t{}\t7*\tRATE / "#.format(wellname,waterrate)
        self.strwefac       = " '{}'\t{} / "#.format(wellname,efficiency)
        self.strwopen       = " '{}'\tSHUT\t3* / "#.format(wellname)

        # MAIN SCHEDULE DATA

        self._headers = ["DATE","KEYWORD","DETAILS"]
        self._running = [np.array([]) for _ in self._headers]

        self.headers = self._headers
        self.running = [np.asarray(column) for column in self._running]

        # INPUT, production and completion data

        self.fprod = fprod
        self.fcomp = fcomp

        self.prods = dataset(fprod,skiplines=1)
        self.comps = dataset(fcomp,skiplines=1)

        self.prods.texttocolumn(0,"\t",maxsplit=7)
        self.comps.texttocolumn(0,"\t",maxsplit=6)

        self.prods.get_columns(headers=["WELL","DATE","DAYS","OPROD","WPROD","GPROD","WINJ"],inplace=True)
        self.comps.get_columns(headers=["WELL","DATE","EVENT","TOP","BOTTOM"],inplace=True)

        self.prods.sort(header_indices=[1],inplace=True)
        self.comps.sort(header_indices=[1],inplace=True)

        self.prodwellnames = np.unique(self.prods.running[0])

        self.prods.astype(1,dtype=np.datetime64,datestring=True,shiftmonths=-1)
        self.prods.astype(2,dtype=np.int64)
        self.prods.astype(3,dtype=np.float64)
        self.prods.astype(4,dtype=np.float64)
        self.prods.astype(5,dtype=np.float64)
        self.prods.astype(6,dtype=np.float64)

        self.compwellnames = np.unique(self.comps.running[0])

        self.comps.astype(1,dtype=np.datetime64,datestring=True)
        self.comps.astype(3,dtype=np.float64)
        self.comps.astype(4,dtype=np.float64)

    def getwell(self,wellname):

        self.prods.filter(0,keywords=[wellname],inplace=False)
        self.comps.filter(0,keywords=[wellname],inplace=False)

        class well: pass

        well.name = wellname

        well.opdates = self.prods.running[1]

        well.opdays = self.prods.running[2]

        well.prodtotal = self.prods.running[3]+self.prods.running[4]+self.prods.running[5]
        well.injtotal = self.prods.running[6]

        well.total = well.prodtotal+well.injtotal

        well.opstatus = np.empty(well.opdays.shape,dtype=object)

        well.oil = np.zeros(well.opdays.shape,dtype=np.float32)
        well.water = np.zeros(well.opdays.shape,dtype=np.float32)
        well.gas = np.zeros(well.opdays.shape,dtype=np.float32)

        well.opstatus[well.prodtotal>0] = "production"

        well.oil[well.prodtotal>0] = self.prods.running[3][well.prodtotal>0]
        well.water[well.prodtotal>0] = self.prods.running[4][well.prodtotal>0]
        well.gas[well.prodtotal>0] = self.prods.running[5][well.prodtotal>0]
        
        well.opstatus[well.injtotal>0] = "injection"

        well.water[well.injtotal>0] = self.prods.running[6][well.injtotal>0]

        well.compdates = self.comps.running[1]

        well.compevents = self.comps.running[2]
        well.compuppers = self.comps.running[3]
        well.complowers = self.comps.running[4]

        well.compupdatedates = np.unique(well.compdates)

        compcounts = np.insert(np.cumsum(np.sum(well.compdates==well.compupdatedates.reshape((-1,1)),axis=1)),0,0)

        well.compupdatecounts = np.empty(well.compupdatedates.shape,dtype=int)

        compopenintervals = np.empty((0,2))

        for index,date in enumerate(well.compupdatedates):

            compevents = well.compevents[compcounts[index]:compcounts[index+1]]
            compuppers = well.compuppers[compcounts[index]:compcounts[index+1]]
            complowers = well.complowers[compcounts[index]:compcounts[index+1]]

            perfevents = compevents=="PERF"

            perfintervals = np.array([compuppers[perfevents],complowers[perfevents]]).T

            compopenintervals = np.concatenate((compopenintervals,perfintervals),axis=0)

            plugevents = compevents=="PLUG"

            pluguppermatch = np.any(compopenintervals[:,0]==compuppers[plugevents].reshape((-1,1)),axis=0)
            pluglowermatch = np.any(compopenintervals[:,1]==complowers[plugevents].reshape((-1,1)),axis=0)

            plugmatch = np.where(np.logical_and(pluguppermatch,pluglowermatch))[0]

            compopenintervals = np.delete(compopenintervals,plugmatch,0)

            well.compupdatecounts[index] = compopenintervals.shape[0]

        return well

    def wellcompletioncheck(self,well):

        compdata = zip(well.compdates,well.compevents,well.compuppers,well.complowers)

        for index,(compdate,compevent,compupper,complower) in enumerate(compdata):

            if index==0:
                self.set_rows([compdate,"WELSPECS",self.strwspec.format(well.name)])

            if compevent == "PERF":
                bottom = complower
                perfs = "OPEN"
            elif compevent == "PLUG":
                bottom = "1*"
                perfs = "SHUT"

            self.set_rows([compdate,"COMPDATMD",self.strcompdat.format(well.name,compupper,bottom,perfs)])
            self.set_rows([compdate,"COMPORD",self.strcompord.format(well.name)])

    def wellcrosscheck(self,well,flagReturn=False,flagShow=False):

        warnWPLUGPERF = "{:%Y-%m-%d}: {} first perf and last plug dates do not fit production days."
        warnWPERF = "{:%Y-%m-%d}: {} first perf date does not fit production days."
        warnWPLUG = "{:%Y-%m-%d}: {} last plug date does not fit production days."
        warnWEFAC = "{:%Y-%m-%d}: {} efficiency is more than unit [{:2d} out of {:2d} days]."

        flagNoPrevProd = True

        print("{} check is in progress ...".format(well.name))

        opdata = zip(well.opdates,well.opdays,well.opstatus,well.oil,well.water,well.gas)

        shutdates = []

        for index,(date,day,status,oil,water,gas) in enumerate(opdata):

            prodmonthSTARTday = date+relativedelta(days=1)

            prodmonthdaycount = calendar.monthrange(prodmonthSTARTday.year,prodmonthSTARTday.month)[1]

            prodmonthENDday = datetime(prodmonthSTARTday.year,prodmonthSTARTday.month,prodmonthdaycount)

            if np.sum(well.compupdatedates<prodmonthSTARTday)==0:
                compSTARTindex = 0
            else:
                compSTARTindex = np.sum(well.compupdatedates<prodmonthSTARTday)-1

            compENDindex = np.sum(well.compupdatedates<=prodmonthENDday)

            compupdatecounts = well.compupdatecounts[compSTARTindex:compENDindex]

            compupdatedates = well.compupdatedates[compSTARTindex:compENDindex]

            perfdates = compupdatedates[compupdatecounts!=0]
            plugdates = compupdatedates[compupdatecounts==0]

            try:
                flagNoPostProd = True if well.opdates[index+1]-relativedelta(months=1)>prodmonthENDday else False
            except IndexError:
                flagNoPostProd = True

            if np.sum(well.compupdatedates<prodmonthSTARTday)==0:
                flagCompShutSTART = True
            else:
                flagCompShutSTART = compupdatecounts[0]==0

            flagCompShutEND = compupdatecounts[-1]==0

            flagPlugPerf = any([compopencount==0 for compopencount in compupdatecounts[1:-1]])

            if flagCompShutSTART and flagCompShutEND:
                compday = plugdates[-1].day-perfdates[0].day
                prodeff = day/compday
                if status == "production":
                    self.set_rows([perfdates[0],"WCONHIST",self.strprodhist.format(well.name,oil,water,gas)])
                elif status == "injection":
                    self.set_rows([perfdates[0],"WCONINJH",self.strinjhist.format(well.name,water)])
                self.set_rows([perfdates[0],"WEFAC",self.strwefac.format(well.name,prodeff)])
                well.opdates[index] = perfdates[0]
                self.set_rows([plugdates[-1],"WELOPEN",self.strwopen.format(well.name)])
                shutdates.append(plugdates[-1])
                flagNoPrevProd = True
                if flagShow:
                    print("{:%d %b %Y} Peforated and Plugged: OPEN ({:%d %b %Y}) and SHUT ({:%d %b %Y}) WEFAC ({:.3f})".format(prodmonthENDday,perfdates[0],plugdates[-1],prodeff))

            elif flagCompShutSTART:
                compday = prodmonthENDday.day-perfdates[0].day
                prodeff = day/compday
                if status == "production":
                    self.set_rows([perfdates[0],"WCONHIST",self.strprodhist.format(well.name,oil,water,gas)])
                elif status == "injection":
                    self.set_rows([perfdates[0],"WCONINJH",self.strinjhist.format(well.name,water)])
                self.set_rows([perfdates[0],"WEFAC",self.strwefac.format(well.name,prodeff)])
                well.opdates[index] = perfdates[0]
                if flagNoPostProd:
                    self.set_rows([prodmonthENDday,"WELOPEN",self.strwopen.format(well.name)])
                    shutdates.append(prodmonthENDday)
                    flagNoPrevProd = True
                    if flagShow:
                        print("{:%d %b %Y} Peforated and Open: OPEN ({:%d %b %Y}) and SHUT ({:%d %b %Y}) WEFAC ({:.3f})".format(prodmonthENDday,perfdates[0],prodmonthENDday,prodeff))
                else:                  
                    flagNoPrevProd = False
                    if flagShow:
                        print("{:%d %b %Y} Peforated and Open: OPEN ({:%d %b %Y}) and CONT WEFAC ({:.3f})".format(prodmonthENDday,perfdates[0],prodeff))

            elif flagCompShutEND:
                for plugdate in plugdates:
                    if plugdate.day>=day: break
                if not plugdate.day>=day:
                    warnings.warn(warnWPLUG.format(prodmonthENDday,well.name))
                compday = plugdate.day
                prodeff = day/compday
                if status == "production":
                    self.set_rows([date,"WCONHIST",self.strprodhist.format(well.name,oil,water,gas)])
                elif status == "injection":
                    self.set_rows([date,"WCONINJH",self.strinjhist.format(well.name,water)])
                self.set_rows([date,"WEFAC",self.strwefac.format(well.name,prodeff)])
                self.set_rows([plugdate,"WELOPEN",self.strwopen.format(well.name)])
                shutdates.append(plugdate)
                flagNoPrevProd = True
                if flagShow:
                    print("{:%d %b %Y} Open and Plugged: CONT and SHUT ({:%d %b %Y}) WEFAC ({:.3f})".format(prodmonthENDday,plugdate,prodeff))

            elif flagPlugPerf:
                if flagNoPrevProd and flagNoPostProd:
                    # shift the start day to the first perf day
                    # shut the well at the last plug day
                    if not plugdates[-1].day-perfdates[1].day>=day:
                        warnings.warn(warnWPLUGPERF.format(prodmonthENDday,well.name))
                    compday = plugdates[-1].day-perfdates[1].day
                    prodeff = day/compday
                    if status == "production":
                        self.set_rows([perfdates[1],"WCONHIST",self.strprodhist.format(well.name,oil,water,gas)])
                    elif status == "injection":
                        self.set_rows([perfdates[1],"WCONINJH",self.strinjhist.format(well.name,water)])
                    self.set_rows([perfdates[1],"WEFAC",self.strwefac.format(well.name,prodeff)])
                    well.opdates[index] = perfdates[1]
                    self.set_rows([plugdates[-1],"WELOPEN",self.strwopen.format(well.name)])
                    shutdates.append(plugdates[-1])
                    flagNoPrevProd = True
                    if flagShow:
                        print("{:%d %b %Y} Plugged and Perforated: OPEN ({:%d %b %Y}) and SHUT ({:%d %b %Y}) WEFAC ({:.3f})".format(prodmonthENDday,perfdates[1],plugdates[-1],prodeff))
                elif flagNoPrevProd and not flagNoPostProd:
                    # shift the start day to the proper perf day
                    for perfdate in np.flip(perfdates[1:]):
                        if prodmonthENDday.day-perfdate.day>=day: break
                    if not prodmonthENDday.day-perfdate.day>=day:
                        warnings.warn(warnWPERF.format(prodmonthENDday,well.name))
                    compday = prodmonthENDday.day-perfdate.day
                    prodeff = day/compday
                    if status == "production":
                        self.set_rows([perfdate,"WCONHIST",self.strprodhist.format(well.name,oil,water,gas)])
                    elif status == "injection":
                        self.set_rows([perfdate,"WCONINJH",self.strinjhist.format(well.name,water)])
                    self.set_rows([perfdate,"WEFAC",self.strwefac.format(well.name,prodeff)])
                    well.opdates[index] = perfdate
                    flagNoPrevProd = False
                    if flagShow:
                        print("{:%d %b %Y} Plugged and Perforated: OPEN ({:%d %b %Y}) and CONT WEFAC ({:.3f})".format(prodmonthENDday,perfdate,prodeff))
                elif not flagNoPrevProd and flagNoPostProd:
                    # try shut the well at the proper plug day if not successful shut it at the end of month
                    for plugdate in plugdates:
                        if plugdate.day>=day: break
                    if not plugdate.day>=day:
                        plugdate = prodmonthENDday
                    compday = plugdate.day
                    prodeff = day/compday
                    if status == "production":
                        self.set_rows([date,"WCONHIST",self.strprodhist.format(well.name,oil,water,gas)])
                    elif status == "injection":
                        self.set_rows([date,"WCONINJH",self.strinjhist.format(well.name,water)])
                    self.set_rows([date,"WEFAC",self.strwefac.format(well.name,prodeff)])
                    self.set_rows([plugdate,"WELOPEN",self.strwopen.format(well.name)])
                    shutdates.append(plugdate)
                    flagNoPrevProd = True
                    if flagShow:
                        print("{:%d %b %Y} Plugged and Perforated: CONT and SHUT ({:%d %b %Y}) WEFAC ({:.3f})".format(prodmonthENDday,plugdate,prodeff))
                elif not flagNoPrevProd and not flagNoPostProd:
                    # try shut the well if not successful do nothing
                    for plugdate in plugdates:
                        if plugdate.day>=day: break
                    if not plugdate.day>=day:
                        compday = prodmonthdaycount
                        prodeff = day/compday
                        flagNoPrevProd = False
                        if flagShow:
                            print("{:%d %b %Y} Plugged and Perforated: CONT and CONT WEFAC ({:.3f})".format(prodmonthENDday,prodeff))
                    else:
                        compday = plugdate.day
                        prodeff = day/compday
                        self.set_rows([plugdate,"WELOPEN",self.strwopen.format(well.name)])
                        shutdates.append(plugdate)
                        flagNoPrevProd = True
                        if flagShow:
                            print("{:%d %b %Y} Plugged and Perforated: CONT and SHUT ({:%d %b %Y}) WEFAC ({:.3f})".format(prodmonthENDday,plugdate,prodeff))
                    if status == "production":
                        self.set_rows([date,"WCONHIST",self.strprodhist.format(well.name,oil,water,gas)])
                    elif status == "injection":
                        self.set_rows([date,"WCONINJH",self.strinjhist.format(well.name,water)])
                    self.set_rows([date,"WEFAC",self.strwefac.format(well.name,prodeff)])

            else:
                compday = prodmonthdaycount
                prodeff = day/compday
                if status == "production":
                    self.set_rows([date,"WCONHIST",self.strprodhist.format(well.name,oil,water,gas)])
                elif status == "injection":
                    self.set_rows([date,"WCONINJH",self.strinjhist.format(well.name,water)])
                self.set_rows([date,"WEFAC",self.strwefac.format(well.name,prodeff)])
                if flagNoPostProd:
                    self.set_rows([prodmonthENDday,"WELOPEN",self.strwopen.format(well.name)])
                    shutdates.append(prodmonthENDday)
                    flagNoPrevProd = True
                    if flagShow:
                        print("{:%d %b %Y} No completion events: CONT and SHUT ({:%d %b %Y}) WEFAC ({:.3f})".format(prodmonthENDday,prodmonthENDday,prodeff))
                else:
                    flagNoPrevProd = False
                    if flagShow:
                        print("{:%d %b %Y} No completion events: CONT and CONT WEFAC ({:.3f})".format(prodmonthENDday,prodeff))

            if prodeff>1:
                warnings.warn(warnWEFAC.format(prodmonthENDday,well.name,day,compday))

            well.shutdates = np.array(shutdates)

        if flagShow:
            print("{} check is complete.".format(well.name))

        if flagReturn:

            well.opdates = np.append(well.opdates,well.shutdates)
            well.total = np.append(well.total,np.zeros(well.shutdates.shape))
            
            sortindex = np.argsort(well.opdates)

            well.opdates = well.opdates[sortindex]
            well.total = well.total[sortindex]

            return well

    def wellcrossplot(self,wellname,fignum=1):

        well = self.getwell(wellname)

        fig = plt.figure(num=fignum,figsize=(16,6),tight_layout=True)

        ax1 = fig.add_subplot(1,2,1)
        ax2 = ax1.twinx()

        ax1.scatter(well.opdates,well.total)

        ax1.step(well.opdates,well.total,'b',where='post')
        ax2.step(well.compupdatedates,well.compupdatecounts,'r--',where='post')

        ax1.set_ylabel('Total Production or Injection [m3/day]')
        ax2.set_yticks([])

        ax1.set_title("BEFORE CORRECTIONS")

        ax1.set_ylim(ymin=0,ymax=max(well.total)*1.1)
        ax2.set_ylim(ymin=0,ymax=max(well.compupdatecounts)+0.5)

        for tick in ax1.get_xticklabels():
            tick.set_rotation(45)

        well = self.wellcrosscheck(well,flagReturn=True,flagShow=True)

        ax3 = fig.add_subplot(1,2,2)
        ax4 = ax3.twinx()

        ax3.scatter(well.opdates,well.total)

        ax3.step(well.opdates,well.total,'b',where='post')
        ax4.step(well.compupdatedates,well.compupdatecounts,'r--',where='post')

        ax3.set_yticks([])
        ax4.set_ylabel('Open Perforation Intervals',rotation=270)

        ax3.set_title("AFTER CORRECTIONS")

        ax4.yaxis.set_label_coords(1.05,0.5)

        ax3.set_ylim(ymin=0,ymax=max(well.total)*1.1)
        ax4.set_ylim(ymin=0,ymax=max(well.compupdatecounts)+0.5)

        ax4.set_yticks(range(0,max(well.compupdatecounts)+1))

        for tick in ax3.get_xticklabels():
            tick.set_rotation(45)

        plt.show()

    def productioncheck(self):

        # warnings.filterwarnings("ignore",'.*',UserWarning)

        if any(self.prods.running[3]<0):
            for index in np.where(self.self.prods.running[3]<0)[0]:
                well = self.prods.running[0][index]
                date = self.prods.running[1][index]
                warnings.warn("{:%d %b %Y} {} oil production has negative entry.".format(date,well))

        if any(self.prods.running[4]<0):
            for index in np.where(self.self.prods.running[4]<0)[0]:
                well = self.prods.running[0][index]
                date = self.prods.running[1][index]
                warnings.warn("{:%d %b %Y} {} water production has negative entry.".format(date,well))

        if any(self.prods.running[5]<0):
            for index in np.where(self.self.prods.running[5]<0)[0]:
                well = self.prods.running[0][index]
                date = self.prods.running[1][index]
                warnings.warn("{:%d %b %Y} {} gas production has negative entry.".format(date,well))

        if any(self.prods.running[6]<0):
            for index in np.where(self.self.prods.running[6]<0)[0]:
                well = self.prods.running[0][index]
                date = self.prods.running[1][index]
                warnings.warn("{:%d %b %Y} {} water injection has negative entry.".format(date,well))

        producedtotal = self.prods.running[3]+self.prods.running[4]+self.prods.running[5]
        injectedtotal = self.prods.running[6]

        producedmonthcounts = np.sum(producedtotal!=0)
        injectedmonthcounts = np.sum(injectedtotal!=0)

        totalmonthcounts = self.prods.running[2].size

        if any(producedtotal+injectedtotal==0):
            for index in np.where(producedtotal+injectedtotal==0)[0]:
                well = self.prods.running[0][index]
                date = self.prods.running[1][index]
                warnings.warn("{:%d %b %Y} {} has zero production and injection.".format(date,well))

        if producedmonthcounts+injectedmonthcounts>totalmonthcounts:
            for index in np.where(np.logical_and(producedtotal!=0,injectedtotal!=0))[0]:
                well = self.prods.running[0][index]
                date = self.prods.running[1][index]
                warnings.warn("{:%d %b %Y} {} has both production and injection data.".format(date,well))

        # production dates must be the last day of month

    def completioncheck(self):
        # completion data should be sorted, first perforation of the interval and then plug
        # for the future there can be more than two completion scenarios (perf and plug)
        # completion top must be smaller than bottom
        # they must be positive values

        for wellname in self.prodwellnames:

            well = self.getwell(wellname)

            self.wellcompletioncheck(well)

    def crosscheck(self):

        warnNOPROD = "{} has completion but no production data."
        warnNOCOMP = "{} has production but no completion data."

        warnCROSS = "{} production has been defined before completion."

        for wellname in np.setdiff1d(self.prodwellnames,self.compwellnames):
            warnings.warn(warnNOCOMP.format(wellname))

        for wellname in np.setdiff1d(self.compwellnames,self.prodwellnames):
            warnings.warn(warnNOPROD.format(wellname))

        for wellname in self.prodwellnames:

            well = self.getwell(wellname)

            try:
                datemin = well.opdates.min()
            except ValueError:
                datemin = datetime(3000,1,1)

            date = datemin+relativedelta(months=1)

            days = calendar.monthrange(date.year,date.month)[1]

            date = datetime(date.year,date.month,days)

            if well.compupdatedates.min()>=date:
                warnings.warn(warnCROSS.format(well.name))

        for wellname in self.prodwellnames:

            well = self.getwell(wellname)

            self.wellcrosscheck(well,flagReturn=False,flagShow=False)

    def write(self,filepath):

        with open(filepath,"w",encoding='utf-8') as wfile:

            welspec = self.running[1]=="WELSPECS"
            compdat = self.running[1]=="COMPDATMD"
            compord = self.running[1]=="COMPORD"
            prodhst = self.running[1]=="WCONHIST"
            injdhst = self.running[1]=="WCONINJH"
            wefffac = self.running[1]=="WEFAC"
            welopen = self.running[1]=="WELOPEN"

            for date in np.unique(self.running[0]):

                currentdate = self.running[0]==date

                currentcont = self.running[1][currentdate]

                wfile.write("\n\n")
                wfile.write("DATES\n")
                wfile.write(self.strdates.format(date.strftime("%d %b %Y").upper()))
                wfile.write("\n")
                wfile.write("/\n\n")

                if any(currentcont=="WELSPECS"):
                    indices = np.logical_and(currentdate,welspec)
                    wfile.write("WELSPECS\n")
                    for detail in self.running[2][indices]:
                        wfile.write(detail)
                        wfile.write("\n")
                    wfile.write("/\n\n")

                if any(currentcont=="COMPDATMD"):
                    indices = np.logical_and(currentdate,compdat)
                    wfile.write("COMPDATMD\n")
                    for detail in self.running[2][indices]:
                        wfile.write(detail)
                        wfile.write("\n")
                    wfile.write("/\n\n")

                if any(currentcont=="COMPORD"):
                    indices = np.logical_and(currentdate,compord)
                    wfile.write("COMPORD\n")
                    for detail in self.running[2][indices]:
                        wfile.write(detail)
                        wfile.write("\n")
                    wfile.write("/\n\n")

                if any(currentcont=="WCONHIST"):
                    indices = np.logical_and(currentdate,prodhst)
                    wfile.write("WCONHIST\n")
                    for detail in self.running[2][indices]:
                        wfile.write(detail)
                        wfile.write("\n")
                    wfile.write("/\n\n")

                if any(currentcont=="WCONINJH"):
                    indices = np.logical_and(currentdate,injdhst)
                    wfile.write("WCONINJH\n")
                    for detail in self.running[2][indices]:
                        wfile.write(detail)
                        wfile.write("\n")
                    wfile.write("/\n\n")

                if any(currentcont=="WEFAC"):
                    indices = np.logical_and(currentdate,wefffac)
                    wfile.write("WEFAC\n")
                    for detail in self.running[2][indices]:
                        wfile.write(detail)
                        wfile.write("\n")
                    wfile.write("/\n\n")

                if any(currentcont=="WELOPEN"):
                    indices = np.logical_and(currentdate,welopen)
                    wfile.write("WELOPEN\n")
                    for detail in self.running[2][indices]:
                        wfile.write(detail)
                        wfile.write("\n")
                    wfile.write("/\n\n")

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
